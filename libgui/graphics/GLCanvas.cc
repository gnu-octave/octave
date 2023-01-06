////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2011-2023 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "GLCanvas.h"
#include "gl-select.h"

#include "gl-render.h"
#include "gl2ps-print.h"
#include "graphics.h"
#include "interpreter.h"

OCTAVE_BEGIN_NAMESPACE(octave)

#if defined (HAVE_QOPENGLWIDGET)
#  define OCTAVE_QT_OPENGL_WIDGET_FORMAT_ARGS
#else
#  if defined (Q_OS_WIN32)
#    define OCTAVE_QT_OPENGL_WIDGET_FORMAT_ARGS         \
  QGLFormat (QGL::SampleBuffers | QGL::AlphaChannel     \
             | QGL::IndirectRendering),
#  else
#    define OCTAVE_QT_OPENGL_WIDGET_FORMAT_ARGS         \
  QGLFormat (QGL::SampleBuffers | QGL::AlphaChannel),
#  endif
#endif

GLCanvas::GLCanvas (octave::base_qobject& oct_qobj,
                    octave::interpreter& interp,
                    const graphics_handle& gh, QWidget *xparent)
: OCTAVE_QT_OPENGL_WIDGET (OCTAVE_QT_OPENGL_WIDGET_FORMAT_ARGS xparent),
  Canvas (oct_qobj, interp, gh), m_glfcns (), m_renderer (m_glfcns)
{
  setFocusPolicy (Qt::ClickFocus);
  setFocus ();
}

GLCanvas::~GLCanvas (void)
{ }

void
GLCanvas::initializeGL (void)
{
  m_glfcns.init ();
}

void
GLCanvas::draw (const graphics_handle& gh)
{
  gh_manager& gh_mgr = m_interpreter.get_gh_manager ();

  octave::autolock guard  (gh_mgr.graphics_lock ());

  graphics_object go = gh_mgr.get_object (gh);

  if (go)
    {
      graphics_object fig = go.get_ancestor ("figure");
      double dpr = fig.get ("__device_pixel_ratio__").double_value ();
      m_renderer.set_viewport (dpr * width (), dpr * height ());
      m_renderer.set_device_pixel_ratio (dpr);
      m_renderer.draw (go);
    }
}

uint8NDArray
GLCanvas::do_getPixels (const graphics_handle& gh)
{
  uint8NDArray retval;

  gh_manager& gh_mgr = m_interpreter.get_gh_manager ();

  graphics_object go = gh_mgr.get_object (gh);

  if (go && go.isa ("figure"))
    {
      Matrix pos = go.get ("position").matrix_value ();
      double dpr = go.get ("__device_pixel_ratio__").double_value ();
      pos(2) *= dpr;
      pos(3) *= dpr;

      // Make sure we have a valid current context
      if (! begin_rendering ())
        return retval;

      // When the figure is not visible or its size is frozen for printing,
      // we use a framebuffer object to make sure we are rendering on a
      // suitably large frame.
      if (go.get ("visible").string_value () == "off"
          || go.get ("__printing__").string_value () == "on")
        {
          OCTAVE_QT_OPENGL_FBO
            fbo (pos(2), pos(3),
                 OCTAVE_QT_OPENGL_FBO::Attachment::Depth);

          fbo.bind ();

          m_renderer.set_viewport (pos(2), pos(3));
          m_renderer.set_device_pixel_ratio (dpr);
          m_renderer.draw (go);
          retval = m_renderer.get_pixels (pos(2), pos(3));

          fbo.release ();
        }
      else
        {
          m_renderer.set_viewport (pos(2), pos(3));
          m_renderer.set_device_pixel_ratio (dpr);
          m_renderer.draw (go);
          retval = m_renderer.get_pixels (pos(2), pos(3));
        }

      end_rendering ();
    }

  return retval;
}

void
GLCanvas::do_print (const QString& file_cmd, const QString& term,
                    const graphics_handle& handle)
{
  gh_manager& gh_mgr = m_interpreter.get_gh_manager ();

  octave::autolock guard  (gh_mgr.graphics_lock ());

  graphics_object go = gh_mgr.get_object (handle);

  if (go.valid_object ())
    {
      graphics_object fig (go.get_ancestor ("figure"));

      // Make sure we have a valid current context
      if (! begin_rendering ())
        error ("print: no valid OpenGL offscreen context");

      try
        {
          if (fig.get ("visible").string_value () == "on")
            octave::gl2ps_print (m_glfcns, fig, file_cmd.toStdString (),
                                 term.toStdString ());
          else
            {
              // When the figure is not visible, we use a framebuffer object
              // to make sure we are rendering on a suitably large frame.
              Matrix pos = fig.get ("position").matrix_value ();
              double dpr = fig.get ("__device_pixel_ratio__").double_value ();
              pos(2) *= dpr;
              pos(3) *= dpr;

              OCTAVE_QT_OPENGL_FBO
                fbo (pos(2), pos(3),
                     OCTAVE_QT_OPENGL_FBO::Attachment::Depth);

              fbo.bind ();

              octave::gl2ps_print (m_glfcns, fig, file_cmd.toStdString (),
                                   term.toStdString ());

              fbo.release ();
            }
        }
      catch (octave::execution_exception& ee)
        {
          emit interpreter_event
            ([=] (void)
            {
              // INTERPRETER THREAD
              throw ee;
            });
        }

      end_rendering ();
    }
}

graphics_object
GLCanvas::selectFromAxes (const graphics_object& ax, const QPoint& pt)
{
  makeCurrent ();

  if (ax)
    {
      octave::opengl_selector s (m_glfcns);

      s.set_viewport (width (), height ());
      return s.select (ax, pt.x (), height () - pt.y (),
                       octave::select_ignore_hittest);
    }

  return graphics_object ();
}

void
GLCanvas::drawZoomBox (const QPoint& p1, const QPoint& p2)
{
  Matrix overlaycolor (3, 1);
  overlaycolor(0) = 0.45;
  overlaycolor(1) = 0.62;
  overlaycolor(2) = 0.81;
  double overlayalpha = 0.1;
  Matrix bordercolor = overlaycolor;
  double borderalpha = 0.9;
  double borderwidth = 1.5;

  m_renderer.draw_zoom_box (width (), height (),
                            p1.x (), p1.y (), p2.x (), p2.y (),
                            overlaycolor, overlayalpha,
                            bordercolor, borderalpha, borderwidth);
}

void
GLCanvas::paintGL (void)
{
  canvasPaintEvent ();
}

void
GLCanvas::mouseDoubleClickEvent (QMouseEvent *xevent)
{
  canvasMouseDoubleClickEvent (xevent);
}

void
GLCanvas::mouseMoveEvent (QMouseEvent *xevent)
{
  canvasMouseMoveEvent (xevent);
}

void
GLCanvas::mousePressEvent (QMouseEvent *xevent)
{
  canvasMousePressEvent (xevent);
}

void
GLCanvas::mouseReleaseEvent (QMouseEvent *xevent)
{
  canvasMouseReleaseEvent (xevent);
}

void
GLCanvas::wheelEvent (QWheelEvent *xevent)
{
  canvasWheelEvent (xevent);
}

void
GLCanvas::keyPressEvent (QKeyEvent *xevent)
{
  if (! canvasKeyPressEvent (xevent))
    OCTAVE_QT_OPENGL_WIDGET::keyPressEvent (xevent);
}

void
GLCanvas::keyReleaseEvent (QKeyEvent *xevent)
{
  if (! canvasKeyReleaseEvent (xevent))
    OCTAVE_QT_OPENGL_WIDGET::keyReleaseEvent (xevent);
}

bool
GLCanvas::begin_rendering (void)
{
  bool retval = true;

  if (! isValid ())
    {
#  if defined (HAVE_QT_OFFSCREEN)
      static bool os_ctx_ok = true;
      if (os_ctx_ok && ! m_os_context.isValid ())
        {
          // Try to initialize offscreen context
          m_os_surface.create ();
          if (! m_os_context.create ())
            {
              os_ctx_ok = false;
              return false;
            }
        }

      retval = m_os_context.makeCurrent (&m_os_surface);
#  else
      retval = false;
#  endif
    }
  else
    makeCurrent ();

  return retval;
}

void
GLCanvas::end_rendering (void)
{
  doneCurrent ();
}

OCTAVE_END_NAMESPACE(octave)
