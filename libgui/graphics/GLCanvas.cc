////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2011-2024 The Octave Project Developers
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

GLWidget::GLWidget (Canvas& parent_canvas, QWidget *parent)
  : QOpenGLWidget (parent), m_parent_canvas (parent_canvas),
    m_glfcns (), m_renderer (m_glfcns)
{
  setFocusPolicy (Qt::ClickFocus);
  setFocus ();
}

GLWidget::~GLWidget () { }

void
GLWidget::initializeGL ()
{
  // The qopengl_functions object (part of Octave, not Qt) is just
  // wrapper around QOpenGLFunctions_1_1.  Does initialization really
  // need to be deferred until initializeGL is called?

  m_glfcns.init ();

  // All other resources we need are currently (supposed to be)
  // managed by the QOpenGLWidget object so there is else nothing to
  // do here.  If we used custom shader programs, then we would need
  // to initialize them here.
}

void
GLWidget::draw (graphics_object go)
{
  if (go)
    {
      begin_rendering ();

      unwind_action reset_current ([=] () { end_rendering (); });

      graphics_object fig = go.get_ancestor ("figure");
      double dpr = fig.get ("__device_pixel_ratio__").double_value ();
      m_renderer.set_viewport (dpr * width (), dpr * height ());
      m_renderer.set_device_pixel_ratio (dpr);

      m_renderer.draw (go);
    }
}

uint8NDArray
GLWidget::do_getPixels (graphics_object go)
{
  uint8NDArray retval;

  if (go && go.isa ("figure"))
    {
      Matrix pos = go.get ("position").matrix_value ();
      double dpr = go.get ("__device_pixel_ratio__").double_value ();
      pos(2) *= dpr;
      pos(3) *= dpr;

      begin_rendering ();

      unwind_action reset_current ([=] () { end_rendering (); });

      // When the figure is not visible or its size is frozen for printing,
      // we use a framebuffer object to make sure we are rendering on a
      // suitably large frame.
      if (go.get ("visible").string_value () == "off"
          || go.get ("__printing__").string_value () == "on")
        {
          QOpenGLFramebufferObject
            fbo (pos(2), pos(3),
                 QOpenGLFramebufferObject::Attachment::Depth);

          fbo.bind ();

          unwind_action release_fbo ([&] () { fbo.release (); });

          m_renderer.set_viewport (pos(2), pos(3));
          m_renderer.set_device_pixel_ratio (dpr);
          m_renderer.draw (go);

          retval = m_renderer.get_pixels (pos(2), pos(3));
        }
      else
        {
          m_renderer.set_viewport (pos(2), pos(3));
          m_renderer.set_device_pixel_ratio (dpr);
          m_renderer.draw (go);

          retval = m_renderer.get_pixels (pos(2), pos(3));
        }
    }

  return retval;
}

void
GLWidget::do_print (const QString& file_cmd, const QString& term,
                    graphics_object go)
{
  if (go.valid_object ())
    {
      begin_rendering ();

      unwind_action reset_current ([=] () { end_rendering (); });

      graphics_object fig (go.get_ancestor ("figure"));

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

          QOpenGLFramebufferObject
            fbo (pos(2), pos(3),
                 QOpenGLFramebufferObject::Attachment::Depth);

          fbo.bind ();

          unwind_action release_fbo ([&] () { fbo.release (); });

          octave::gl2ps_print (m_glfcns, fig, file_cmd.toStdString (),
                               term.toStdString ());
        }
    }
}

graphics_object
GLWidget::selectFromAxes (const graphics_object& ax, const QPoint& pt)
{
  if (ax)
    {
      begin_rendering ();

      unwind_action reset_current ([=] () { end_rendering (); });

      octave::opengl_selector s (m_glfcns);

      s.set_viewport (width (), height ());

      return s.select (ax, pt.x (), height () - pt.y (),
                       octave::select_ignore_hittest);
    }

  return graphics_object ();
}

void
GLWidget::drawZoomBox (const QPoint& p1, const QPoint& p2)
{
  Matrix overlaycolor (3, 1);
  overlaycolor(0) = 0.45;
  overlaycolor(1) = 0.62;
  overlaycolor(2) = 0.81;
  double overlayalpha = 0.1;
  Matrix bordercolor = overlaycolor;
  double borderalpha = 0.9;
  double borderwidth = 1.5;

  begin_rendering ();

  unwind_action reset_current ([=] () { end_rendering (); });

  m_renderer.draw_zoom_box (width (), height (),
                            p1.x (), p1.y (), p2.x (), p2.y (),
                            overlaycolor, overlayalpha,
                            bordercolor, borderalpha, borderwidth);
}

void
GLWidget::paintGL ()
{
  m_parent_canvas.canvasPaintEvent ();
}

void
GLWidget::mouseDoubleClickEvent (QMouseEvent *xevent)
{
  m_parent_canvas.canvasMouseDoubleClickEvent (xevent);
}

void
GLWidget::mouseMoveEvent (QMouseEvent *xevent)
{
  m_parent_canvas.canvasMouseMoveEvent (xevent);
}

void
GLWidget::mousePressEvent (QMouseEvent *xevent)
{
  m_parent_canvas.canvasMousePressEvent (xevent);
}

void
GLWidget::mouseReleaseEvent (QMouseEvent *xevent)
{
  m_parent_canvas.canvasMouseReleaseEvent (xevent);
}

void
GLWidget::wheelEvent (QWheelEvent *xevent)
{
  m_parent_canvas.canvasWheelEvent (xevent);
}

void
GLWidget::keyPressEvent (QKeyEvent *xevent)
{
  if (! m_parent_canvas.canvasKeyPressEvent (xevent))
    QOpenGLWidget::keyPressEvent (xevent);
}

void
GLWidget::keyReleaseEvent (QKeyEvent *xevent)
{
  if (! m_parent_canvas.canvasKeyReleaseEvent (xevent))
    QOpenGLWidget::keyReleaseEvent (xevent);
}

bool
GLWidget::begin_rendering ()
{
  bool retval = true;

  if (! isValid ())
    {
      // FIXME: Is this really the right way to manager offscreen
      // rendering for printing?

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
    }
  else
    makeCurrent ();

  return retval;
}

void
GLWidget::end_rendering ()
{
  doneCurrent ();
}

GLCanvas::GLCanvas (octave::interpreter& interp,
                    const graphics_handle& gh, QWidget *parent)
  : Canvas (interp, gh), m_glwidget (new GLWidget (*this, parent))
{ }

GLCanvas::~GLCanvas ()
{
  delete m_glwidget;
}

void
GLCanvas::draw (const graphics_handle& gh)
{
  gh_manager& gh_mgr = m_interpreter.get_gh_manager ();

  octave::autolock guard  (gh_mgr.graphics_lock ());

  graphics_object go = gh_mgr.get_object (gh);

  m_glwidget->draw (go);
}

uint8NDArray
GLCanvas::do_getPixels (const graphics_handle& gh)
{
  uint8NDArray retval;

  gh_manager& gh_mgr = m_interpreter.get_gh_manager ();

  graphics_object go = gh_mgr.get_object (gh);

  return m_glwidget->do_getPixels (go);
}

void
GLCanvas::do_print (const QString& file_cmd, const QString& term,
                    const graphics_handle& handle)
{
  gh_manager& gh_mgr = m_interpreter.get_gh_manager ();

  octave::autolock guard  (gh_mgr.graphics_lock ());

  graphics_object go = gh_mgr.get_object (handle);

  try
    {
      m_glwidget->do_print (file_cmd, term, go);
    }
  catch (octave::execution_exception& ee)
    {
      emit interpreter_event
        ([=] ()
        {
          // INTERPRETER THREAD
          throw ee;
        });
    }
}

graphics_object
GLCanvas::selectFromAxes (const graphics_object& ax, const QPoint& pt)
{
  return m_glwidget->selectFromAxes (ax, pt);
}

void
GLCanvas::drawZoomBox (const QPoint& p1, const QPoint& p2)
{
  m_glwidget->drawZoomBox (p1, p2);
}

bool
GLCanvas::begin_rendering ()
{
  return m_glwidget->begin_rendering ();
}

void
GLCanvas::end_rendering ()
{
  m_glwidget->end_rendering ();
}

OCTAVE_END_NAMESPACE(octave)
