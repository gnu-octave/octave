/*

Copyright (C) 2011-2018 Michael Goffioul

This file is part of Octave.

Octave is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<https://www.gnu.org/licenses/>.

*/

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "gl-render.h"
#include "gl2ps-print.h"
#include "graphics.h"
#include "octave-link.h"

#include "GLCanvas.h"
#include "gl-select.h"

namespace QtHandles
{
#if defined (HAVE_QOPENGLWIDGET)
#  define OCTAVE_QT_OPENGL_WIDGET_FORMAT_ARGS
#else
#  if defined (Q_OS_WIN32)
#    define OCTAVE_QT_OPENGL_WIDGET_FORMAT_ARGS         \
  QGLFormat (QGL::SampleBuffers | QGL::AlphaChannel     \
             | QGL::IndirectRendering),
#  else
#    define OCTAVE_QT_OPENGL_WIDGET_FORMAT_ARGS \
  QGLFormat (QGL::SampleBuffers | QGL::AlphaChannel),
#  endif
#endif

  GLCanvas::GLCanvas (QWidget *xparent, const graphics_handle& gh)
    : OCTAVE_QT_OPENGL_WIDGET (OCTAVE_QT_OPENGL_WIDGET_FORMAT_ARGS xparent),
      Canvas (gh)
  {
    setFocusPolicy (Qt::ClickFocus);
    setFocus ();
  }

  GLCanvas::~GLCanvas (void)
  { }

  void
  GLCanvas::draw (const graphics_handle& gh)
  {
    gh_manager::auto_lock lock;
    graphics_object go = gh_manager::get_object (gh);

    if (go)
      {
        octave::opengl_renderer r;

        r.set_viewport (width (), height ());
        r.draw (go);
      }
  }

  uint8NDArray
  GLCanvas::do_getPixels (const graphics_handle& gh)
  {
    uint8NDArray retval;
    graphics_object go = gh_manager::get_object (gh);

    if (go && go.isa ("figure"))
      {
        Matrix pos = go.get ("position").matrix_value ();

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
            fbo (pos(2), pos(3),OCTAVE_QT_OPENGL_FBO::Attachment::Depth);

            fbo.bind ();

            octave::opengl_renderer r;
            r.set_viewport (pos(2), pos(3));
            r.draw (go);
            retval = r.get_pixels (pos(2), pos(3));

            fbo.release ();
          }
        else
          {
            octave::opengl_renderer r;
            r.set_viewport (pos(2), pos(3));
            r.draw (go);
            retval = r.get_pixels (pos(2), pos(3));
          }

        end_rendering ();
      }

    return retval;
  }

  void
  GLCanvas::do_print (const QString& file_cmd, const QString& term,
                      const graphics_handle& handle)
  {
    gh_manager::auto_lock lock;
    graphics_object obj = gh_manager::get_object (handle);

    if (obj.valid_object ())
      {
        graphics_object figObj (obj.get_ancestor ("figure"));
        try
          {
            // Make sure we have a valid current context
            if (! begin_rendering ())
              error ("print: no valid OpenGL offscreen context");

            octave::gl2ps_print (figObj, file_cmd.toStdString (),
                                 term.toStdString ());
          }
        catch (octave::execution_exception& e)
          {
            octave_link::post_exception (std::current_exception ());
            end_rendering ();
          }
      }
  }

  void
  GLCanvas::toggleAxes (const graphics_handle& gh)
  {
    canvasToggleAxes (gh);
  }

  void
  GLCanvas::toggleGrid (const graphics_handle& gh)
  {
    canvasToggleGrid (gh);
  }

  void
  GLCanvas::autoAxes (const graphics_handle& gh)
  {
    canvasAutoAxes (gh);
  }

  graphics_object
  GLCanvas::selectFromAxes (const graphics_object& ax, const QPoint& pt)
  {
    makeCurrent ();

    if (ax)
      {
        octave::opengl_selector s;

        s.set_viewport (width (), height ());
        return s.select (ax, pt.x (), height () - pt.y (),
                         octave::select_ignore_hittest);
      }

    return graphics_object ();
  }

  inline void
  glDrawZoomBox (const QPoint& p1, const QPoint& p2)
  {
    glVertex2d (p1.x (), p1.y ());
    glVertex2d (p2.x (), p1.y ());
    glVertex2d (p2.x (), p2.y ());
    glVertex2d (p1.x (), p2.y ());
    glVertex2d (p1.x (), p1.y ());
  }

  void
  GLCanvas::drawZoomBox (const QPoint& p1, const QPoint& p2)
  {
    glMatrixMode (GL_MODELVIEW);
    glPushMatrix ();
    glLoadIdentity ();

    glMatrixMode (GL_PROJECTION);
    glPushMatrix ();
    glLoadIdentity ();
    glOrtho (0, width (), height (), 0, 1, -1);

    glPushAttrib (GL_DEPTH_BUFFER_BIT | GL_CURRENT_BIT);
    glDisable (GL_DEPTH_TEST);

    glBegin (GL_POLYGON);
    glColor4f (0.45, 0.62, 0.81, 0.1);
    glDrawZoomBox (p1, p2);
    glEnd ();

    glLineWidth (1.5);
    glBegin (GL_LINE_STRIP);
    glColor4f (0.45, 0.62, 0.81, 0.9);
    glDrawZoomBox (p1, p2);
    glEnd ();

    glPopAttrib ();

    glMatrixMode (GL_MODELVIEW);
    glPopMatrix ();

    glMatrixMode (GL_PROJECTION);
    glPopMatrix ();
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
}
