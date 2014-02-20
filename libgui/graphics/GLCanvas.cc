/*

Copyright (C) 2011-2014 Michael Goffioul

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "gl-render.h"
#include "graphics.h"

#include "GLCanvas.h"
#include "gl-select.h"

//////////////////////////////////////////////////////////////////////////////

namespace QtHandles
{

//////////////////////////////////////////////////////////////////////////////

GLCanvas::GLCanvas (QWidget* parent, const graphics_handle& handle)
  : QGLWidget (parent), Canvas (handle)
{
  setFocusPolicy (Qt::ClickFocus);
}

//////////////////////////////////////////////////////////////////////////////

GLCanvas::~GLCanvas (void)
{
}

//////////////////////////////////////////////////////////////////////////////

void GLCanvas::draw (const graphics_handle& handle)
{
  graphics_object go = gh_manager::get_object (handle);

  if (go)
    {
      opengl_renderer r;

      r.set_viewport (width (), height ());
      r.draw(go);
    }
}

//////////////////////////////////////////////////////////////////////////////

graphics_object GLCanvas::selectFromAxes (const graphics_object& ax,
                                          const QPoint& pt)
{
  makeCurrent ();

  if (ax)
    {
      opengl_selector s;

      s.set_viewport (width (), height ());
      return s.select (ax, pt.x (), height () - pt.y ());
    }

  return graphics_object ();
}

//////////////////////////////////////////////////////////////////////////////

inline void glDrawZoomBox (const QPoint& p1, const QPoint& p2)
{
  glVertex2d (p1.x (), p1.y ());
  glVertex2d (p2.x (), p1.y ());
  glVertex2d (p2.x (), p2.y ());
  glVertex2d (p1.x (), p2.y ());
  glVertex2d (p1.x (), p1.y ());
}

void GLCanvas::drawZoomBox (const QPoint& p1, const QPoint& p2)
{
  glPushMatrix ();

  glMatrixMode (GL_MODELVIEW);
  glLoadIdentity ();

  glMatrixMode (GL_PROJECTION);
  glLoadIdentity ();
  glOrtho (0, width (), height (), 0, 1, -1);

  glPushAttrib (GL_DEPTH_BUFFER_BIT | GL_CURRENT_BIT);
  glDisable (GL_DEPTH_TEST);

  glBegin (GL_POLYGON);
  glColor4f (0.45, 0.62, 0.81, 0.1);
  glDrawZoomBox (p1, p2);
  glEnd ();

  glBegin (GL_LINE_STRIP);
  glLineWidth (1.5);
  glColor4f (0.45, 0.62, 0.81, 0.9);
  glDrawZoomBox (p1, p2);
  glEnd ();

  glPopAttrib ();
  glPopMatrix ();
}

//////////////////////////////////////////////////////////////////////////////

void GLCanvas::paintGL (void)
{
  canvasPaintEvent ();
}

//////////////////////////////////////////////////////////////////////////////

void GLCanvas::mouseMoveEvent (QMouseEvent* event)
{
  canvasMouseMoveEvent (event);
}

//////////////////////////////////////////////////////////////////////////////

void GLCanvas::mousePressEvent (QMouseEvent* event)
{
  canvasMousePressEvent (event);
}

//////////////////////////////////////////////////////////////////////////////

void GLCanvas::mouseReleaseEvent (QMouseEvent* event)
{
  canvasMouseReleaseEvent (event);
}

//////////////////////////////////////////////////////////////////////////////

void GLCanvas::keyPressEvent (QKeyEvent* event)
{
  if (! canvasKeyPressEvent (event))
    QGLWidget::keyPressEvent (event);
}

//////////////////////////////////////////////////////////////////////////////

void GLCanvas::keyReleaseEvent (QKeyEvent* event)
{
  if (! canvasKeyReleaseEvent (event))
    QGLWidget::keyReleaseEvent (event);
}

//////////////////////////////////////////////////////////////////////////////

}; // namespace QtHandles
