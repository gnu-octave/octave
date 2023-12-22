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

#if ! defined (octave_GLCanvas_h)
#define octave_GLCanvas_h 1

#include <QOffscreenSurface>
#include <QOpenGLContext>
#include <QOpenGLFramebufferObject>
#include <QOpenGLWidget>

#include "Canvas.h"

#include "gl-render.h"
#include "qopengl-functions.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class GLWidget : public QOpenGLWidget
{
  Q_OBJECT

public:

  GLWidget (Canvas& parent_canvas, QWidget *parent);

  ~GLWidget ();

  void initializeGL ();

  void draw (graphics_object go);
  uint8NDArray  do_getPixels (graphics_object go);
  void do_print (const QString& file_cmd, const QString& term,
                 graphics_object go);
  void drawZoomBox (const QPoint& p1, const QPoint& p2);
  void resize (int /* x */, int /* y */,
               int /* width */, int /* height */) { }
  graphics_object selectFromAxes (const graphics_object& ax,
                                  const QPoint& pt);

  bool begin_rendering ();
  void end_rendering ();

protected:

  void paintGL ();
  void mouseDoubleClickEvent (QMouseEvent *event);
  void mouseMoveEvent (QMouseEvent *event);
  void mousePressEvent (QMouseEvent *event);
  void mouseReleaseEvent (QMouseEvent *event);
  void wheelEvent (QWheelEvent *event);
  void keyPressEvent (QKeyEvent *event);
  void keyReleaseEvent (QKeyEvent *event);

private:

  Canvas& m_parent_canvas;

  qopengl_functions m_glfcns;
  opengl_renderer m_renderer;

  QOpenGLContext m_os_context;
  QOffscreenSurface m_os_surface;
};

class GLCanvas : public Canvas
{
public:

  GLCanvas (octave::interpreter& interp, const graphics_handle& handle,
            QWidget *parent);

  ~GLCanvas ();

  void draw (const graphics_handle& handle);
  uint8NDArray  do_getPixels (const graphics_handle& handle);
  void do_print (const QString& file_cmd, const QString& term,
                 const graphics_handle& handle);
  void drawZoomBox (const QPoint& p1, const QPoint& p2);
  void resize (int /* x */, int /* y */,
               int /* width */, int /* height */) { }
  graphics_object selectFromAxes (const graphics_object& ax,
                                  const QPoint& pt);

  QWidget * qWidget () { return m_glwidget; }

private:

  GLWidget *m_glwidget;

  bool begin_rendering ();
  void end_rendering ();
};

OCTAVE_END_NAMESPACE(octave)

#endif
