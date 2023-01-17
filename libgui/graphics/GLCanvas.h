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

#if ! defined (octave_GLCanvas_h)
#define octave_GLCanvas_h 1

#include <QOpenGLWidget>
#include <QOpenGLFramebufferObject>
#include <QOpenGLContext>
#include <QOffscreenSurface>

#include "Canvas.h"

#include "gl-render.h"
#include "qopengl-functions.h"

OCTAVE_BEGIN_NAMESPACE(octave)

  class GLCanvas : public QOpenGLWidget, public Canvas
  {
  public:
    GLCanvas (octave::interpreter& interp,
              const graphics_handle& handle, QWidget *parent);
    ~GLCanvas (void);

    void initializeGL (void);

    void draw (const graphics_handle& handle);
    uint8NDArray  do_getPixels (const graphics_handle& handle);
    void do_print (const QString& file_cmd, const QString& term,
                   const graphics_handle& handle);
    void drawZoomBox (const QPoint& p1, const QPoint& p2);
    void resize (int /* x */, int /* y */,
                 int /* width */, int /* height */) { }
    graphics_object selectFromAxes (const graphics_object& ax,
                                    const QPoint& pt);
    QWidget * qWidget (void) { return this; }

  protected:
    void paintGL (void);
    void mouseDoubleClickEvent (QMouseEvent *event);
    void mouseMoveEvent (QMouseEvent *event);
    void mousePressEvent (QMouseEvent *event);
    void mouseReleaseEvent (QMouseEvent *event);
    void wheelEvent (QWheelEvent *event);
    void keyPressEvent (QKeyEvent *event);
    void keyReleaseEvent (QKeyEvent *event);

  private:

    bool begin_rendering (void);
    void end_rendering (void);

    octave::qopengl_functions m_glfcns;
    octave::opengl_renderer m_renderer;

    QOpenGLContext m_os_context;
    QOffscreenSurface m_os_surface;
  };

OCTAVE_END_NAMESPACE(octave)

#endif
