////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2018-2023 The Octave Project Developers
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

#if ! defined (octave_qopengl_functions_h)
#define octave_qopengl_functions_h 1

#include "oct-opengl.h"

#if defined (HAVE_QOPENGLFUNCTIONS_1_1)
#  include <QOpenGLFunctions_1_1>
#endif

OCTAVE_BEGIN_NAMESPACE(octave)

// If we don't have QOPENGLFUNCTIONS_1_1, then we will default to
// calling OpenGL functions directly through the base
// opengl_functions class.

class qopengl_functions : public opengl_functions
{
public:

  qopengl_functions (void)
#if defined (HAVE_QOPENGLFUNCTIONS_1_1)
    : m_glfcns ()
#endif
  { }

  qopengl_functions (const qopengl_functions&) = default;

  qopengl_functions& operator = (const qopengl_functions&) = default;

  ~qopengl_functions (void) = default;

  void init (void)
  {
#if defined (HAVE_QOPENGLFUNCTIONS_1_1)
    m_glfcns.initializeOpenGLFunctions ();
#endif
  }

#if defined (HAVE_QOPENGLFUNCTIONS_1_1)

  void glAlphaFunc (GLenum fcn, GLclampf ref)
  {
    m_glfcns.glAlphaFunc (fcn, ref);
  }

  void glBegin (GLenum mode)
  {
    m_glfcns.glBegin (mode);
  }

  void glBindTexture (GLenum target, GLuint texture)
  {
    m_glfcns.glBindTexture (target, texture);
  }

  void glBitmap (GLsizei width, GLsizei height, GLfloat xorig, GLfloat yorig,
                 GLfloat xmove, GLfloat ymove, const GLubyte *bitmap)
  {
    m_glfcns.glBitmap (width, height, xorig, yorig, xmove, ymove, bitmap);
  }

  void glBlendFunc (GLenum sfactor, GLenum dfactor)
  {
    m_glfcns.glBlendFunc (sfactor, dfactor);
  }

  void glCallList (GLuint list)
  {
    m_glfcns.glCallList (list);
  }

  void glClearColor (GLclampf red, GLclampf green, GLclampf blue,
                     GLclampf alpha)
  {
    m_glfcns.glClearColor (red, green, blue, alpha);
  }

  void glClear (GLbitfield mask)
  {
    m_glfcns.glClear (mask);
  }

  void glClipPlane (GLenum plane, const GLdouble *equation)
  {
    m_glfcns.glClipPlane (plane, equation);
  }

  void glColor3dv (const GLdouble *v)
  {
    m_glfcns.glColor3dv (v);
  }

  void glColor3f (GLfloat red, GLfloat green, GLfloat blue)
  {
    m_glfcns.glColor3f (red, green, blue);
  }

  void glColor3fv (const GLfloat *v)
  {
    m_glfcns.glColor3fv (v);
  }

  void glColor4d (GLdouble red, GLdouble green, GLdouble blue, GLdouble alpha)
  {
    m_glfcns.glColor4d (red, green, blue, alpha);
  }

  void glColor4f (GLfloat red, GLfloat green, GLfloat blue, GLfloat alpha)
  {
    m_glfcns.glColor4f (red, green, blue, alpha);
  }

  void glColor4fv (const GLfloat *v)
  {
    m_glfcns.glColor4fv (v);
  }

  void glDeleteLists (GLuint list, GLsizei range)
  {
    m_glfcns.glDeleteLists (list, range);
  }

  void glDeleteTextures (GLsizei n, const GLuint *textures)
  {
    m_glfcns.glDeleteTextures (n, textures);
  }

  void glDepthFunc (GLenum fcn)
  {
    m_glfcns.glDepthFunc (fcn);
  }

  void glDisable (GLenum cap)
  {
    m_glfcns.glDisable (cap);
  }

  void glDrawPixels (GLsizei width, GLsizei height, GLenum format,
                     GLenum type, const GLvoid *pixels)
  {
    m_glfcns.glDrawPixels (width, height, format, type, pixels);
  }

  void glEdgeFlag (GLboolean flag)
  {
    m_glfcns.glEdgeFlag (flag);
  }

  void glEnable (GLenum cap)
  {
    m_glfcns.glEnable (cap);
  }

  void glEndList (void)
  {
    m_glfcns.glEndList ();
  }

  void glEnd (void)
  {
    m_glfcns.glEnd ();
  }

  void glFinish (void)
  {
    m_glfcns.glFinish ();
  }

  GLuint glGenLists (GLsizei range)
  {
    return m_glfcns.glGenLists (range);
  }

  void glGenTextures (GLsizei n, GLuint *textures)
  {
    m_glfcns.glGenTextures (n, textures);
  }

  void glGetBooleanv (GLenum pname, GLboolean *data)
  {
    m_glfcns.glGetBooleanv (pname, data);
  }

  void glGetDoublev (GLenum pname, GLdouble *data)
  {
    m_glfcns.glGetDoublev (pname, data);
  }

  GLenum glGetError (void)
  {
    return m_glfcns.glGetError ();
  }

  void glGetFloatv (GLenum pname, GLfloat *data)
  {
    m_glfcns.glGetFloatv (pname, data);
  }

  void glGetIntegerv (GLenum pname, GLint *data)
  {
    m_glfcns.glGetIntegerv (pname, data);
  }

  const GLubyte * glGetString (GLenum name)
  {
    return m_glfcns.glGetString (name);
  }

  void glHint (GLenum target, GLenum mode)
  {
    m_glfcns.glHint (target, mode);
  }

  void glInitNames (void)
  {
    m_glfcns.glInitNames ();
  }

  GLboolean glIsEnabled (GLenum cap)
  {
    return m_glfcns.glIsEnabled (cap);
  }

  void glLightfv (GLenum light, GLenum pname, const GLfloat *params)
  {
    m_glfcns.glLightfv (light, pname, params);
  }

  void glLineStipple (GLint factor, GLushort pattern)
  {
    m_glfcns.glLineStipple (factor, pattern);
  }

  void glLineWidth (GLfloat width)
  {
    m_glfcns.glLineWidth (width);
  }

  void glLoadIdentity (void)
  {
    m_glfcns.glLoadIdentity ();
  }

  void glMaterialf (GLenum face, GLenum pname, GLfloat param)
  {
    m_glfcns.glMaterialf (face, pname, param);
  }

  void glMaterialfv (GLenum face, GLenum pname, const GLfloat *params)
  {
    m_glfcns.glMaterialfv (face, pname, params);
  }

  void glMatrixMode (GLenum mode)
  {
    m_glfcns.glMatrixMode (mode);
  }

  void glMultMatrixd (const GLdouble *m)
  {
    m_glfcns.glMultMatrixd (m);
  }

  void glNewList (GLuint list, GLenum mode)
  {
    m_glfcns.glNewList (list, mode);
  }

  void glNormal3d (GLdouble nx, GLdouble ny, GLdouble nz)
  {
    m_glfcns.glNormal3d (nx, ny, nz);
  }

  void glNormal3dv (const GLdouble *v)
  {
    m_glfcns.glNormal3dv (v);
  }

  void glOrtho (GLdouble left, GLdouble right, GLdouble bottom, GLdouble top,
                GLdouble near_val, GLdouble far_val)
  {
    m_glfcns.glOrtho (left, right, bottom, top, near_val, far_val);
  }

  void glPixelStorei (GLenum pname, GLint param)
  {
    m_glfcns.glPixelStorei (pname, param);
  }

  void glPixelZoom (GLfloat xfactor, GLfloat yfactor)
  {
    m_glfcns.glPixelZoom (xfactor, yfactor);
  }

  void glPolygonMode (GLenum face, GLenum mode)
  {
    m_glfcns.glPolygonMode (face, mode);
  }

  void glPolygonOffset (GLfloat factor, GLfloat units)
  {
    m_glfcns.glPolygonOffset (factor, units);
  }

  void glPopAttrib (void)
  {
    m_glfcns.glPopAttrib ();
  }

  void glPopMatrix (void)
  {
    m_glfcns.glPopMatrix ();
  }

  void glPopName (void)
  {
    m_glfcns.glPopName ();
  }

  void glPushAttrib (GLbitfield mask)
  {
    m_glfcns.glPushAttrib (mask);
  }

  void glPushMatrix (void)
  {
    m_glfcns.glPushMatrix ();
  }

  void glPushName (GLuint name)
  {
    m_glfcns.glPushName (name);
  }

  void glRasterPos3d (GLdouble x, GLdouble y, GLdouble z)
  {
    m_glfcns.glRasterPos3d (x, y, z);
  }

  void glReadPixels (GLint x, GLint y, GLsizei width, GLsizei height,
                     GLenum format, GLenum type, GLvoid *pixels)
  {
    m_glfcns.glReadPixels (x, y, width, height, format, type, pixels);
  }

  GLint glRenderMode (GLenum mode)
  {
    return m_glfcns.glRenderMode (mode);
  }

  void glRotated (GLdouble angle, GLdouble x, GLdouble y, GLdouble z)
  {
    m_glfcns.glRotated (angle, x, y, z);
  }

  void glScaled (GLdouble x, GLdouble y, GLdouble z)
  {
    m_glfcns.glScaled (x, y, z);
  }

  void glScalef (GLfloat x, GLfloat y, GLfloat z)
  {
    m_glfcns.glScalef (x, y, z);
  }

  void glSelectBuffer (GLsizei size, GLuint *buffer)
  {
    m_glfcns.glSelectBuffer (size, buffer);
  }

  void glShadeModel (GLenum mode)
  {
    m_glfcns.glShadeModel (mode);
  }

  void glTexCoord2d (GLdouble s, GLdouble t)
  {
    m_glfcns.glTexCoord2d (s, t);
  }

  void glTexImage2D (GLenum target, GLint level, GLint internalFormat,
                     GLsizei width, GLsizei height, GLint border,
                     GLenum format, GLenum type, const GLvoid *pixels)
  {
    m_glfcns.glTexImage2D (target, level, internalFormat, width, height,
                           border, format, type, pixels);
  }

  void glTexParameteri (GLenum target, GLenum pname, GLint param)
  {
    m_glfcns.glTexParameteri (target, pname, param);
  }

  void glTranslated (GLdouble x, GLdouble y, GLdouble z)
  {
    m_glfcns.glTranslated (x, y, z);
  }

  void glTranslatef (GLfloat x, GLfloat y, GLfloat z)
  {
    m_glfcns.glTranslatef (x, y, z);
  }

  void glVertex2d (GLdouble x, GLdouble y)
  {
    m_glfcns.glVertex2d (x, y);
  }

  void glVertex3d (GLdouble x, GLdouble y, GLdouble z)
  {
    m_glfcns.glVertex3d (x, y, z);
  }

  void glVertex3dv (const GLdouble *v)
  {
    m_glfcns.glVertex3dv (v);
  }

  void glViewport (GLint x, GLint y, GLsizei width, GLsizei height)
  {
    m_glfcns.glViewport (x, y, width, height);
  }

private:

  QOpenGLFunctions_1_1 m_glfcns;

#endif
};

OCTAVE_END_NAMESPACE(octave)

#endif
