/*

Copyright (C) 2016 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if ! defined (octave_oct_opengl_h)
#define octave_oct_opengl_h 1

#include "octave-config.h"

#if defined (HAVE_GL_GL_H)
#  include <GL/gl.h>
#elif defined (HAVE_OPENGL_GL_H) || defined (HAVE_FRAMEWORK_OPENGL)
#  include <OpenGL/gl.h>
#endif

#if defined (HAVE_GL_GLU_H)
#  include <GL/glu.h>
#elif defined (HAVE_OPENGL_GLU_H) || defined (HAVE_FRAMEWORK_OPENGL)
#  include <OpenGL/glu.h>
#endif

#if defined (HAVE_GL_GLEXT_H)
#  include <GL/glext.h>
#elif defined (HAVE_OPENGL_GLEXT_H) || defined (HAVE_FRAMEWORK_OPENGL)
#  include <OpenGL/glext.h>
#endif

#endif
