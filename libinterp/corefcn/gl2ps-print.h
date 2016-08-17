/*

Copyright (C) 2009-2016 Shai Ayal

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

#if ! defined (octave_gl2ps_renderer_h)
#define octave_gl2ps_renderer_h 1

#include "octave-config.h"

#include <string>

#include "graphics.h"

namespace octave
{
  extern OCTINTERP_API void
  gl2ps_print (const graphics_object& fig, const std::string& stream,
               const std::string& term);
}

#if defined (OCTAVE_USE_DEPRECATED_FUNCTIONS)

inline void
gl2ps_print (const graphics_object& fig, const std::string& stream,
             const std::string& term)
{
  return octave::gl2ps_print (fig, stream, term);
} 

#endif

#endif
