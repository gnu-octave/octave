/*

Copyright (C) 2012-2018 John W. Eaton

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

#if ! defined (octave_oct_base64_h)
#define octave_oct_base64_h 1

#include "octave-config.h"

#include <string>

template <typename T> class Array;

namespace octave
{
  extern OCTAVE_API bool
  base64_encode (const char *inc, const size_t inlen, char **out);

  extern OCTAVE_API Array<double>
  base64_decode (const std::string& str);
}

#if defined (OCTAVE_USE_DEPRECATED_FUNCTIONS)

OCTAVE_DEPRECATED (4.4, "use 'octave::base_64_encode' instead")
extern bool
octave_base64_encode (const char *inc, const size_t inlen, char **out);

OCTAVE_DEPRECATED (4.4, "use 'octave::base_64_decode' instead")
extern Array<double>
octave_base64_decode (const std::string& str);

#endif

#endif
