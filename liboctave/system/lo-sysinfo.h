/*

Copyright (C) 2018 Markus Mützel

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

#if ! defined (octave_lo_sysinfo_h)
#define octave_lo_sysinfo_h 1

#include "octave-config.h"

#include <string>

namespace octave
{
  namespace sys
  {
    extern std::string blas_version (void);

    extern std::string lapack_version (void);
  }
}

#endif
