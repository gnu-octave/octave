/*

Copyright (C) 1996-2015 John W. Eaton

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

#if ! defined (octave_lo_sysdep_h)
#define octave_lo_sysdep_h 1

#include "octave-config.h"

#include <string>

#include "lo-ieee.h"

class string_vector;

namespace octave
{
  namespace sys
  {
    extern std::string getcwd (void);

    extern int chdir (const std::string&);

#if defined (__WIN32__) && ! defined (__CYGWIN__)
    extern pid_t popen2 (const std::string&, const string_vector&,
                         bool, int *, std::string&);
#endif
  }
}

#if defined (OCTAVE_USE_DEPRECATED_FUNCTIONS)

OCTAVE_DEPRECATED ("use octave::sys::getcwd instead")
const auto octave_getcwd = octave::sys::getcwd;

OCTAVE_DEPRECATED ("use octave::sys::chdir instead")
const auto octave_chdir = octave::sys::chdir;

#if defined (__WIN32__) && ! defined (__CYGWIN__)
OCTAVE_DEPRECATED ("use octave::sys:: instead")
const auto octave_popen2 = octave::sys::popen2;
#endif

#endif

#endif
