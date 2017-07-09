/*

Copyright (C) 1996-2017 John W. Eaton

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

#if ! defined (octave_mach_info_h)
#define octave_mach_info_h 1

#include "octave-config.h"

#include <string>

namespace octave
{
  namespace mach_info
  {
    enum float_format
    {
      flt_fmt_unknown,
      flt_fmt_ieee_little_endian,
      flt_fmt_ieee_big_endian,
    };

    float_format native_float_format (void);

    bool words_big_endian (void);

    bool words_little_endian (void);

    float_format string_to_float_format (const std::string&);

    std::string float_format_as_string (float_format);
  }
}

#if defined (OCTAVE_USE_DEPRECATED_FUNCTIONS)

class oct_mach_info
{
public:

  typedef octave::mach_info::float_format float_format;

  OCTAVE_DEPRECATED (4.4, "use 'octave::mach_info::native_float_format' instead")
  static float_format native_float_format (void)
  {
    return octave::mach_info::native_float_format ();
  }

  OCTAVE_DEPRECATED (4.4, "use 'octave::mach_info::words_big_endian' instead")
  static bool words_big_endian (void)
  {
    return octave::mach_info::words_big_endian ();
  }

  OCTAVE_DEPRECATED (4.4, "use 'octave::mach_info::words_little_endian' instead")
  static bool words_little_endian (void)
  {
    return octave::mach_info::words_little_endian ();
  }

  OCTAVE_DEPRECATED (4.4, "use 'octave::mach_info::string_to_float_format' instead")
  static float_format string_to_float_format (const std::string& str)
  {
    return octave::mach_info::string_to_float_format (str);
  }

  OCTAVE_DEPRECATED (4.4, "use 'octave::mach_info::float_format_as_string' instead")
  static std::string float_format_as_string (float_format ff)
  {
    return octave::mach_info::float_format_as_string (ff);
  }
};

#endif

#endif
