////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2023 The Octave Project Developers
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

#if ! defined (octave_mach_info_h)
#define octave_mach_info_h 1

#include "octave-config.h"

#include <string>

OCTAVE_BEGIN_NAMESPACE(octave)

OCTAVE_BEGIN_NAMESPACE(mach_info)

enum float_format
{
  // If these values change, you must also change the values
  // returned by octave_get_float_format.

  flt_fmt_unknown = 0,
  flt_fmt_ieee_little_endian = 1,
  flt_fmt_ieee_big_endian = 2,
};

OCTAVE_API float_format native_float_format (void);

OCTAVE_API bool words_big_endian (void);

OCTAVE_API bool words_little_endian (void);

OCTAVE_API float_format string_to_float_format (const std::string&);

OCTAVE_API std::string float_format_as_string (float_format);

OCTAVE_END_NAMESPACE(mach_info)
OCTAVE_END_NAMESPACE(octave)

#endif
