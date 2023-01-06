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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "f77-fcn.h"
#include "lo-error.h"
#include "mach-info.h"

extern "C"
{
  int octave_get_float_format (void);

  int octave_is_big_endian (void);
}

OCTAVE_BEGIN_NAMESPACE(octave)

OCTAVE_BEGIN_NAMESPACE(mach_info)

static float_format get_float_format (void)
{
  switch (octave_get_float_format ())
    {
    case 1:
      return flt_fmt_ieee_little_endian;

    case 2:
      return flt_fmt_ieee_big_endian;

    default:
      return flt_fmt_unknown;
    }
}

static bool is_big_endian (void)
{
  return octave_is_big_endian ();
}

float_format native_float_format (void)
{
  static float_format fmt = get_float_format ();

  return fmt;
}

bool words_big_endian (void)
{
  static bool big_endian = is_big_endian ();

  return big_endian;
}

bool words_little_endian (void)
{
  static bool little_endian = ! is_big_endian ();

  return little_endian;
}

float_format string_to_float_format (const std::string& s)
{
  float_format retval = flt_fmt_unknown;

  if (s == "native" || s == "n")
    retval = native_float_format ();
  else if (s == "ieee-be" || s == "b")
    retval = flt_fmt_ieee_big_endian;
  else if (s == "ieee-le" || s == "l")
    retval = flt_fmt_ieee_little_endian;
  else if (s == "unknown")
    retval = flt_fmt_unknown;
  else
    (*current_liboctave_error_handler)
      ("invalid architecture type specified");

  return retval;
}

std::string float_format_as_string (float_format flt_fmt)
{
  std::string retval = "unknown";

  switch (flt_fmt)
    {
    case flt_fmt_ieee_big_endian:
      retval = "ieee-be";
      break;

    case flt_fmt_ieee_little_endian:
      retval = "ieee-le";
      break;

    default:
      break;
    }

  return retval;
}

OCTAVE_END_NAMESPACE(mach_info)
OCTAVE_END_NAMESPACE(octave)
