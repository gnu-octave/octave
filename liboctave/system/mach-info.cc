////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2026 The Octave Project Developers
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

#include <cstdint>

#include "f77-fcn.h"
#include "mach-info.h"
#include "oct-error.h"

OCTAVE_BEGIN_NAMESPACE(octave)
OCTAVE_BEGIN_NAMESPACE(mach_info)

// FIXME: Maybe this function should be declared in a header file?  Or
// we should be obtaining values from C++ std::numeric_limits<double>?

extern "C"
{
  extern double F77_FUNC (d1mach, D1MACH) (const F77_INT*);
}

typedef union
{
  double d;
  int32_t i[2];
} equiv;

typedef struct
{
  int fp_fmt;
  equiv fp_par[4];
} float_params;

#define INIT_FLT_PAR(fp, fmt, sm1, sm2, lrg1, lrg2, rt1, rt2, dv1, dv2) \
  do                                                                    \
    {                                                                   \
      fp.fp_fmt = (fmt);                                                \
      fp.fp_par[0].i[0] = (sm1);  fp.fp_par[0].i[1] = (sm2);            \
      fp.fp_par[1].i[0] = (lrg1); fp.fp_par[1].i[1] = (lrg2);           \
      fp.fp_par[2].i[0] = (rt1);  fp.fp_par[2].i[1] = (rt2);            \
      fp.fp_par[3].i[0] = (dv1);  fp.fp_par[3].i[1] = (dv2);            \
    }                                                                   \
  while (0)

static int equiv_compare (const equiv *std, const equiv *v, int len)
{
  int i;
  for (i = 0; i < len; i++)
    if (v[i].i[0] != std[i].i[0] || v[i].i[1] != std[i].i[1])
      return 0;
  return 1;
}

// Determine whether floating point format appears to be IEEE little
// endian (1), IEEE big endian (2) or unknown (0)
// little endian, or 2 if it is IEEE big endian.

static float_format get_float_format ()
{
  float_params fp[3];

  INIT_FLT_PAR (fp[0], 1,
                0,    1048576,
                -1, 2146435071,
                0, 1017118720,
                0, 1018167296);

  INIT_FLT_PAR (fp[1], 2,
                1048576,  0,
                2146435071, -1,
                1017118720,  0,
                1018167296,  0);

  INIT_FLT_PAR (fp[2], 0,
                0, 0,
                0, 0,
                0, 0,
                0, 0);

  equiv mach_fp_par[4];

  F77_INT opt;

  opt = 1;
  mach_fp_par[0].d = F77_FUNC (d1mach, D1MACH) (&opt);

  opt = 2;
  mach_fp_par[1].d = F77_FUNC (d1mach, D1MACH) (&opt);

  opt = 3;
  mach_fp_par[2].d = F77_FUNC (d1mach, D1MACH) (&opt);

  opt = 4;
  mach_fp_par[3].d = F77_FUNC (d1mach, D1MACH) (&opt);

  int float_params_id = 0;
  int i = 0;
  do
    {
      if (equiv_compare (fp[i].fp_par, mach_fp_par, 4))
        {
          float_params_id = fp[i].fp_fmt;
          break;
        }
    }
  while (fp[++i].fp_fmt != 0);

  switch (float_params_id)
    {
    case 1:
      return flt_fmt_ieee_little_endian;

    case 2:
      return flt_fmt_ieee_big_endian;

    default:
      return flt_fmt_unknown;
    }
}

static int is_big_endian ()
{
  // Are we little or big endian?  From Harbison & Steele.

  union
  {
    long l;
    char c[sizeof (long)];
  } u;

  u.l = 1;

  return (u.c[sizeof (long) - 1] == 1);
}

float_format
native_float_format ()
{
  static float_format fmt = get_float_format ();

  return fmt;
}

bool
words_big_endian ()
{
  static bool big_endian = is_big_endian ();

  return big_endian;
}

bool
words_little_endian ()
{
  static bool little_endian = ! is_big_endian ();

  return little_endian;
}

bool
nan_with_payload ()
{
#if defined (HAVE_QNAN_WITH_PAYLOAD)
  return 1;
#else
  return 0;
#endif
}

float_format
string_to_float_format (const std::string& s)
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

std::string
float_format_as_string (float_format flt_fmt)
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
