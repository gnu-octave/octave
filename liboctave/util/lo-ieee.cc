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

#include <cmath>
#include <cstdlib>

#include <limits>

#include "lo-error.h"
#include "lo-ieee.h"
#include "mach-info.h"

static double lo_inf;
static double lo_nan;
static double lo_na;

static float lo_float_inf;
static float lo_float_nan;
static float lo_float_na;

static int lo_ieee_hw;
static int lo_ieee_lw;

#if defined (HAVE_MIPS_NAN)
  #define LO_IEEE_NA_HW 0x7FF040F4
#else
  #define LO_IEEE_NA_HW 0x7FF840F4
#endif
#define LO_IEEE_NA_LW 0x40000000
#define LO_IEEE_NA_FLOAT 0x7FC207A2

int
__lo_ieee_is_NA (double x)
{
  lo_ieee_double t;
  t.value = x;
  return (__lo_ieee_isnan (x) && t.word[lo_ieee_hw] == LO_IEEE_NA_HW
          && t.word[lo_ieee_lw] == LO_IEEE_NA_LW) ? 1 : 0;
}

double
lo_ieee_inf_value (void)
{
  octave_ieee_init ();

  return lo_inf;
}

double
lo_ieee_na_value (void)
{
  octave_ieee_init ();

  return lo_na;
}

double
lo_ieee_nan_value (void)
{
  octave_ieee_init ();

  return lo_nan;
}

int
__lo_ieee_float_is_NA (float x)
{
  lo_ieee_float t;
  t.value = x;
  return (__lo_ieee_float_isnan (x) && (t.word == LO_IEEE_NA_FLOAT)) ? 1 : 0;
}

float
lo_ieee_float_inf_value (void)
{
  octave_ieee_init ();

  return lo_float_inf;
}

float
lo_ieee_float_na_value (void)
{
  octave_ieee_init ();

  return lo_float_na;
}

float
lo_ieee_float_nan_value (void)
{
  octave_ieee_init ();

  return lo_float_nan;
}

void
octave_ieee_init (void)
{
  static bool initialized = false;

  if (! initialized)
    {
      octave::mach_info::float_format ff = octave::mach_info::native_float_format ();

      switch (ff)
        {
        case octave::mach_info::flt_fmt_ieee_big_endian:
        case octave::mach_info::flt_fmt_ieee_little_endian:
          {
            lo_nan = std::numeric_limits<double>::quiet_NaN ();
            lo_inf = std::numeric_limits<double>::infinity ();

            lo_float_nan = std::numeric_limits<float>::quiet_NaN ();
            lo_float_inf = std::numeric_limits<float>::infinity ();

            // The following is patterned after code in R.

            if (ff == octave::mach_info::flt_fmt_ieee_big_endian)
              {
                lo_ieee_hw = 0;
                lo_ieee_lw = 1;
              }
            else
              {
                lo_ieee_hw = 1;
                lo_ieee_lw = 0;
              }

            lo_ieee_double t;
            t.word[lo_ieee_hw] = LO_IEEE_NA_HW;
            t.word[lo_ieee_lw] = LO_IEEE_NA_LW;

            lo_na = t.value;

            lo_ieee_float tf;
            tf.word = LO_IEEE_NA_FLOAT;

            lo_float_na = tf.value;
          }
          break;

        default:
          // If the format is unknown, then you will probably not have a
          // useful system, so we will abort here.  Anyone wishing to
          // experiment with building Octave on a system without IEEE
          // floating point should be capable of removing this check and
          // the configure test.
          //
          // If the error handler returns, then we'll abort.

          (*current_liboctave_error_handler)
            ("lo_ieee_init: floating point format is not IEEE!  Maybe DLAMCH is miscompiled, or you are using some strange system without IEEE floating point math?");

          abort ();
        }

      initialized = true;
    }
}
