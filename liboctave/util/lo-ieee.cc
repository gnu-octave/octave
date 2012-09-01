/*

Copyright (C) 1996-2012 John W. Eaton

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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cstdlib>

#include <limits>

#include "lo-error.h"
#include "lo-ieee.h"
#include "mach-info.h"

void
octave_ieee_init (void)
{
  oct_mach_info::float_format ff = oct_mach_info::native_float_format ();

  switch (ff)
    {
    case oct_mach_info::flt_fmt_ieee_big_endian:
    case oct_mach_info::flt_fmt_ieee_little_endian:
      {
        octave_NaN = std::numeric_limits<double>::quiet_NaN ();
        octave_Inf = std::numeric_limits<double>::infinity ();

        octave_Float_NaN = std::numeric_limits<float>::quiet_NaN ();
        octave_Float_Inf = std::numeric_limits<float>::infinity ();

        // The following is patterned after code in R.

        if (ff == oct_mach_info::flt_fmt_ieee_big_endian)
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

        octave_NA = t.value;

        lo_ieee_float tf;
        tf.word = LO_IEEE_NA_FLOAT;
        octave_Float_NA = tf.value;
      }
      break;

    case oct_mach_info::flt_fmt_cray:
    case oct_mach_info::flt_fmt_vax_d:
    case oct_mach_info::flt_fmt_vax_g:
    default:
      // If the format is unknown, then you will probably not have a
      // useful system, so we will abort here.  Anyone wishing to
      // experiment with building Octave on a system without IEEE
      // floating point should be capable of removing this check and
      // the configure test.
      (*current_liboctave_error_handler)
        ("lo_ieee_init: floating point format is not IEEE!  Maybe DLAMCH is miscompiled, or you are using some strange system without IEEE floating point math?");
      abort ();
    }
}
