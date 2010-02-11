/*

Copyright (C) 1996, 1997, 1998, 1999, 2000, 2002, 2003, 2004, 2005,
              2007, 2008, 2009 John W. Eaton

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

#include <cfloat>
#include <cstdlib>

#if defined (HAVE_FLOATINGPOINT_H)
#include <floatingpoint.h>
#endif

#if defined (HAVE_IEEEFP_H)
#include <ieeefp.h>
#endif

#if defined (HAVE_NAN_H)
#if defined (SCO)
#define _IEEE 1
#endif
#include <nan.h>
#if defined (SCO)
#undef _IEEE
#endif
#endif

#include "lo-error.h"
#include "lo-ieee.h"
#include "lo-math.h"
#include "mach-info.h"

void
octave_ieee_init (void)
{
  // Default values.  DBL_MAX is not right for NaN and NA, but do you
  // have a better suggestion?  If you don't have IEEE floating point
  // values, there are many parts of Octave that will not work
  // correctly.

  octave_Inf = octave_NaN = octave_NA = DBL_MAX;
  octave_Float_Inf = octave_Float_NaN = octave_Float_NA = FLT_MAX;

  oct_mach_info::float_format ff = oct_mach_info::native_float_format ();

  switch (ff)
    {
    case oct_mach_info::flt_fmt_ieee_big_endian:
    case oct_mach_info::flt_fmt_ieee_little_endian:
      {
        // Don't optimize away tmp_inf / tmp_inf to generate octave_NaN.

        volatile double tmp_inf;

#if defined (SCO)
        volatile double tmp = 1.0;
        tmp_inf = 1.0 / (tmp - tmp);
#elif defined (__alpha__) && defined (__osf__)
        extern unsigned int DINFINITY[2];
        tmp_inf =  (*(X_CAST(double *, DINFINITY)));
#else
        double tmp = 1e+10;
        tmp_inf = tmp;
        for (;;)
          {
            tmp_inf *= 1e+10;
            if (tmp_inf == tmp)
              break;
            tmp = tmp_inf;
          }
#endif

#if defined (__alpha__) && defined (__osf__)
        extern unsigned int DQNAN[2];
        octave_NaN = (*(X_CAST(double *, DQNAN)));
#elif defined (__NetBSD__)
        octave_NaN = nan ("");
#else
        octave_NaN = tmp_inf / tmp_inf;
        // try to ensure that lo_ieee_sign gives false for a NaN.
        if (lo_ieee_signbit (octave_NaN))
          octave_NaN = -octave_NaN;

#endif

        octave_Inf = tmp_inf;

        // This is patterned after code in R.

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

        volatile float float_tmp_inf;

#if defined (SCO)
        volatile float float_tmp = 1.0;
        float_tmp_inf = 1.0 / (float_tmp - float_tmp);
#else
        float float_tmp = 1e+10;
        float_tmp_inf = float_tmp;
        for (;;)
          {
            float_tmp_inf *= 1e+10;
            if (float_tmp_inf == float_tmp)
              break;
            float_tmp = float_tmp_inf;
          }
#endif

#if defined (__NetBSD__)
        octave_Float_NaN = nanf ("");
#else
        octave_Float_NaN = float_tmp_inf / float_tmp_inf;
#endif
        octave_Float_Inf = float_tmp_inf;

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
