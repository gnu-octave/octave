/*

Copyright (C) 1996, 1997 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cfloat>
#include <cmath>

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

#include "lo-ieee.h"
#include "mach-info.h"

void
octave_ieee_init (void)
{
  oct_mach_info::float_format ff = oct_mach_info::native_float_format ();

  if (ff == octave_mach_info::flt_fmt_vax_d
      || ff == octave_mach_info::flt_fmt_vax_g
      || ff == octave_mach_info::flt_fmt_cray)
    {
      octave_Inf = octave_NaN = octaveNA = DBL_MAX;
    }
  else
    {
#if defined (HAVE_ISINF) || defined (HAVE_FINITE)

#if defined (SCO)
      double tmp = 1.0;
      octave_Inf = 1.0 / (tmp - tmp);
#elif defined (__alpha__) && defined (__osf__)
      extern unsigned int DINFINITY[2];
      octave_Inf =  (*(X_CAST(double *, DINFINITY)));
#else
      double tmp = 1e+10;
      octave_Inf = tmp;
      for (;;)
	{
	  octave_Inf *= 1e+10;
	  if (octave_Inf == tmp)
	    break;
	  tmp = octave_Inf;
	}
#endif

#endif

#if defined (HAVE_ISNAN)

#if defined (__alpha__) && defined (__osf__)
      extern unsigned int DQNAN[2];
      octave_NaN = (*(X_CAST(double *, DQNAN)));
#else
      octave_NaN = octave_Inf / octave_Inf;
#endif

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

#endif
    }
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
