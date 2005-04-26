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
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

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

#include "lo-error.h"
#include "lo-ieee.h"
#include "mach-info.h"

void
octave_ieee_init (void)
{
  // Default values.  DBL_MAX is not right for NaN and NA, but do you
  // have a better suggestion?  If you don't have IEEE floating point
  // values, there are many parts of Octave that will not work
  // correctly.

  octave_Inf = octave_NaN = octave_NA = DBL_MAX;

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
#else
	octave_NaN = tmp_inf / tmp_inf;
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
      }
      break;

    case oct_mach_info::flt_fmt_cray:
    case oct_mach_info::flt_fmt_vax_d:
    case oct_mach_info::flt_fmt_vax_g:
      break;

    default:
      // If the format is unknown, then you will probably not have a
      // useful system, but we will just issue a warning and go on...
      (*current_liboctave_warning_handler)
	("lo_ieee_init: unrecognized floating point format!");
    }
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
