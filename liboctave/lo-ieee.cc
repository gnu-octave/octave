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

// Octave's idea of infinity.
double octave_Inf;

// Octave's idea of not a number.
double octave_NaN;

void
octave_ieee_init (void)
{
#if defined (HAVE_ISINF) || defined (HAVE_FINITE)

// Some version of gcc on some old version of Linux used to crash when
// trying to make Inf and NaN.

#if defined (SCO)
  double tmp = 1.0;
  octave_Inf = 1.0 / (tmp - tmp);
#elif defined (__alpha__) && ! defined (linux)
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

#if defined (__alpha__) && ! defined (linux)
  extern unsigned int DQNAN[2];
  octave_NaN = (*(X_CAST(double *, DQNAN)));
#else
  octave_NaN = octave_Inf / octave_Inf;
#endif

#endif
}

#if defined (SCO)

extern "C" int
isnan (double x)
{
  return (IsNANorINF (x) && NaN (x) && ! IsINF (x)) ? 1 : 0;
}

extern "C" int
isinf (double x)
{
  return (IsNANorINF (x) && IsINF (x)) ? 1 : 0;
}

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
