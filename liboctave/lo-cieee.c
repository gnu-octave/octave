/*

Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007, 2008 John W. Eaton

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

#include <float.h>

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
#include "lo-math.h"

#if ! defined (HAVE_ISNAN) && defined (HAVE__ISNAN)
#define isnan _isnan
#define HAVE_ISNAN 1
#endif

#if ! defined (HAVE_FINITE) && defined (HAVE__FINITE)
#define finite _finite
#define HAVE_FINITE 1
#endif

#if ! defined (HAVE_COPYSIGN) && defined (HAVE__COPYSIGN)
#define copysign _copysign
#define HAVE_COPYSIGN 1
#endif

#if defined (_AIX) && defined (__GNUG__)
#undef finite
#define finite(x) ((x) < DBL_MAX && (x) > -DBL_MAX)
#endif

/* Octave's idea of infinity.  */
double octave_Inf;
float octave_Float_Inf;

/* Octave's idea of a missing value.  */
double octave_NA;
float octave_Float_NA;

/* Octave's idea of not a number.  */
double octave_NaN;
float octave_Float_NaN;

int lo_ieee_hw;
int lo_ieee_lw;

#if defined (SCO)

int
__isnan (double x)
{
  return (IsNANorINF (x) && NaN (x) && ! IsINF (x)) ? 1 : 0;
}

int
__isinf (double x)
{
  return (IsNANorINF (x) && IsINF (x)) ? 1 : 0;
}

int
__isnanf (float x)
{
  return (IsNANorINF (x) && NaN (x) && ! IsINF (x)) ? 1 : 0;
}

int
__isinff (float x)
{
  return (IsNANorINF (x) && IsINF (x)) ? 1 : 0;
}

#endif

int
__lo_ieee_isnan (double x)
{
#if defined (HAVE_ISNAN)
  return isnan (x);
#else
  return 0;
#endif
}

int
__lo_ieee_finite (double x)
{
#if defined (HAVE_FINITE)
  return finite (x) != 0 && ! __lo_ieee_isnan (x);
#elif defined (HAVE_ISINF)
  return (! isinf (x) && ! __lo_ieee_isnan (x));
#else
  return ! __lo_ieee_isnan (x);
#endif
}

int
__lo_ieee_isinf (double x)
{
#if defined (HAVE_ISINF)
  return isinf (x);
#elif defined (HAVE_FINITE)
  return (! (finite (x) || __lo_ieee_isnan (x)));
#else
  return 0;
#endif
}

int
__lo_ieee_is_NA (double x)
{
#if defined (HAVE_ISNAN)
  lo_ieee_double t;
  t.value = x;
  return (isnan (x) && t.word[lo_ieee_hw] == LO_IEEE_NA_HW 
	  && t.word[lo_ieee_lw] == LO_IEEE_NA_LW) ? 1 : 0;
#else
  return 0;
#endif
}

int
__lo_ieee_is_old_NA (double x)
{
#if defined (HAVE_ISNAN)
  lo_ieee_double t;
  t.value = x;
  return (isnan (x) && t.word[lo_ieee_lw] == LO_IEEE_NA_LW_OLD 
	  && t.word[lo_ieee_hw] == LO_IEEE_NA_HW_OLD) ? 1 : 0;
#else
  return 0;
#endif
}

double
__lo_ieee_replace_old_NA (double x)
{
  if (__lo_ieee_is_old_NA (x))
    return lo_ieee_na_value ();
  else
    return x;
}

int
__lo_ieee_is_NaN_or_NA (double x)
{
  return __lo_ieee_isnan (x);
}

double
lo_ieee_inf_value (void)
{
  return octave_Inf;
}

double
lo_ieee_na_value (void)
{
  return octave_NA;
}

double
lo_ieee_nan_value (void)
{
  return octave_NaN;
}

#if ! (defined (signbit) || defined (HAVE_DECL_SIGNBIT)) && defined (HAVE_SIGNBIT)
extern int signbit (double);
#endif

int
__lo_ieee_signbit (double x)
{
/* In the following definitions, only check x < 0 explicitly to avoid
   a function call when it looks like signbit or copysign are actually
   functions.  */

#if defined (signbit)
  return signbit (x);
#elif defined (HAVE_SIGNBIT)
  return (x < 0 || signbit (x));
#elif defined (copysign)
  return (copysign (1.0, x) < 0);
#elif defined (HAVE_COPYSIGN)
  return (x < 0 || copysign (1.0, x) < 0);
#else
  return x < 0;
#endif
}

int
__lo_ieee_float_isnan (float x)
{
#if defined (HAVE_ISNAN)
  return isnan (x);
#else
  return 0;
#endif
}

int
__lo_ieee_float_finite (float x)
{
#if defined (HAVE_FINITE)
  return finite (x) != 0 && ! __lo_ieee_float_isnan (x);
#elif defined (HAVE_ISINF)
  return (! isinf (x) && ! __lo_ieee_float_isnan (x));
#else
  return ! __lo_ieee_float_isnan (x);
#endif
}

int
__lo_ieee_float_isinf (float x)
{
#if defined (HAVE_ISINF)
  return isinf (x);
#elif defined (HAVE_FINITE)
  return (! (finite (x) || __lo_ieee_float_isnan (x)));
#else
  return 0;
#endif
}

int
__lo_ieee_float_is_NA (float x)
{
#if defined (HAVE_ISNAN)
  lo_ieee_float t;
  t.value = x;
  return (isnan (x) && (t.word == LO_IEEE_NA_FLOAT)) ? 1 : 0;
#else
  return 0;
#endif
}

int
__lo_ieee_float_is_NaN_or_NA (float x)
{
  return __lo_ieee_float_isnan (x);
}

float
lo_ieee_float_inf_value (void)
{
  return octave_Float_Inf;
}

float
lo_ieee_float_na_value (void)
{
  return octave_Float_NA;
}

float
lo_ieee_float_nan_value (void)
{
  return octave_Float_NaN;
}

#if ! (defined (signbit) || defined (HAVE_DECL_SIGNBIT)) && defined (HAVE_SIGNBIT)
extern int signbit (float);
#endif

int
__lo_ieee_float_signbit (float x)
{
/* In the following definitions, only check x < 0 explicitly to avoid
   a function call when it looks like signbit or copysign are actually
   functions.  */

#if defined (signbit)
  return signbit (x);
#elif defined (HAVE_SIGNBIT)
  return (x < 0 || signbit (x));
#elif defined (copysign)
  return (copysign (1.0, x) < 0);
#elif defined (HAVE_COPYSIGN)
  return (x < 0 || copysign (1.0, x) < 0);
#else
  return x < 0;
#endif
}
