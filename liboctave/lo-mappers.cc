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

#if defined (HAVE_SUNMATH_H)
#include <sunmath.h>
#endif

#include "lo-error.h"
#include "lo-ieee.h"
#include "lo-mappers.h"
#include "lo-specfun.h"
#include "lo-utils.h"
#include "oct-cmplx.h"

#include "f77-fcn.h"

#ifndef M_LOG10E
#define M_LOG10E 0.43429448190325182765
#endif

// double -> double mappers.

double
arg (double x)
{
  return atan2 (0.0, x);
}

double
conj (double x)
{
  return x;
}

double
fix (double x)
{
  return x > 0 ? floor (x) : ceil (x);
}

double
imag (double)
{
  return 0.0;
}

double
real (double x)
{
  return x;
}

double
xround (double x)
{
#if defined (HAVE_ROUND)
  return round (x);
#else
  return x > 0 ? floor (x + 0.5) : ceil (x - 0.5);
#endif
}

double
signum (double x)
{
  double tmp = 0.0;

  if (x < 0.0)
    tmp = -1.0;
  else if (x > 0.0)
    tmp = 1.0;

  return xisnan (x) ? octave_NaN : tmp;
}

// double -> bool mappers.

bool
xisnan (double x)
{
  return lo_ieee_isnan (x);
}

bool
xfinite (double x)
{
  return lo_ieee_finite (x);
}

bool
xisinf (double x)
{
  return lo_ieee_isinf (x);
}

bool
octave_is_NA (double x)
{
  return lo_ieee_is_NA (x);
}

bool
octave_is_NaN_or_NA (double x)
{
  return lo_ieee_is_NaN_or_NA (x);
}

// (double, double) -> double mappers.

// XXX FIXME XXX -- need to handle NA too?

double
xmin (double x, double y)
{
  if (x < y)
    return x;

  if (y <= x)
    return y;

  if (octave_is_NaN_or_NA (x) && ! octave_is_NaN_or_NA (y))
    return y;
  else if (octave_is_NaN_or_NA (y) && ! octave_is_NaN_or_NA (x))
    return x;
  else if (octave_is_NA (x) || octave_is_NA (y))
    return octave_NA;
  else
    return octave_NaN;
}

double
xmax (double x, double y)
{
  if (x > y)
    return x;

  if (y >= x)
    return y;

  if (octave_is_NaN_or_NA (x) && ! octave_is_NaN_or_NA (y))
    return y;
  else if (octave_is_NaN_or_NA (y) && ! octave_is_NaN_or_NA (x))
    return x;
  else if (octave_is_NA (x) || octave_is_NA (y))
    return octave_NA;
  else
    return octave_NaN;
}

// complex -> complex mappers.

Complex
acos (const Complex& x)
{
  static Complex i (0, 1);

  return (real (x) * imag (x) < 0.0) ? i * acosh (x) : -i * acosh (x);
}

Complex
acosh (const Complex& x)
{
  return log (x + sqrt (x*x - 1.0));
}

Complex
asin (const Complex& x)
{
  static Complex i (0, 1);

  return -i * log (i*x + sqrt (1.0 - x*x));
}

Complex
asinh (const Complex& x)
{
  return log (x + sqrt (x*x + 1.0));
}

Complex
atan (const Complex& x)
{
  static Complex i (0, 1);

  return i * log ((i + x) / (i - x)) / 2.0;
}

Complex
atanh (const Complex& x)
{
  return log ((1.0 + x) / (1.0 - x)) / 2.0;
}

Complex
ceil (const Complex& x)
{
  return Complex (ceil (real (x)), ceil (imag (x)));
}

Complex
fix (const Complex& x)
{
  return Complex (fix (real (x)), fix (imag (x)));
}

Complex
floor (const Complex& x)
{
  return Complex (floor (real (x)), floor (imag (x)));
}

Complex
xround (const Complex& x)
{
  return Complex (xround (real (x)), xround (imag (x)));
}

Complex
signum (const Complex& x)
{
  return x / abs (x);
}

// complex -> bool mappers.

bool
xisnan (const Complex& x)
{
  return (xisnan (real (x)) || xisnan (imag (x)));
}

bool
xfinite (const Complex& x)
{
  double rx = real (x);
  double ix = imag (x);

  return (xfinite (rx) && ! octave_is_NaN_or_NA (rx)
	  && xfinite (ix) && ! octave_is_NaN_or_NA (ix));
}

bool
xisinf (const Complex& x)
{
  return (xisinf (real (x)) || xisinf (imag (x)));
}

bool
octave_is_NA (const Complex& x)
{
  return (octave_is_NA (real (x)) || octave_is_NA (imag (x)));
}

bool
octave_is_NaN_or_NA (const Complex& x)
{
  return (octave_is_NaN_or_NA (real (x)) || octave_is_NaN_or_NA (imag (x)));
}

// (complex, complex) -> complex mappers.

// XXX FIXME XXX -- need to handle NA too?

Complex
xmin (const Complex& x, const Complex& y)
{
  return abs (x) <= abs (y) ? x : (xisnan (x) ? x : y);
}

Complex
xmax (const Complex& x, const Complex& y)
{
  return abs (x) >= abs (y) ? x : (xisnan (x) ? x : y);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
