/*

Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
              2005, 2006, 2007, 2008, 2009 John W. Eaton

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

#include "lo-error.h"
#include "lo-ieee.h"
#include "lo-mappers.h"
#include "lo-math.h"
#include "lo-specfun.h"
#include "lo-utils.h"
#include "oct-cmplx.h"

#include "f77-fcn.h"

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
  if (x >= 0)
    {
      double y = floor (x);

      if ((x - y) >= 0.5)
	y += 1.0;

      return y;
    }
  else
    {
      double y = ceil (x);

      if ((y - x) >= 0.5)
	y -= 1.0;

      return y;
    }
#endif
}

double
xtrunc (double x)
{
#if defined (HAVE_TRUNC)
  return trunc (x);
#else
  return x > 0 ? floor (x) : ceil (x);
#endif
}

double 
xroundb (double x)
{
  double t = xround (x);

  if (fabs (x - t) == 0.5)
    t = 2 * xtrunc (0.5 * t);

  return t;
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

double
xlog2 (double x)
{
#if defined (HAVE_LOG2)
  return log2 (x);
#else
#if defined (M_LN2)
  static double ln2 = M_LN2;
#else
  static double ln2 = log (2);
#endif

  return log (x) / ln2;
#endif
}

Complex
xlog2 (const Complex& x)
{
#if defined (M_LN2)
  static double ln2 = M_LN2;
#else
  static double ln2 = log (2);
#endif

  return std::log (x) / ln2;
}

double
xexp2 (double x)
{
#if defined (HAVE_EXP2)
  return exp2 (x);
#else
#if defined (M_LN2)
  static double ln2 = M_LN2;
#else
  static double ln2 = log (2);
#endif

  return exp (x * ln2);
#endif
}

double
xlog2 (double x, int& exp)
{
  return frexp (x, &exp);
}

Complex
xlog2 (const Complex& x, int& exp)
{
  double ax = std::abs (x);
  double lax = xlog2 (ax, exp);
  return (exp == 0) ? x : (x / ax) * lax;
}

// double -> bool mappers.

#if ! defined(HAVE_CMATH_ISNAN)
bool
xisnan (double x)
{
  return lo_ieee_isnan (x);
}
#endif

#if ! defined(HAVE_CMATH_ISFINITE)
bool
xfinite (double x)
{
  return lo_ieee_finite (x);
}
#endif

#if ! defined(HAVE_CMATH_ISINF)
bool
xisinf (double x)
{
  return lo_ieee_isinf (x);
}
#endif

bool
octave_is_NA (double x)
{
  return lo_ieee_is_NA (x);
}

bool
octave_is_NaN_or_NA (double x)
{
  return lo_ieee_isnan (x);
}

// (double, double) -> double mappers.

// According to Matlab, is both args are NaN, the first one is returned.

double
xmin (double x, double y)
{
  return  xisnan (y) ? x : (x <= y ? x : y);
}

double
xmax (double x, double y)
{
  return  xisnan (y) ? x : (x >= y ? x : y);
}

// complex -> complex mappers.

Complex
acos (const Complex& x)
{
  static Complex i (0, 1);

  return -i * (log (x + i * (sqrt (1.0 - x*x))));
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
xroundb (const Complex& x)
{
  return Complex (xroundb (real (x)), xroundb (imag (x)));
}

Complex
signum (const Complex& x)
{
  double tmp = abs (x);

  return tmp == 0 ? 0.0 : x / tmp;
}

// complex -> bool mappers.

bool
octave_is_NA (const Complex& x)
{
  return (octave_is_NA (real (x)) || octave_is_NA (imag (x)));
}

bool
octave_is_NaN_or_NA (const Complex& x)
{
  return (xisnan (real (x)) || xisnan (imag (x)));
}

// (complex, complex) -> complex mappers.

// FIXME -- need to handle NA too?

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


// float -> float mappers.

float
arg (float x)
{
  return atan2 (0.0f, x);
}

float
conj (float x)
{
  return x;
}

float
fix (float x)
{
  return x > 0 ? floor (x) : ceil (x);
}

float
imag (float)
{
  return 0.0;
}

float
real (float x)
{
  return x;
}

float
xround (float x)
{
#if defined (HAVE_ROUND)
  return round (x);
#else
  if (x >= 0)
    {
      float y = floor (x);

      if ((x - y) >= 0.5)
	y += 1.0;

      return y;
    }
  else
    {
      float y = ceil (x);

      if ((y - x) >= 0.5)
	y -= 1.0;

      return y;
    }
#endif
}

float
xtrunc (float x)
{
#if defined (HAVE_TRUNC)
  return trunc (x);
#else
  return x > 0 ? floor (x) : ceil (x);
#endif
}

float 
xroundb (float x)
{
  float t = xround (x);

  if (fabs (x - t) == 0.5)
    t = 2 * xtrunc (0.5 * t);

  return t;
}

float
signum (float x)
{
  float tmp = 0.0;

  if (x < 0.0)
    tmp = -1.0;
  else if (x > 0.0)
    tmp = 1.0;

  return xisnan (x) ? octave_Float_NaN : tmp;
}

float
xlog2 (float x)
{
#if defined (HAVE_LOG2)
  return log2 (x);
#else
#if defined (M_LN2)
  static float ln2 = M_LN2;
#else
  static float ln2 = log2 (2);
#endif

  return log (x) / ln2;
#endif
}

FloatComplex
xlog2 (const FloatComplex& x)
{
#if defined (M_LN2)
  static float ln2 = M_LN2;
#else
  static float ln2 = log (2);
#endif

  return std::log (x) / ln2;
}

float
xexp2 (float x)
{
#if defined (HAVE_EXP2)
  return exp2 (x);
#else
#if defined (M_LN2)
  static float ln2 = M_LN2;
#else
  static float ln2 = log2 (2);
#endif

  return exp (x * ln2);
#endif
}

float
xlog2 (float x, int& exp)
{
  return frexpf (x, &exp);
}

FloatComplex
xlog2 (const FloatComplex& x, int& exp)
{
  float ax = std::abs (x);
  float lax = xlog2 (ax, exp);
  return (exp == 0) ? x : (x / ax) * lax;
}

// float -> bool mappers.

#if ! defined(HAVE_CMATH_ISNANF)
bool
xisnan (float x)
{
  return lo_ieee_isnan (x);
}
#endif

#if ! defined(HAVE_CMATH_ISFINITEF)
bool
xfinite (float x)
{
  return lo_ieee_finite (x);
}
#endif

#if ! defined(HAVE_CMATH_ISINFF)
bool
xisinf (float x)
{
  return lo_ieee_isinf (x);
}
#endif

bool
octave_is_NA (float x)
{
  return lo_ieee_is_NA (x);
}

bool
octave_is_NaN_or_NA (float x)
{
  return lo_ieee_isnan (x);
}

// (float, float) -> float mappers.

// FIXME -- need to handle NA too?

float
xmin (float x, float y)
{
  return  xisnan (y) ? x : (x <= y ? x : y);
}

float
xmax (float x, float y)
{
  return  xisnan (y) ? x : (x >= y ? x : y);
}

// complex -> complex mappers.

FloatComplex
acos (const FloatComplex& x)
{
  static FloatComplex i (0, 1);

  return -i * (log (x + i * (sqrt (static_cast<float>(1.0) - x*x))));
}

FloatComplex
acosh (const FloatComplex& x)
{
  return log (x + sqrt (x*x - static_cast<float>(1.0)));
}

FloatComplex
asin (const FloatComplex& x)
{
  static FloatComplex i (0, 1);

  return -i * log (i*x + sqrt (static_cast<float>(1.0) - x*x));
}

FloatComplex
asinh (const FloatComplex& x)
{
  return log (x + sqrt (x*x + static_cast<float>(1.0)));
}

FloatComplex
atan (const FloatComplex& x)
{
  static FloatComplex i (0, 1);

  return i * log ((i + x) / (i - x)) / static_cast<float>(2.0);
}

FloatComplex
atanh (const FloatComplex& x)
{
  return log ((static_cast<float>(1.0) + x) / (static_cast<float>(1.0) - x)) / static_cast<float>(2.0);
}

FloatComplex
ceil (const FloatComplex& x)
{
  return FloatComplex (ceil (real (x)), ceil (imag (x)));
}

FloatComplex
fix (const FloatComplex& x)
{
  return FloatComplex (fix (real (x)), fix (imag (x)));
}

FloatComplex
floor (const FloatComplex& x)
{
  return FloatComplex (floor (real (x)), floor (imag (x)));
}

FloatComplex
xround (const FloatComplex& x)
{
  return FloatComplex (xround (real (x)), xround (imag (x)));
}

FloatComplex
xroundb (const FloatComplex& x)
{
  return FloatComplex (xroundb (real (x)), xroundb (imag (x)));
}

FloatComplex
signum (const FloatComplex& x)
{
  float tmp = abs (x);

  return tmp == 0 ? 0.0 : x / tmp;
}

// complex -> bool mappers.

bool
octave_is_NA (const FloatComplex& x)
{
  return (octave_is_NA (real (x)) || octave_is_NA (imag (x)));
}

bool
octave_is_NaN_or_NA (const FloatComplex& x)
{
  return (xisnan (real (x)) || xisnan (imag (x)));
}

// (complex, complex) -> complex mappers.

// FIXME -- need to handle NA too?

FloatComplex
xmin (const FloatComplex& x, const FloatComplex& y)
{
  return abs (x) <= abs (y) ? x : (xisnan (x) ? x : y);
}

FloatComplex
xmax (const FloatComplex& x, const FloatComplex& y)
{
  return abs (x) >= abs (y) ? x : (xisnan (x) ? x : y);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
