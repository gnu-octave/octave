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

#if defined (HAVE_IEEEFP_H)
#include <ieeefp.h>
#endif

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

#if defined (_AIX) && defined (__GNUG__)
#undef finite
#define finite(x) ((x) < DBL_MAX && (x) > -DBL_MAX)
#endif

#ifndef M_LOG10E
#define M_LOG10E 0.43429448190325182765
#endif

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

// double -> double mappers.

double
arg (double x)
{
  if (x < 0.0)
    return M_PI;
  else
#if defined (HAVE_ISNAN)
    return xisnan (x) ? octave_NaN : 0.0;
#else
    return 0.0;
#endif
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
imag (double x)
{
#if defined (HAVE_ISNAN)
  return xisnan (x) ? octave_NaN : 0.0;
#else
  return 0.0;
#endif
}

double
real (double x)
{
  return x;
}

double
round (double x)
{
  return D_NINT (x);
}

double
signum (double x)
{
  double tmp = 0.0;
  if (x < 0.0)
    tmp = -1.0;
  else if (x > 0.0)
    tmp = 1.0;

#if defined (HAVE_ISNAN)
  return xisnan (x) ? octave_NaN : tmp;
#else
  return tmp;
#endif
}

double
xerf (double x)
{
#if defined (HAVE_ERF)
  return erf (x);
#else
  (*current_liboctave_error_handler)
    ("erf (x) not available on this system");
#endif
}

double
xerfc (double x)
{
#if defined (HAVE_ERFC)
  return erfc (x);
#else
  (*current_liboctave_error_handler)
    ("erfc (x) not available on this system");
#endif
}

// double -> bool mappers.

bool
xisnan (double x)
{
#if defined (HAVE_ISNAN)
  return isnan (x) != 0;
#else
  return false;
#endif
}

bool
xfinite (double x)
{
#if defined (HAVE_FINITE)
  return finite (x) != 0;
#elif defined (HAVE_ISINF) && defined (HAVE_ISNAN)
  return (! isinf (x) && ! isnan (x));
#else
  return true;
#endif
}

bool
xisinf (double x)
{
#if defined (HAVE_ISINF)
  return isinf (x);
#elif defined (HAVE_FINITE) && defined (HAVE_ISNAN)
  return (! (finite (x) || isnan (x)));
#else
  return false;
#endif
}

// (double, double) -> double mappers.

double
xmin (double x, double y)
{
  return x < y ? x : (xisnan (x) ? x : y);
}

double
xmax (double x, double y)
{
  return x > y ? x : (xisnan (x) ? x : y);
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
round (const Complex& x)
{
  return Complex (D_NINT (real (x)), D_NINT (imag (x)));
}

Complex
signum (const Complex& x)
{
  return x / abs (x);
}

Complex
tanh (const Complex& x)
{
  return sinh (x) / cosh (x);
}

// complex -> bool mappers.

bool
xisnan (const Complex& x)
{
#if defined (HAVE_ISNAN)
  return (isnan (real (x)) || isnan (imag (x)));
#else
  return false;
#endif
}

bool
xfinite (const Complex& x)
{
  return (xfinite (real (x)) && xfinite (imag (x)));
}

bool
xisinf (const Complex& x)
{
  return (xisinf (real (x)) || xisinf (imag (x)));
}

// (complex, complex) -> complex mappers.

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
