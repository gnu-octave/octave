/*

Copyright (C) 1996 John W. Eaton

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

#include "lo-error.h"
#include "lo-ieee.h"
#include "lo-mappers.h"
#include "lo-utils.h"
#include "oct-cmplx.h"
#include "oct-math.h"

#include "f77-fcn.h"

#if defined (_AIX) && defined (__GNUG__)
#undef finite
#define finite(x) ((x) < DBL_MAX && (x) > -DBL_MAX)
#endif

extern "C"
{
  double F77_FCN (dgamma, DGAMMA) (const double&);

  int F77_FCN (dlgams, DLGAMS) (const double&, double&, double&);
}

#ifndef M_LOG10E
#define M_LOG10E 0.43429448190325182765
#endif

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

#if defined (HAVE_LGAMMA) && ! defined (SIGNGAM_DECLARED)
extern int signgam;
#endif

// Double -> double mappers.

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
  return static_cast<int> (x);
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

double
xisnan (double x)
{
#if defined (HAVE_ISNAN)
  return isnan (x);
#else
  return 0;
#endif
}

double
xfinite (double x)
{
#if defined (HAVE_FINITE)
  return finite (x);
#elif defined (HAVE_ISINF) && defined (HAVE_ISNAN)
  return (! isinf (x) && ! isnan (x));
#else
  return 1;
#endif
}

double
xgamma (double x)
{
  return F77_FCN (dgamma, DGAMMA) (x);
}

double
xisinf (double x)
{
#if defined (HAVE_ISINF)
  return isinf (x);
#elif defined (HAVE_FINITE) && defined (HAVE_ISNAN)
  return (! (finite (x) || isnan (x)));
#else
  return 0;
#endif
}

double
xlgamma (double x)
{
  double result;
  double sgngam;

  F77_FCN (dlgams, DLGAMS) (x, result, sgngam);

  return result;
}

// Complex -> double mappers.

double
xisnan (const Complex& x)
{
#if defined (HAVE_ISNAN)
  return (isnan (real (x)) || isnan (imag (x)));
#else
  return 0;
#endif
}

double
xfinite (const Complex& x)
{
  return (! (xisinf (real (x)) || xisinf (imag (x))));
}

double
xisinf (const Complex& x)
{
  return (! xfinite (x));
}

// Complex -> complex mappers.

Complex
acos (const Complex& x)
{
  static Complex i (0, 1);
  Complex retval = -i * log (x + sqrt (x*x - 1.0));
  return retval;
}

Complex
acosh (const Complex& x)
{
  Complex retval = log (x + sqrt (x*x - 1.0));
  return retval;
}

Complex
asin (const Complex& x)
{
  static Complex i (0, 1);
  Complex retval = -i * log (i*x + sqrt (1.0 - x*x));
  return retval;
}

Complex
asinh (const Complex& x)
{
  Complex retval = log (x + sqrt (x*x + 1.0));
  return retval;
}

Complex
atan (const Complex& x)
{
  static Complex i (0, 1);
  Complex retval = i * log ((i + x) / (i - x)) / 2.0;
  return retval;
}

Complex
atanh (const Complex& x)
{
  Complex retval = log ((1 + x) / (1 - x)) / 2.0;
  return retval;
}

Complex
ceil (const Complex& x)
{
  return Complex (ceil (real (x)), ceil (imag (x)));
}

Complex
fix (const Complex& x)
{
  return Complex (static_cast<int> (real (x)),
		  static_cast<int> (imag (x)));
}

Complex
floor (const Complex& x)
{
  return Complex (floor (real (x)), floor (imag (x)));
}

Complex
log10 (const Complex& x)
{
  return M_LOG10E * log (x);
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
tan (const Complex& x)
{
  Complex retval = sin (x) / cos (x);
  return retval;
}

Complex
tanh (const Complex& x)
{
  Complex retval = sinh (x) / cosh (x);
  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
