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

#include <cctype>
#include <cfloat>

#include "oct-cmplx.h"
#include "oct-math.h"

#include "defun.h"
#include "error.h"
#include "f77-fcn.h"
#include "mappers.h"
#include "sysdep.h"
#include "utils.h"
#include "variables.h"

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
  int tmp;
  tmp = (int) x;
  return (double) tmp;
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
  error ("erf(x) not available on this system");
#endif
}

double
xerfc (double x)
{
#if defined (HAVE_ERFC)
  return erfc (x);
#else
  error ("erfc(x) not available on this system");
#endif
}

double
xisnan (double x)
{
#if defined (HAVE_ISNAN)
  return (double) isnan (x);
#else
  return 0;
#endif
}

double
xfinite (double x)
{
#if defined (HAVE_FINITE)
  return (double) finite (x);
#elif defined (HAVE_ISINF) && defined (HAVE_ISNAN)
  return (double) (! isinf (x) && ! isnan (x));
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
  return (double) isinf (x);
#elif defined (HAVE_FINITE) && defined (HAVE_ISNAN)
  return (double) (! (finite (x) || isnan (x)));
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
  double rx = real (x);
  double ix = imag (x);
  return (double) (isnan (rx) || isnan (ix));
#else
  return 0;
#endif
}

double
xfinite (const Complex& x)
{
  double rx = real (x);
  double ix = imag (x);
  return (double) (! ((int) xisinf (rx) || (int) xisinf (ix)));
}

double
xisinf (const Complex& x)
{
  return (double) (! (int) xfinite (x));
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
  static Complex i (0, 1);
  Complex retval = log ((1 + x) / (1 - x)) / 2.0;
  return retval;
}

Complex
ceil (const Complex& x)
{
  int re = (int) ceil (real (x));
  int im = (int) ceil (imag (x));
  return Complex (re, im);
}

Complex
fix (const Complex& x)
{
  int re = (int) real (x);
  int im = (int) imag (x);
  return Complex (re, im);
}

Complex
floor (const Complex& x)
{
  int re = (int) floor (real (x));
  int im = (int) floor (imag (x));
  return Complex (re, im);
}

Complex
log10 (const Complex& x)
{
  return M_LOG10E * log (x);
}

Complex
round (const Complex& x)
{
  double re = D_NINT (real (x));
  double im = D_NINT (imag (x));
  return Complex (re, im);
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

// XXX FIXME XXX -- perhaps this could be avoided by determining
// whether the is* functions are actually functions or just macros.

int
xisalnum (int c)
{
  return isalnum (c);
}

int
xisalpha (int c)
{
  return isalpha (c);
}

int
xisascii (int c)
{
  return isascii (c);
}

int
xiscntrl (int c)
{
  return iscntrl (c);
}

int
xisdigit (int c)
{
  return isdigit (c);
}

int
xisgraph (int c)
{
  return isgraph (c);
}

int
xislower (int c)
{
  return islower (c);
}

int
xisprint (int c)
{
  return isprint (c);
}

int
xispunct (int c)
{
  return ispunct (c);
}

int
xisspace (int c)
{
  return isspace (c);
}

int
xisupper (int c)
{
  return isupper (c);
}

int
xisxdigit (int c)
{
  return isxdigit (c);
}

void
install_mapper_functions (void)
{
  DEFUN_MAPPER (abs, 0, fabs, abs, 0, 0.0, 0.0, 0,
    "abs (X): compute abs (X) for each element of X");

  DEFUN_MAPPER (acos, 0, acos, 0, acos, -1.0, 1.0, 1,
    "acos (X): compute acos (X) for each element of X");

  DEFUN_MAPPER (acosh, 0, acosh, 0, acosh, 1.0, DBL_MAX, 1,
    "acosh (X): compute acosh (X) for each element of X");

  DEFUN_MAPPER (angle, 0, arg, arg, 0, 0.0, 0.0, 0,
    "angle (X): compute arg (X) for each element of X");

  DEFUN_MAPPER (arg, 0, arg, arg, 0, 0.0, 0.0, 0,
    "arg (X): compute arg (X) for each element of X");

  DEFUN_MAPPER (asin, 0, asin, 0, asin, -1.0, 1.0, 1,
    "asin (X): compute asin (X) for each element of X");

  DEFUN_MAPPER (asinh, 0, asinh, 0, asinh, 0.0, 0.0, 0,
    "asinh (X): compute asinh (X) for each element of X");

  DEFUN_MAPPER (atan, 0, atan, 0, atan, 0.0, 0.0, 0,
    "atan (X): compute atan (X) for each element of X");

  DEFUN_MAPPER (atanh, 0, atanh, 0, atanh, -1.0, 1.0, 1,
    "atanh (X): compute atanh (X) for each element of X");

  DEFUN_MAPPER (ceil, 0, ceil, 0, ceil, 0.0, 0.0, 0,
    "ceil (X): round elements of X toward +Inf");

  DEFUN_MAPPER (conj, 0, conj, 0, conj, 0.0, 0.0, 0,
    "conj (X): compute complex conjugate for each element of X");

  DEFUN_MAPPER (cos, 0, cos, 0, cos, 0.0, 0.0, 0,
    "cos (X): compute cos (X) for each element of X");

  DEFUN_MAPPER (cosh, 0, cosh, 0, cosh, 0.0, 0.0, 0,
    "cosh (X): compute cosh (X) for each element of X");

  DEFUN_MAPPER (erf, 0, xerf, 0, 0, 0.0, 0.0, 0,
    "erf (X): compute erf (X) for each element of X");

  DEFUN_MAPPER (erfc, 0, xerfc, 0, 0, 0.0, 0.0, 0,
    "erfc (X): compute erfc (X) for each element of X");

  DEFUN_MAPPER (exp, 0, exp, 0, exp, 0.0, 0.0, 0,
    "exp (X): compute exp (X) for each element of X");

  DEFUN_MAPPER (finite, 0, xfinite, xfinite, 0, 0.0, 0.0, 0,
    "finite (X): return 1 for finite elements of X");

  DEFUN_MAPPER (fix, 0, fix, 0, fix, 0.0, 0.0, 0,
    "fix (X): round elements of X toward zero");

  DEFUN_MAPPER (floor, 0, floor, 0, floor, 0.0, 0.0, 0,
    "floor (X): round elements of X toward -Inf");

  DEFUN_MAPPER (gamma, 0, xgamma, 0, 0, 0.0, 0.0, 0,
    "gamma (X): compute gamma (X) for each element of X");

  DEFUN_MAPPER (imag, 0, imag, imag, 0, 0.0, 0.0, 0,
    "imag (X): return imaginary part for each elements of X");

  DEFUN_MAPPER (isalnum, xisalnum, 0, 0, 0, 0.0, 0.0, 0,
    "isalnum (X): ");

  DEFUN_MAPPER (isalpha, xisalpha, 0, 0, 0, 0.0, 0.0, 0,
    "isalpha (X): ");

  DEFUN_MAPPER (isascii, xisascii, 0, 0, 0, 0.0, 0.0, 0,
    "isascii (X): ");

  DEFUN_MAPPER (iscntrl, xiscntrl, 0, 0, 0, 0.0, 0.0, 0,
    "iscntrl (X): ");

  DEFUN_MAPPER (isdigit, xisdigit, 0, 0, 0, 0.0, 0.0, 0,
    "isdigit (X): ");

  DEFUN_MAPPER (isinf, 0, xisinf, xisinf, 0, 0.0, 0.0, 0,
    "isinf (X): return 1 for elements of X infinite");

  DEFUN_MAPPER (isgraph, xisgraph, 0, 0, 0, 0.0, 0.0, 0,
    "isgraph (X): ");

  DEFUN_MAPPER (islower, xislower, 0, 0, 0, 0.0, 0.0, 0,
    "islower (X): ");

  DEFUN_MAPPER (isnan, 0, xisnan, xisnan, 0, 0.0, 0.0, 0,
    "isnan (X): return 1 where elements of X are NaNs");

  DEFUN_MAPPER (isprint, xisprint, 0, 0, 0, 0.0, 0.0, 0,
    "isprint (X): ");

  DEFUN_MAPPER (ispunct, xispunct, 0, 0, 0, 0.0, 0.0, 0,
    "ispunct (X): ");

  DEFUN_MAPPER (isspace, xisspace, 0, 0, 0, 0.0, 0.0, 0,
    "isspace (X): ");

  DEFUN_MAPPER (isupper, xisupper, 0, 0, 0, 0.0, 0.0, 0,
    "isupper (X): ");

  DEFUN_MAPPER (isxdigit, xisxdigit, 0, 0, 0, 0.0, 0.0, 0,
    "isxdigit (X): ");

  DEFUN_MAPPER (lgamma, 0, xlgamma, 0, 0, 0.0, 0.0, 0,
    "lgamma (X): compute log gamma (X) for each element of X");

  DEFUN_MAPPER (log, 0, log, 0, log, 0.0, DBL_MAX, 1,
    "log (X): compute log (X) for each element of X");

  DEFUN_MAPPER (log10, 0, log10, 0, log10, 0.0, DBL_MAX, 1,
    "log10 (X): compute log10 (X) for each element of X");

  DEFUN_MAPPER (real, 0, real, real, 0, 0.0, 0.0, 0,
    "real (X): return real part for each element of X");

  DEFUN_MAPPER (round, 0, round, 0, round, 0.0, 0.0, 0,
    "round (X): round elements of X to nearest integer");

  DEFUN_MAPPER (sign, 0, signum, 0, signum, 0.0, 0.0, 0,
    "sign (X): apply signum function to elements of X");

  DEFUN_MAPPER (sin, 0, sin, 0, sin, 0.0, 0.0, 0,
    "sin (X): compute sin (X) for each element of X");

  DEFUN_MAPPER (sinh, 0, sinh, 0, sinh, 0.0, 0.0, 0,
    "sinh (X): compute sinh (X) for each element of X");

  DEFUN_MAPPER (sqrt, 0, sqrt, 0, sqrt, 0.0, DBL_MAX, 1,
    "sqrt (X): compute sqrt (X) for each element of X");

  DEFUN_MAPPER (tan, 0, tan, 0, tan, 0.0, 0.0, 0,
    "tan (X): compute tan (X) for each element of X");

  DEFUN_MAPPER (tanh, 0, tanh, 0, tanh, 0.0, 0.0, 0,
    "tanh (X): compute tanh (X) for each element of X");

  DEFALIAS (gammaln, lgamma);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
