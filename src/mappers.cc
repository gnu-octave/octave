// mappers.cc                                             -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1995 John W. Eaton

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
Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <math.h>
#include <float.h>
#include <Complex.h>

#include "missing-math.h"
#include "f77-uscore.h"
#include "variables.h"
#include "mappers.h"
#include "error.h"
#include "utils.h"
#include "defun.h"

#if defined (_AIX) && defined (__GNUG__)
#undef finite
#define finite(x) ((x) < DBL_MAX && (x) > -DBL_MAX)
#endif

extern "C"
{
  double F77_FCN (dgamma) (const double&);
  int F77_FCN (dlgams) (const double&, double&, double&);
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
    return 0.0;
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
  return 0.0;
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
  return tmp;
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
  return (double) (x > -DBL_MAX && x < DBL_MAX);
#endif
}

double
xgamma (double x)
{
  return F77_FCN (dgamma) (x);
}

double
xisinf (double x)
{
#if defined (HAVE_ISINF)
  return (double) isinf (x);
#elif defined (HAVE_FINITE) && defined (HAVE_ISNAN)
  return (double) (! (finite (x) || isnan (x)));
#else
  return (double) (x == DBL_MAX || x == -DBL_MAX);
#endif
}

double
xlgamma (double x)
{
  double result;
  double sgngam;

  F77_FCN (dlgams) (x, result, sgngam);

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

void
install_mapper_functions (void)
{
  DEFUN_MAPPER ("abs", Sabs, 0, 0.0, 0.0, fabs, abs, 0,
    "abs (X): compute abs (X) for each element of X");

  DEFUN_MAPPER ("acos", Sacos, 1, -1.0, 1.0, acos, 0, acos,
    "acos (X): compute acos (X) for each element of X");

  DEFUN_MAPPER ("acosh", Sacosh, 1, 1.0, DBL_MAX, acosh, 0, acosh,
    "acosh (X): compute acosh (X) for each element of X");

  DEFUN_MAPPER ("angle", Sangle, 0, 0.0, 0.0, arg, arg, 0,
    "angle (X): compute arg (X) for each element of X");

  DEFUN_MAPPER ("arg", Sarg, 0, 0.0, 0.0, arg, arg, 0,
    "arg (X): compute arg (X) for each element of X");

  DEFUN_MAPPER ("asin", Sasin, 1, -1.0, 1.0, asin, 0, asin,
    "asin (X): compute asin (X) for each element of X");

  DEFUN_MAPPER ("asinh", Sasinh, 0, 0.0, 0.0, asinh, 0, asinh,
    "asinh (X): compute asinh (X) for each element of X");

  DEFUN_MAPPER ("atan", Satan, 0, 0.0, 0.0, atan, 0, atan,
    "atan (X): compute atan (X) for each element of X");

  DEFUN_MAPPER ("atanh", Satanh, 1, -1.0, 1.0, atanh, 0, atanh,
    "atanh (X): compute atanh (X) for each element of X");

  DEFUN_MAPPER ("ceil", Sceil, 0, 0.0, 0.0, ceil, 0, ceil,
    "ceil (X): round elements of X toward +Inf");

  DEFUN_MAPPER ("conj", Sconj, 0, 0.0, 0.0, conj, 0, conj,
    "conj (X): compute complex conjugate for each element of X");

  DEFUN_MAPPER ("cos", Scos, 0, 0.0, 0.0, cos, 0, cos,
    "cos (X): compute cos (X) for each element of X");

  DEFUN_MAPPER ("cosh", Scosh, 0, 0.0, 0.0, cosh, 0, cosh,
    "cosh (X): compute cosh (X) for each element of X");

  DEFUN_MAPPER ("erf", Serf, 0, 0.0, 0.0, xerf, 0, 0,
    "erf (X): compute erf (X) for each element of X");

  DEFUN_MAPPER ("erfc", Serfc, 0, 0.0, 0.0, xerfc, 0, 0,
    "erfc (X): compute erfc (X) for each element of X");

  DEFUN_MAPPER ("exp", Sexp, 0, 0.0, 0.0, exp, 0, exp,
    "exp (X): compute exp (X) for each element of X");

  DEFUN_MAPPER ("finite", Sfinite, 0, 0.0, 0.0, xfinite, xfinite, 0,
    "finite (X): return 1 for finite elements of X");

  DEFUN_MAPPER ("fix", Sfix, 0, 0.0, 0.0, fix, 0, fix,
    "fix (X): round elements of X toward zero");

  DEFUN_MAPPER ("floor", Sfloor, 0, 0.0, 0.0, floor, 0, floor,
    "floor (X): round elements of X toward -Inf");

  DEFUN_MAPPER ("gamma", Sgamma, 0, 0.0, 0.0, xgamma, 0, 0,
    "gamma (X): compute gamma (X) for each element of X");

  DEFUN_MAPPER ("isinf", Sisinf, 0, 0.0, 0.0, xisinf, xisinf, 0,
    "isinf (X): return 1 for elements of X infinite");

  DEFUN_MAPPER ("imag", Simag, 0, 0.0, 0.0, imag, imag, 0,
    "imag (X): return imaginary part for each elements of X");

  DEFUN_MAPPER ("isnan", Sisnan, 0, 0.0, 0.0, xisnan, xisnan, 0,
    "isnan (X): return 1 where elements of X are NaNs");

  DEFUN_MAPPER ("lgamma", Slgamma, 0, 0.0, 0.0, xlgamma, 0, 0,
    "lgamma (X): compute log gamma (X) for each element of X");

  DEFUN_MAPPER ("log", Slog, 1, 0.0, DBL_MAX, log, 0, log,
    "log (X): compute log (X) for each element of X");

  DEFUN_MAPPER ("log10", Slog10, 1, 0.0, DBL_MAX, log10, 0, log10,
    "log10 (X): compute log10 (X) for each element of X");

  DEFUN_MAPPER ("real", Sreal, 0, 0.0, 0.0, real, real, 0,
    "real (X): return real part for each element of X");

  DEFUN_MAPPER ("round", Sround, 0, 0.0, 0.0, round, 0, round,
    "round (X): round elements of X to nearest integer");

  DEFUN_MAPPER ("sign", Ssign, 0, 0.0, 0.0, signum, 0, signum,
    "sign (X): apply signum function to elements of X");

  DEFUN_MAPPER ("sin", Ssin, 0, 0.0, 0.0, sin, 0, sin,
    "sin (X): compute sin (X) for each element of X");

  DEFUN_MAPPER ("sinh", Ssinh, 0, 0.0, 0.0, sinh, 0, sinh,
    "sinh (X): compute sinh (X) for each element of X");

  DEFUN_MAPPER ("sqrt", Ssqrt, 1, 0.0, DBL_MAX, sqrt, 0, sqrt,
    "sqrt (X): compute sqrt (X) for each element of X");

  DEFUN_MAPPER ("tan", Stan, 0, 0.0, 0.0, tan, 0, tan,
    "tan (X): compute tan (X) for each element of X");

  DEFUN_MAPPER ("tanh", Stanh, 0, 0.0, 0.0, tanh, 0, tanh,
    "tanh (X): compute tanh (X) for each element of X");
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
