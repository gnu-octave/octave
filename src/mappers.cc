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

#include <cctype>
#include <cfloat>

#include "lo-specfun.h"
#include "lo-mappers.h"

#include "defun.h"
#include "error.h"
#include "ov-mapper.h"
#include "variables.h"

// XXX FIXME XXX -- perhaps this could be avoided by determining
// whether the is* functions are actually functions or just macros.

static int
xisalnum (int c)
{
  return isalnum (c);
}

static int
xisalpha (int c)
{
  return isalpha (c);
}

static int
xisascii (int c)
{
  return isascii (c);
}

static int
xiscntrl (int c)
{
  return iscntrl (c);
}

static int
xisdigit (int c)
{
  return isdigit (c);
}

static int
xisgraph (int c)
{
  return isgraph (c);
}

static int
xislower (int c)
{
  return islower (c);
}

static int
xisprint (int c)
{
  return isprint (c);
}

static int
xispunct (int c)
{
  return ispunct (c);
}

static int
xisspace (int c)
{
  return isspace (c);
}

static int
xisupper (int c)
{
  return isupper (c);
}

static int
xisxdigit (int c)
{
  return isxdigit (c);
}

static int
xtoascii (int c)
{
  return toascii (c);
}

static int
xtolower (int c)
{
  return tolower (c);
}

static int
xtoupper (int c)
{
  return toupper (c);
}

void
install_mapper_functions (void)
{
  DEFUN_MAPPER (abs, 0, 0, 0, fabs, abs, 0, 0.0, 0.0, 0,
    "abs (X): compute abs (X) for each element of X");

  DEFUN_MAPPER (acos, 0, 0, 0, acos, 0, acos, -1.0, 1.0, 1,
    "acos (X): compute acos (X) for each element of X");

  DEFUN_MAPPER (acosh, 0, 0, 0, acosh, 0, acosh, 1.0, DBL_MAX, 1,
    "acosh (X): compute acosh (X) for each element of X");

  DEFUN_MAPPER (angle, 0, 0, 0, arg, arg, 0, 0.0, 0.0, 0,
    "angle (X): compute arg (X) for each element of X");

  DEFUN_MAPPER (arg, 0, 0, 0, arg, arg, 0, 0.0, 0.0, 0,
    "arg (X): compute arg (X) for each element of X");

  DEFUN_MAPPER (asin, 0, 0, 0, asin, 0, asin, -1.0, 1.0, 1,
    "asin (X): compute asin (X) for each element of X");

  DEFUN_MAPPER (asinh, 0, 0, 0, asinh, 0, asinh, 0.0, 0.0, 0,
    "asinh (X): compute asinh (X) for each element of X");

  DEFUN_MAPPER (atan, 0, 0, 0, atan, 0, atan, 0.0, 0.0, 0,
    "atan (X): compute atan (X) for each element of X");

  DEFUN_MAPPER (atanh, 0, 0, 0, atanh, 0, atanh, -1.0, 1.0, 1,
    "atanh (X): compute atanh (X) for each element of X");

  DEFUN_MAPPER (ceil, 0, 0, 0, ceil, 0, ceil, 0.0, 0.0, 0,
    "ceil (X): round elements of X toward +Inf");

  DEFUN_MAPPER (conj, 0, 0, 0, conj, 0, conj, 0.0, 0.0, 0,
    "conj (X): compute complex conjugate for each element of X");

  DEFUN_MAPPER (cos, 0, 0, 0, cos, 0, cos, 0.0, 0.0, 0,
    "cos (X): compute cos (X) for each element of X");

  DEFUN_MAPPER (cosh, 0, 0, 0, cosh, 0, cosh, 0.0, 0.0, 0,
    "cosh (X): compute cosh (X) for each element of X");

  DEFUN_MAPPER (erf, 0, 0, 0, xerf, 0, 0, 0.0, 0.0, 0,
    "erf (X): compute erf (X) for each element of X");

  DEFUN_MAPPER (erfc, 0, 0, 0, xerfc, 0, 0, 0.0, 0.0, 0,
    "erfc (X): compute erfc (X) for each element of X");

  DEFUN_MAPPER (exp, 0, 0, 0, exp, 0, exp, 0.0, 0.0, 0,
    "exp (X): compute exp (X) for each element of X");

  DEFUN_MAPPER (finite, 0, xfinite, xfinite, 0, 0, 0, 0.0, 0.0, 0,
    "finite (X): return 1 for finite elements of X");

  DEFUN_MAPPER (fix, 0, 0, 0, fix, 0, fix, 0.0, 0.0, 0,
    "fix (X): round elements of X toward zero");

  DEFUN_MAPPER (floor, 0, 0, 0, floor, 0, floor, 0.0, 0.0, 0,
    "floor (X): round elements of X toward -Inf");

  DEFUN_MAPPER (gamma, 0, 0, 0, xgamma, 0, 0, 0.0, 0.0, 0,
    "gamma (X): compute gamma (X) for each element of X");

  DEFUN_MAPPER (imag, 0, 0, 0, imag, imag, 0, 0.0, 0.0, 0,
    "imag (X): return imaginary part for each elements of X");

  DEFUN_MAPPER (isalnum, xisalnum, 0, 0, 0, 0, 0, 0.0, 0.0, 0,
    "isalnum (X): ");

  DEFUN_MAPPER (isalpha, xisalpha, 0, 0, 0, 0, 0, 0.0, 0.0, 0,
    "isalpha (X): ");

  DEFUN_MAPPER (isascii, xisascii, 0, 0, 0, 0, 0, 0.0, 0.0, 0,
    "isascii (X): ");

  DEFUN_MAPPER (iscntrl, xiscntrl, 0, 0, 0, 0, 0, 0.0, 0.0, 0,
    "iscntrl (X): ");

  DEFUN_MAPPER (isdigit, xisdigit, 0, 0, 0, 0, 0, 0.0, 0.0, 0,
    "isdigit (X): ");

  DEFUN_MAPPER (isinf, 0, xisinf, xisinf, 0, 0, 0, 0.0, 0.0, 0,
    "isinf (X): return 1 for elements of X infinite");

  DEFUN_MAPPER (isgraph, xisgraph, 0, 0, 0, 0, 0, 0.0, 0.0, 0,
    "isgraph (X): ");

  DEFUN_MAPPER (islower, xislower, 0, 0, 0, 0, 0, 0.0, 0.0, 0,
    "islower (X): ");

  DEFUN_MAPPER (isnan, 0, xisnan, xisnan, 0, 0, 0, 0.0, 0.0, 0,
    "isnan (X): return 1 where elements of X are NaNs");

  DEFUN_MAPPER (isprint, xisprint, 0, 0, 0, 0, 0, 0.0, 0.0, 0,
    "isprint (X): ");

  DEFUN_MAPPER (ispunct, xispunct, 0, 0, 0, 0, 0, 0.0, 0.0, 0,
    "ispunct (X): ");

  DEFUN_MAPPER (isspace, xisspace, 0, 0, 0, 0, 0, 0.0, 0.0, 0,
    "isspace (X): ");

  DEFUN_MAPPER (isupper, xisupper, 0, 0, 0, 0, 0, 0.0, 0.0, 0,
    "isupper (X): ");

  DEFUN_MAPPER (isxdigit, xisxdigit, 0, 0, 0, 0, 0, 0.0, 0.0, 0,
    "isxdigit (X): ");

  DEFUN_MAPPER (lgamma, 0, 0, 0, xlgamma, 0, 0, 0.0, 0.0, 0,
    "lgamma (X): compute log gamma (X) for each element of X");

  DEFUN_MAPPER (log, 0, 0, 0, log, 0, log, 0.0, DBL_MAX, 1,
    "log (X): compute log (X) for each element of X");

  DEFUN_MAPPER (log10, 0, 0, 0, log10, 0, log10, 0.0, DBL_MAX, 1,
    "log10 (X): compute log10 (X) for each element of X");

  DEFUN_MAPPER (real, 0, 0, 0, real, real, 0, 0.0, 0.0, 0,
    "real (X): return real part for each element of X");

  DEFUN_MAPPER (round, 0, 0, 0, round, 0, round, 0.0, 0.0, 0,
    "round (X): round elements of X to nearest integer");

  DEFUN_MAPPER (sign, 0, 0, 0, signum, 0, signum, 0.0, 0.0, 0,
    "sign (X): apply signum function to elements of X");

  DEFUN_MAPPER (sin, 0, 0, 0, sin, 0, sin, 0.0, 0.0, 0,
    "sin (X): compute sin (X) for each element of X");

  DEFUN_MAPPER (sinh, 0, 0, 0, sinh, 0, sinh, 0.0, 0.0, 0,
    "sinh (X): compute sinh (X) for each element of X");

  DEFUN_MAPPER (sqrt, 0, 0, 0, sqrt, 0, sqrt, 0.0, DBL_MAX, 1,
    "sqrt (X): compute sqrt (X) for each element of X");

  DEFUN_MAPPER (tan, 0, 0, 0, tan, 0, tan, 0.0, 0.0, 0,
    "tan (X): compute tan (X) for each element of X");

  DEFUN_MAPPER (tanh, 0, 0, 0, tanh, 0, tanh, 0.0, 0.0, 0,
    "tanh (X): compute tanh (X) for each element of X");

  DEFUN_MAPPER (toascii, xtoascii, 0, 0, 0, 0, 0, 0.0, 0.0, 1,
    "toascii (STRING): return ASCII representation of STRING in a matrix");

  DEFUN_MAPPER (tolower, xtolower, 0, 0, 0, 0, 0, 0.0, 0.0, 2,
    "tolower (STRING): convert upper case characters to lower case in STRING");

  DEFUN_MAPPER (toupper, xtoupper, 0, 0, 0, 0, 0, 0.0, 0.0, 2,
    "toupper (STRING): convert lower case characters to upper case in STRING");

  DEFALIAS (gammaln, lgamma);

  DEFALIAS (isfinite, finite);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
