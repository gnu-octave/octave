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
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} abs (@var{z})\n\
Compute the magnitude of @var{z}, defined as\n\
@iftex\n\
@tex\n\
$|z| = \\sqrt{x^2 + y^2}$.\n\
@end tex\n\
@end iftex\n\
@ifinfo\n\
|@var{z}| = @code{sqrt (x^2 + y^2)}.\n\
@end ifinfo\n\
\n\
For example,\n\
\n\
@example\n\
@group\n\
abs (3 + 4i)\n\
     @result{} 5\n\
@end group\n\
@end example\n\
@end deftypefn");

  DEFUN_MAPPER (acos, 0, 0, 0, acos, 0, acos, -1.0, 1.0, 1,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} acos (@var{X})\n\
acos (X): compute the inverse cosine of X for each element of X\n\
@end deftypefn");

  DEFUN_MAPPER (acosh, 0, 0, 0, acosh, 0, acosh, 1.0, DBL_MAX, 1,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} acosh (@var{X})\n\
acosh (X): compute the inverse hyperbolic cosine of X for each element of X.\n\
@end deftypefn");

  DEFUN_MAPPER (angle, 0, 0, 0, arg, arg, 0, 0.0, 0.0, 0,
    "See arg.");

  DEFUN_MAPPER (arg, 0, 0, 0, arg, arg, 0, 0.0, 0.0, 0,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} angle (@var{z})\n\
Compute the argument of @var{z}, defined as\n\
@iftex\n\
@tex\n\
$\\theta = \\tan^{-1}(y/x)$.\n\
@end tex\n\
@end iftex\n\
@ifinfo\n\
@var{theta} = @code{atan (@var{y}/@var{x})}.\n\
@end ifinfo\n\
\n\
@noindent\n\
in radians. \n\
\n\
For example,\n\
\n\
@example\n\
@group\n\
arg (3 + 4i)\n\
     @result{} 0.92730\n\
@end group\n\
@end example\n\
@end deftypefn");

  DEFUN_MAPPER (asin, 0, 0, 0, asin, 0, asin, -1.0, 1.0, 1,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} asin (@var{X})\n\
asin (X): compute inverse sin (X) for each element of X\n\
@end deftypefn");

  DEFUN_MAPPER (asinh, 0, 0, 0, asinh, 0, asinh, 0.0, 0.0, 0,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} asinh (@var{X})\n\
asinh (X): compute the inverse hyperbolic sin (X) for each element of X\n\
@end deftypefn");

  DEFUN_MAPPER (atan, 0, 0, 0, atan, 0, atan, 0.0, 0.0, 0,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} atan (@var{X})\n\
atan (X): compute the inverse tangent of (X) for each element of X\n\
@end deftypefn");

  DEFUN_MAPPER (atanh, 0, 0, 0, atanh, 0, atanh, -1.0, 1.0, 1,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} atanh (@var{X})\n\
atanh (X): compute the inverse hyperbolic tanget of X for each element of X\n\
@end deftypefn");

  DEFUN_MAPPER (ceil, 0, 0, 0, ceil, 0, ceil, 0.0, 0.0, 0,
    "-*- texinfo -*-\n\
@deftypefn {Usage} {} ceil (@var{x})\n\
Return the smallest integer not less than @var{x}.  If @var{x} is\n\
complex, return @code{ceil (real (@var{x})) + ceil (imag (@var{x})) * I}.\n\
@end deftypefn");

  DEFUN_MAPPER (conj, 0, 0, 0, conj, 0, conj, 0.0, 0.0, 0,
    "-* texinfo -*-\n\
@deftypefn {Mapping Function} {} conj (@var{z})\n\
Return the complex conjugate of @var{z}, defined as\n\
@iftex\n\
@tex\n\
$\\bar{z} = x - iy$.\n\
@end tex\n\
@end iftex\n\
@ifinfo\n\
@code{conj (@var{z})} = @var{x} - @var{i}@var{y}.\n\
@end ifinfo\n\
@end deftypefn\n\
\n\
See also: real, imag");

  DEFUN_MAPPER (cos, 0, 0, 0, cos, 0, cos, 0.0, 0.0, 0,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} cos (@var{X})\n\
cos (X): compute the cosine of X for each element of X\n\
@end deftypefn");

  DEFUN_MAPPER (cosh, 0, 0, 0, cosh, 0, cosh, 0.0, 0.0, 0,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} acosh (@var{X})\n\
acosh (X): compute the inverse hyperbolic cosine of X for each element of X\n\
@end deftypefn");

  DEFUN_MAPPER (erf, 0, 0, 0, xerf, 0, 0, 0.0, 0.0, 0,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} erf (@var{z})\n\
Computes the error function,\n\
@iftex\n\
@tex\n\
$$\n\
 {\\rm erf} (z) = {2 \\over \\sqrt{\\pi}}\\int_0^z e^{-t^2} dt\n\
$$\n\
@end tex\n\
@end iftex\n\
@ifinfo\n\
\n\
@smallexample\n\
                         z\n\
                        /\n\
erf (z) = (2/sqrt (pi)) | e^(-t^2) dt\n\
                        /\n\
                     t=0\n\
@end smallexample\n\
@end ifinfo\n\
@end deftypefn\n\

See also: erfc, erfinv");

  DEFUN_MAPPER (erfc, 0, 0, 0, xerfc, 0, 0, 0.0, 0.0, 0,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} erfc (@var{z})\n\
Computes the complementary error function,\n\
@iftex\n\
@tex\n\
$1 - {\\rm erf} (z)$.\n\
@end tex\n\
@end iftex\n\
@ifinfo\n\
@code{1 - erf (@var{z})}.\n\
@end ifinfo\n\
@end deftypefn\n\
\n\
See also: erf, erfinv");

  DEFUN_MAPPER (exp, 0, 0, 0, exp, 0, exp, 0.0, 0.0, 0,
    "-*- texinfo -*-\n\
@deftypefn {Usage} {} exp (@var{x})\n\
Compute the exponential of @var{x}.  To compute the matrix exponential,\n\
see @ref{Linear Algebra}.\n\
@end deftypefn");

  DEFUN_MAPPER (finite, 0, xfinite, xfinite, 0, 0, 0, 0.0, 0.0, 0,
    "finite (X): return 1 for finite elements of X");

  DEFUN_MAPPER (fix, 0, 0, 0, fix, 0, fix, 0.0, 0.0, 0,
    "-*- texinfo -*-\n\
@deftypefn {Usage} {} fix (@var{x})\n\
Truncate @var{x} toward zero.  If @var{x} is complex, return\n\
@code{fix (real (@var{x})) + fix (imag (@var{x})) * I}.\n\
@end deftypefn");

  DEFUN_MAPPER (floor, 0, 0, 0, floor, 0, floor, 0.0, 0.0, 0,
    "-*- texinfo -*-\n\
@deftypefn {Usage} {} floor (@var{x})\n\
Return the largest integer not greater than @var{x}.  If @var{x} is\n\
complex, return @code{floor (real (@var{x})) + floor (imag (@var{x})) * I}.\n\
@end deftypefn");

  DEFUN_MAPPER (gamma, 0, 0, 0, xgamma, 0, 0, 0.0, 0.0, 0,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} gamma (@var{z})\n\
Computes the Gamma function,\n\
@iftex\n\
@tex\n\
$$\n\
 \\Gamma (z) = \\int_0^\\infty t^{z-1} e^{-t} dt.\n\
$$\n\
@end tex\n\
@end iftex\n\
@ifinfo\n\
\n\
@example\n\
            infinity\n\
            /\n\
gamma (z) = | t^(z-1) exp (-t) dt.\n\
            /\n\
         t=0\n\
@end example\n\
@end ifinfo\n\
@end deftypefn\n\
\n\
See also: gammai, lgamma");

  DEFUN_MAPPER (imag, 0, 0, 0, imag, imag, 0, 0.0, 0.0, 0,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} imag (@var{z})\n\
Return the imaginary part of @var{z} as a real number.\n\
@end deftypefn\n\
\n\
See also: real, conj");

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
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} lgamma (@var{a}, @var{x})\n\
@deftypefnx {Mapping Function} {} gammaln (@var{a}, @var{x})\n\
Return the natural logarithm of the gamma function.\n\
@end deftypefn\n\
\n\
See also: gamma, gammai");

  DEFUN_MAPPER (log, 0, 0, 0, log, 0, log, 0.0, DBL_MAX, 1,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} log (@var{x})\n\
Compute the natural logarithm for each element of @var{x}.  To compute the\n\
matrix logarithm, see @ref{Linear Algebra}.\n\
@end deftypefn\n\
\n\
See also: log2, log10, logspace, exp");

  DEFUN_MAPPER (log10, 0, 0, 0, log10, 0, log10, 0.0, DBL_MAX, 1,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} log10 (@var{x})\n\
Compute the base-10 logarithm for each element of @var{x}.\n\
@end deftypefn\n\
\n\
See also: log, log2, logspace, exp");

  DEFUN_MAPPER (real, 0, 0, 0, real, real, 0, 0.0, 0.0, 0,
    "-*-texinfo -*-\n\
@deftypefn {Mapping Function} {} real (@var{z})\n\
Return the real part of @var{z}.\n\
@end deftypefn\n\
\n\
See also: imag, conj");

  DEFUN_MAPPER (round, 0, 0, 0, round, 0, round, 0.0, 0.0, 0,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} round (@var{x})\n\
Return the integer nearest to @var{x}.  If @var{x} is complex, return\n\
@code{round (real (@var{x})) + round (imag (@var{x})) * I}.\n\
@end deftypefn\n\
\n\
See also: rem");

  DEFUN_MAPPER (sign, 0, 0, 0, signum, 0, signum, 0.0, 0.0, 0,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} sign (@var{x})\n\
Compute the @dfn{signum} function, which is defined as\n\
@iftex\n\
@tex\n\
$$\n\
{\\rm sign} (@var{x}) = \\cases{1,&$x>0$;\\cr 0,&$x=0$;\\cr -1,&$x<0$.\\cr}\n\
$$\n\
@end tex\n\
@end iftex\n\
@ifinfo\n\
\n\
@example\n\
           -1, x < 0;\n\
sign (x) =  0, x = 0;\n\
            1, x > 0.\n\
@end example\n\
@end ifinfo\n\
\n\
For complex arguments, @code{sign} returns @code{x ./ abs (@var{x})}.\n\
@end deftypefn");

  DEFUN_MAPPER (sin, 0, 0, 0, sin, 0, sin, 0.0, 0.0, 0,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} sin (@var{X})\n\
sin (X): compute the sin of X for each element of X\n\
@end deftypefn");

  DEFUN_MAPPER (sinh, 0, 0, 0, sinh, 0, sinh, 0.0, 0.0, 0,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} sinh (@var{X})\n\
sinh (X): compute the inverse hyperbolic sin of X for each element of X\n\
@end deftypefn");

  DEFUN_MAPPER (sqrt, 0, 0, 0, sqrt, 0, sqrt, 0.0, DBL_MAX, 1,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} sqrt (@var{x})\n\
Compute the square root of @var{x}.  If @var{x} is negative, a complex\n\
result is returned.  To compute the matrix square root, see\n\
@ref{Linear Algebra}.\n\
@end deftypefn");

  DEFUN_MAPPER (tan, 0, 0, 0, tan, 0, tan, 0.0, 0.0, 0,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} tan (@var{z})\n\
tan (X): compute tanget of X for each element of X\n\
@end deftypefn");

  DEFUN_MAPPER (tanh, 0, 0, 0, tanh, 0, tanh, 0.0, 0.0, 0,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} tanh (@var{X})\n\
tanh (X): compute hyperbolic tangent of X for each element of X\n\
@end deftypefn");

  DEFUN_MAPPER (toascii, xtoascii, 0, 0, 0, 0, 0, 0.0, 0.0, 1,
    "toascii (STRING): return ASCII representation of STRING in a matrix");

  DEFUN_MAPPER (tolower, xtolower, 0, 0, 0, 0, 0, 0.0, 0.0, 2,
    "tolower (STRING): convert upper case characters to lower case in STRING");

  DEFUN_MAPPER (toupper, xtoupper, 0, 0, 0, 0, 0, 0.0, 0.0, 2,
    "toupper (STRING): convert lower case characters to upper case in STRING");

  DEFALIAS (gammaln, lgamma);

  DEFALIAS (isfinite, finite);

  // Leave the previous new line, mkgendoc needs it!
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
