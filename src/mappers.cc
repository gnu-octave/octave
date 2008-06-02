/*

Copyright (C) 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001,
              2002, 2003, 2004, 2005, 2006, 2007 John W. Eaton

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

#include <cctype>
#include <cfloat>

#include "lo-ieee.h"
#include "lo-specfun.h"
#include "lo-mappers.h"

#include "defun.h"
#include "error.h"
#include "variables.h"

DEFUN (abs, args, ,
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
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).abs ();
  else
    print_usage ();

  return retval;
}

DEFUN (acos, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} acos (@var{x})\n\
Compute the inverse cosine of each element of @var{x}.\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).acos ();
  else
    print_usage ();

  return retval;
}


DEFUN (acosh, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} acosh (@var{x})\n\
Compute the inverse hyperbolic cosine of each element of @var{x}.\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).acosh ();
  else
    print_usage ();

  return retval;
}

DEFUN (angle, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} angle (@var{z})\n\
See arg.\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).arg ();
  else
    print_usage ();

  return retval;
}

DEFUN (arg, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} arg (@var{z})\n\
@deftypefnx {Mapping Function} {} angle (@var{z})\n\
Compute the argument of @var{z}, defined as\n\
@iftex\n\
@tex\n\
$\\theta = \\tan^{-1}(y/x)$.\n\
@end tex\n\
@end iftex\n\
@ifinfo\n\
@var{theta} = @code{atan (@var{y}/@var{x})}.\n\
@end ifinfo\n\
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
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).arg ();
  else
    print_usage ();

  return retval;
}

DEFUN (asin, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} asin (@var{x})\n\
Compute the inverse sine of each element of @var{x}.\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).asin ();
  else
    print_usage ();

  return retval;
}

/*
%!test
%! rt2 = sqrt (2);
%! rt3 = sqrt (3);
%! v = [0, pi/6, pi/4, pi/3, pi/2, pi/3, pi/4, pi/6, 0];
%! x = [0, 1/2, rt2/2, rt3/2, 1, rt3/2, rt2/2, 1/2, 0];
%! assert(all (abs (asin (x) - v) < sqrt (eps)));
%!error asin ();
%!error asin (1, 2);
*/

DEFUN (asinh, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} asinh (@var{x})\n\
Compute the inverse hyperbolic sine of each element of @var{x}.\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).asinh ();
  else
    print_usage ();

  return retval;
}

DEFUN (atan, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} atan (@var{x})\n\
Compute the inverse tangent of each element of @var{x}.\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).atan ();
  else
    print_usage ();

  return retval;
}

DEFUN (atanh, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} atanh (@var{x})\n\
Compute the inverse hyperbolic tangent of each element of @var{x}.\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).atanh ();
  else
    print_usage ();

  return retval;
}

DEFUN (ceil, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} ceil (@var{x})\n\
Return the smallest integer not less than @var{x}.  If @var{x} is\n\
complex, return @code{ceil (real (@var{x})) + ceil (imag (@var{x})) * I}.\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).ceil ();
  else
    print_usage ();

  return retval;
}

DEFUN (conj, args, ,
    "-*- texinfo -*-\n\
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
@seealso{real, imag}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).conj ();
  else
    print_usage ();

  return retval;
}

DEFUN (cos, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} cos (@var{x})\n\
Compute the cosine of each element of @var{x}.\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).cos ();
  else
    print_usage ();

  return retval;
}

DEFUN (cosh, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} cosh (@var{x})\n\
Compute the hyperbolic cosine of each element of @var{x}.\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).cosh ();
  else
    print_usage ();

  return retval;
}

DEFUN (erf, args, ,
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
@seealso{erfc, erfinv}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).erf ();
  else
    print_usage ();

  return retval;
}

/*

%!test
%! a = -1i*sqrt(-1/(6.4187*6.4187));
%! assert (erf(a), erf(real(a)));

*/

DEFUN (erfc, args, ,
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
@seealso{erf, erfinv}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).erfc ();
  else
    print_usage ();

  return retval;
}

/*

%!test
%! a = -1i*sqrt(-1/(6.4187*6.4187));
%! assert (erfc(a), erfc(real(a)));

*/

DEFUN (exp, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} exp (@var{x})\n\
Compute the exponential of @var{x}.  To compute the matrix exponential,\n\
see @ref{Linear Algebra}.\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).exp ();
  else
    print_usage ();

  return retval;
}

DEFUN (expm1, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} expm1 (@var{x})\n\
Compute exp (@var{x}) - 1 accurately in neighbourhood of zero.\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).expm1 ();
  else
    print_usage ();

  return retval;
}

DEFUN (finite, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} finite (@var{x})\n\
Return 1 for elements of @var{x} that are finite values and zero\n\
otherwise. For example,\n\
\n\
@example\n\
@group\n\
finite ([13, Inf, NA, NaN])\n\
     @result{} [ 1, 0, 0, 0 ]\n\
@end group\n\
@end example\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).finite ();
  else
    print_usage ();

  return retval;
}

/*

%!assert(!(finite (Inf)));
%!assert(!(finite (NaN)));
%!assert(finite (rand(1,10)));

%!assert(!(finite (single(Inf))));
%!assert(!(finite (single(NaN))));
%!assert(finite (single(rand(1,10))));

 */

DEFUN (fix, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} fix (@var{x})\n\
Truncate @var{x} toward zero.  If @var{x} is complex, return\n\
@code{fix (real (@var{x})) + fix (imag (@var{x})) * I}.\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).fix ();
  else
    print_usage ();

  return retval;
}


DEFUN (floor, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} floor (@var{x})\n\
Return the largest integer not greater than @var{x}.  If @var{x} is\n\
complex, return @code{floor (real (@var{x})) + floor (imag (@var{x})) * I}.\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).floor ();
  else
    print_usage ();

  return retval;
}

DEFUN (gamma, args, ,
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
@seealso{gammai, lgamma}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).gamma ();
  else
    print_usage ();

  return retval;
}

/*

%!test
%! a = -1i*sqrt(-1/(6.4187*6.4187));
%! assert (gamma(a), gamma(real(a)));

*/

DEFUN (imag, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} imag (@var{z})\n\
Return the imaginary part of @var{z} as a real number.\n\
@seealso{real, conj}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).imag ();
  else
    print_usage ();

  return retval;
}

DEFUNX ("isalnum", Fisalnum, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} isalnum (@var{s})\n\
Return 1 for characters that are letters or digits (@code{isalpha\n\
(@var{s})} or @code{isdigit (@var{s})} is true).\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).xisalnum ();
  else
    print_usage ();

  return retval;
}

DEFUNX ("isalpha", Fisalpha, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} isalpha (@var{s})\n\
@deftypefnx {Mapping Function} {} isletter (@var{s})\n\
Return true for characters that are letters (@code{isupper (@var{s})}\n\
or @code{islower (@var{s})} is true).\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).xisalpha ();
  else
    print_usage ();

  return retval;
}

DEFUNX ("isascii", Fisascii, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} isascii (@var{s})\n\
Return 1 for characters that are ASCII (in the range 0 to 127 decimal).\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).xisascii ();
  else
    print_usage ();

  return retval;
}

DEFUNX ("iscntrl", Fiscntrl, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} iscntrl (@var{s})\n\
Return 1 for control characters.\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).xiscntrl ();
  else
    print_usage ();

  return retval;
}

DEFUNX ("isdigit", Fisdigit, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} isdigit (@var{s})\n\
Return 1 for characters that are decimal digits.\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).xisdigit ();
  else
    print_usage ();

  return retval;
}

DEFUN (isinf, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} isinf (@var{x})\n\
Return 1 for elements of @var{x} that are infinite and zero\n\
otherwise. For example,\n\
\n\
@example\n\
@group\n\
isinf ([13, Inf, NA, NaN])\n\
     @result{} [ 0, 1, 0, 0 ]\n\
@end group\n\
@end example\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).isinf ();
  else
    print_usage ();

  return retval;
}

/*

%!assert(isinf (Inf));
%!assert(!isinf (NaN));
%!assert(!(isinf (NA)));
%!assert(isinf (rand(1,10)), false(1,10));
%!assert(isinf([NaN -Inf -1 0 1 Inf NA]), [false, true, false, false, false, true, false]);

%!assert(isinf (single(Inf)));
%!assert(!(isinf (single(NaN))));
%!assert(!(isinf (single(NA))));
%!assert(isinf (single(rand(1,10))), false(1,10));
%!assert(isinf(single([NaN -Inf -1 0 1 Inf NA])), [false, true, false, false, false, true, false]);

 */

DEFUNX ("isgraph", Fisgraph, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} isgraph (@var{s})\n\
Return 1 for printable characters (but not the space character).\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).xisgraph ();
  else
    print_usage ();

  return retval;
}

DEFUNX ("islower", Fislower, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} islower (@var{s})\n\
Return 1 for characters that are lower case letters.\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).xislower ();
  else
    print_usage ();

  return retval;
}

DEFUN (isna, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} isna (@var{x})\n\
Return 1 for elements of @var{x} that are NA (missing) values and zero\n\
otherwise.  For example,\n\
\n\
@example\n\
@group\n\
isna ([13, Inf, NA, NaN])\n\
     @result{} [ 0, 0, 1, 0 ]\n\
@end group\n\
@end example\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).isna ();
  else
    print_usage ();

  return retval;
}

/*

%!assert(!(isna (Inf)));
%!assert(!isna (NaN));
%!assert(isna (NA));
%!assert(isna (rand(1,10)), false(1,10));
%!assert(isna([NaN -Inf -1 0 1 Inf NA]), [false, false, false, false, false, false, true]);

%!assert(!(isna (single(Inf))));
%!assert(!isna (single(NaN)));
%!assert(isna (single(NA)));
%!assert(isna (single(rand(1,10))), false(1,10));
%!assert(isna(single([NaN -Inf -1 0 1 Inf NA])), [false, false, false, false, false, false, true]);

 */

DEFUN (isnan, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} isnan (@var{x})\n\
Return 1 for elements of @var{x} that are NaN values and zero\n\
otherwise.  NA values are also considered NaN values.  For example,\n\
\n\
@example\n\
@group\n\
isnan ([13, Inf, NA, NaN])\n\
     @result{} [ 0, 0, 1, 1 ]\n\
@end group\n\
@end example\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).isnan ();
  else
    print_usage ();

  return retval;
}

/*

%!assert(!(isnan (Inf)));
%!assert(isnan (NaN));
%!assert(isnan (NA));
%!assert(isnan (rand(1,10)), false(1,10));
%!assert(isnan([NaN -Inf -1 0 1 Inf NA]), [true, false, false, false, false, false, true]);

%!assert(!(isnan (single(Inf))));
%!assert(isnan (single(NaN)));
%!assert(isnan (single(NA)));
%!assert(isnan (single(rand(1,10))), false(1,10));
%!assert(isnan(single([NaN -Inf -1 0 1 Inf NA])), [true, false, false, false, false, false, true]);

 */

DEFUNX ("isprint", Fisprint, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} isprint (@var{s})\n\
Return 1 for printable characters (including the space character).\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).xisprint ();
  else
    print_usage ();

  return retval;
}

DEFUNX ("ispunct", Fispunct, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} ispunct (@var{s})\n\
Return 1 for punctuation characters.\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).xispunct ();
  else
    print_usage ();

  return retval;
}

DEFUNX ("isspace", Fisspace, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} isspace (@var{s})\n\
Return 1 for whitespace characters (space, formfeed, newline,\n\
carriage return, tab, and vertical tab).\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).xisspace ();
  else
    print_usage ();

  return retval;
}

DEFUNX ("isupper", Fisupper, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} isupper (@var{s})\n\
Return 1 for upper case letters.\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).xisupper ();
  else
    print_usage ();

  return retval;
}

DEFUNX ("isxdigit", Fisxdigit, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} isxdigit (@var{s})\n\
Return 1 for characters that are hexadecimal digits.\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).xisxdigit ();
  else
    print_usage ();

  return retval;
}

DEFUN (lgamma, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} lgamma (@var{x})\n\
@deftypefnx {Mapping Function} {} gammaln (@var{x})\n\
Return the natural logarithm of the gamma function of @var{x}.\n\
@seealso{gamma, gammai}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).lgamma ();
  else
    print_usage ();

  return retval;
}

/*

%!test
%! a = -1i*sqrt(-1/(6.4187*6.4187));
%! assert (lgamma(a), lgamma(real(a)));

*/

DEFUN (log, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} log (@var{x})\n\
Compute the natural logarithm for each element of @var{x}.  To compute the\n\
matrix logarithm, see @ref{Linear Algebra}.\n\
@seealso{log2, log10, logspace, exp}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).log ();
  else
    print_usage ();

  return retval;
}

DEFUN (log10, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} log10 (@var{x})\n\
Compute the base-10 logarithm for each element of @var{x}.\n\
@seealso{log, log2, logspace, exp}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).log10 ();
  else
    print_usage ();

  return retval;
}

DEFUN (log1p, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} log1p (@var{x})\n\
Compute log (1 + @var{x}) accurately in neighbourhood of zero.\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).log1p ();
  else
    print_usage ();

  return retval;
}

DEFUN (real, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} real (@var{z})\n\
Return the real part of @var{z}.\n\
@seealso{imag, conj}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).real ();
  else
    print_usage ();

  return retval;
}

DEFUN (round, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} round (@var{x})\n\
Return the integer nearest to @var{x}.  If @var{x} is complex, return\n\
@code{round (real (@var{x})) + round (imag (@var{x})) * I}.\n\
@seealso{rem}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).round ();
  else
    print_usage ();

  return retval;
}

DEFUN (roundb, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} roundb (@var{x})\n\
Return the integer nearest to @var{x}. If there are two nearest\n\
integers, return the even one (banker's rounding). If @var{x} is complex,\n\
return @code{roundb (real (@var{x})) + roundb (imag (@var{x})) * I}.\n\
@seealso{rem}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).roundb ();
  else
    print_usage ();

  return retval;
}

DEFUN (sign, args, ,
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
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).signum ();
  else
    print_usage ();

  return retval;
}

DEFUN (sin, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} sin (@var{x})\n\
Compute the sine of each element of @var{x}.\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).sin ();
  else
    print_usage ();

  return retval;
}

DEFUN (sinh, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} sinh (@var{x})\n\
Compute the hyperbolic sine of each element of @var{x}.\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).sinh ();
  else
    print_usage ();

  return retval;
}

DEFUN (sqrt, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} sqrt (@var{x})\n\
Compute the square root of @var{x}.  If @var{x} is negative, a complex\n\
result is returned.  To compute the matrix square root, see\n\
@ref{Linear Algebra}.\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).sqrt ();
  else
    print_usage ();

  return retval;
}

DEFUN (tan, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} tan (@var{z})\n\
Compute tangent of each element of @var{x}.\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).tan ();
  else
    print_usage ();

  return retval;
}

DEFUN (tanh, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} tanh (@var{x})\n\
Compute hyperbolic tangent of each element of @var{x}.\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).tanh ();
  else
    print_usage ();

  return retval;
}

DEFUNX ("toascii", Ftoascii, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} toascii (@var{s})\n\
Return ASCII representation of @var{s} in a matrix.  For example,\n\
\n\
@example\n\
@group\n\
toascii (\"ASCII\")\n\
     @result{} [ 65, 83, 67, 73, 73 ]\n\
@end group\n\
\n\
@end example\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).xtoascii ();
  else
    print_usage ();

  return retval;
}

DEFUNX ("tolower", Ftolower, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} tolower (@var{s})\n\
Return a copy of the string @var{s}, with each upper-case character\n\
replaced by the corresponding lower-case one; nonalphabetic characters\n\
are left unchanged.  For example,\n\
\n\
@example\n\
tolower (\"MiXeD cAsE 123\")\n\
     @result{} \"mixed case 123\"\n\
@end example\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).xtolower ();
  else
    print_usage ();

  return retval;
}

DEFUNX ("toupper", Ftoupper, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} toupper (@var{s})\n\
Return a copy of the string @var{s}, with each  lower-case character\n\
replaced by the corresponding upper-case one; nonalphabetic characters\n\
are left unchanged.  For example,\n\
\n\
@example\n\
@group\n\
toupper (\"MiXeD cAsE 123\")\n\
     @result{} \"MIXED CASE 123\"\n\
@end group\n\
@end example\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).xtoupper ();
  else
    print_usage ();

  return retval;
}

DEFALIAS (gammaln, lgamma);

DEFALIAS (isfinite, finite);

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
