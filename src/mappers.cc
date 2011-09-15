/*

Copyright (C) 1993-2011 John W. Eaton
Copyright (C) 2009-2010 VZLU Prague

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
@tex\n\
$|z| = \\sqrt{x^2 + y^2}$.\n\
@end tex\n\
@ifnottex\n\
|@var{z}| = @code{sqrt (x^2 + y^2)}.\n\
@end ifnottex\n\
\n\
For example:\n\
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

/*

%!assert(abs (1), 1);
%!assert(abs (-3.5), 3.5);
%!assert(abs (3+4i), 5);
%!assert(abs (3-4i), 5);
%!assert(abs ([1.1, 3i; 3+4i, -3-4i]), [1.1, 3; 5, 5]);

%!assert(abs (single(1)), single(1));
%!assert(abs (single(-3.5)), single(3.5));
%!assert(abs (single(3+4i)), single(5));
%!assert(abs (single(3-4i)), single(5));
%!assert(abs (single([1.1, 3i; 3+4i, -3-4i])), single([1.1, 3; 5, 5]));

%!error abs ();
%!error abs (1, 2);

 */

DEFUN (acos, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} acos (@var{x})\n\
Compute the inverse cosine in radians for each element of @var{x}.\n\
@seealso{cos, acosd}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).acos ();
  else
    print_usage ();

  return retval;
}

/*

%!test
%! rt2 = sqrt (2);
%! rt3 = sqrt (3);
%! v = [0, pi/6, pi/4, pi/3, pi/2, 2*pi/3, 3*pi/4, 5*pi/6, pi];
%! x = [1, rt3/2, rt2/2, 1/2, 0, -1/2, -rt2/2, -rt3/2, -1];
%! assert(acos (x), v, sqrt(eps));

%!test
%! rt2 = sqrt (2);
%! rt3 = sqrt (3);
%! v = single ([0, pi/6, pi/4, pi/3, pi/2, 2*pi/3, 3*pi/4, 5*pi/6, pi]);
%! x = single ([1, rt3/2, rt2/2, 1/2, 0, -1/2, -rt2/2, -rt3/2, -1]);
%! assert(acos (x), v, sqrt(eps('single')));

%!error acos ();
%!error acos (1, 2);

*/

DEFUN (acosh, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} acosh (@var{x})\n\
Compute the inverse hyperbolic cosine for each element of @var{x}.\n\
@seealso{cosh}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).acosh ();
  else
    print_usage ();

  return retval;
}

/*

%!test
%! v = [0, pi/2*i, pi*i, pi/2*i];
%! x = [1, 0, -1, 0];
%! assert(acosh (x), v, sqrt(eps));

%!test
%! v = single([0, pi/2*i, pi*i, pi/2*i]);
%! x = single([1, 0, -1, 0]);
%! assert(acosh (x), v, sqrt (eps('single')));

%!error acosh ();
%!error acosh (1, 2);

*/

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
@deftypefn  {Mapping Function} {} arg (@var{z})\n\
@deftypefnx {Mapping Function} {} angle (@var{z})\n\
Compute the argument of @var{z}, defined as,\n\
@tex\n\
$\\theta = atan2 (y, x),$\n\
@end tex\n\
@ifnottex\n\
@var{theta} = @code{atan2 (@var{y}, @var{x})},\n\
@end ifnottex\n\
in radians.\n\
\n\
For example:\n\
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

/*

%!assert(arg (1), 0);
%!assert(arg (i), pi/2);
%!assert(arg (-1), pi);
%!assert(arg (-i), -pi/2);
%!assert(arg ([1, i; -1, -i]), [0, pi/2; pi, -pi/2]);

%!assert(arg (single(1)), single(0));
%!assert(arg (single(i)), single(pi/2));
%!assert(arg (single(-1)), single(pi));
%!assert(arg (single(-i)), single(-pi/2));
%!assert(arg (single([1, i; -1, -i])), single([0, pi/2; pi, -pi/2]), 2e1*eps('single'));

%!error arg ();
%!error arg (1, 2);

*/

DEFUN (asin, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} asin (@var{x})\n\
Compute the inverse sine in radians for each element of @var{x}.\n\
@seealso{sin, asind}\n\
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
Compute the inverse hyperbolic sine for each element of @var{x}.\n\
@seealso{sinh}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).asinh ();
  else
    print_usage ();

  return retval;
}

/*

%!test
%! v = [0, pi/2*i, 0, -pi/2*i];
%! x = [0, i, 0, -i];
%! assert(asinh (x), v,  sqrt (eps));

%!test
%! v = single([0, pi/2*i, 0, -pi/2*i]);
%! x = single([0, i, 0, -i]);
%! assert(asinh (x), v,  sqrt (eps('single')));

%!error asinh ();
%!error asinh (1, 2);

*/

DEFUN (atan, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} atan (@var{x})\n\
Compute the inverse tangent in radians for each element of @var{x}.\n\
@seealso{tan, atand}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).atan ();
  else
    print_usage ();

  return retval;
}

/*

%!test
%! rt2 = sqrt (2);
%! rt3 = sqrt (3);
%! v = [0, pi/6, pi/4, pi/3, -pi/3, -pi/4, -pi/6, 0];
%! x = [0, rt3/3, 1, rt3, -rt3, -1, -rt3/3, 0];
%! assert(atan (x), v, sqrt (eps));

%!test
%! rt2 = sqrt (2);
%! rt3 = sqrt (3);
%! v = single([0, pi/6, pi/4, pi/3, -pi/3, -pi/4, -pi/6, 0]);
%! x = single([0, rt3/3, 1, rt3, -rt3, -1, -rt3/3, 0]);
%! assert(atan (x), v, sqrt (eps('single')));

%!error atan ();
%!error atan (1, 2);

 */

DEFUN (atanh, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} atanh (@var{x})\n\
Compute the inverse hyperbolic tangent for each element of @var{x}.\n\
@seealso{tanh}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).atanh ();
  else
    print_usage ();

  return retval;
}

/*

%!test
%! v = [0, 0];
%! x = [0, 0];
%! assert(atanh (x), v, sqrt (eps));

%!test
%! v = single([0, 0]);
%! x = single([0, 0]);
%! assert(atanh (x), v, sqrt (eps('single')));

%!error atanh ();
%!error atanh (1, 2);

*/

DEFUN (cbrt, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} cbrt (@var{x})\n\
Compute the real cube root of each element of @var{x}.\n\
Unlike @code{@var{x}^(1/3)}, the result will be negative if @var{x} is\n\
negative.\n\
@seealso{nthroot}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).cbrt ();
  else
    print_usage ();

  return retval;
}

/*

%!assert (cbrt (64), 4)
%!assert (cbrt (-125), -5)
%!assert (cbrt (0), 0)
%!assert (cbrt (Inf), Inf)
%!assert (cbrt (-Inf), -Inf)
%!assert (cbrt (NaN), NaN)
%!assert (cbrt (2^300), 2^100)
%!assert (cbrt (125*2^300), 5*2^100)

*/

DEFUN (ceil, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} ceil (@var{x})\n\
Return the smallest integer not less than @var{x}.  This is equivalent to\n\
rounding towards positive infinity.  If @var{x} is\n\
complex, return @code{ceil (real (@var{x})) + ceil (imag (@var{x})) * I}.\n\
\n\
@example\n\
@group\n\
ceil ([-2.7, 2.7])\n\
   @result{}  -2   3\n\
@end group\n\
@end example\n\
@seealso{floor, round, fix}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).ceil ();
  else
    print_usage ();

  return retval;
}

/*

%% double precision
%!assert(ceil ([2, 1.1, -1.1, -1]), [2, 2, -1, -1]);

%% compelx double precison
%!assert(ceil ([2+2i, 1.1+1.1i, -1.1-1.1i, -1-i]), [2+2i, 2+2i, -1-i, -1-i]);

%% single precision
%!assert(ceil (single([2, 1.1, -1.1, -1])), single([2, 2, -1, -1]));

%% compelx single preci
%!assert(ceil (single ([2+2i, 1.1+1.1i, -1.1-1.1i, -1-i])), single([2+2i, 2+2i, -1-i, -1-i]));

%!error ceil ();
%!error ceil (1, 2);

*/

DEFUN (conj, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} conj (@var{z})\n\
Return the complex conjugate of @var{z}, defined as\n\
@tex\n\
$\\bar{z} = x - iy$.\n\
@end tex\n\
@ifnottex\n\
@code{conj (@var{z})} = @var{x} - @var{i}@var{y}.\n\
@end ifnottex\n\
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

/*

%!assert(conj (1), 1);
%!assert(conj (i), -i)
%!assert(conj (1+i), 1-i)
%!assert(conj (1-i), 1+i)
%!assert(conj ([-1, -i; -1+i, -1-i]), [-1, i; -1-i, -1+i]);

%!assert(conj (single(1)), single(1));
%!assert(conj (single(i)), single(-i))
%!assert(conj (single(1+i)), single(1-i))
%!assert(conj (single(1-i)), single(1+i))
%!assert(conj (single([-1, -i; -1+i, -1-i])), single([-1, i; -1-i, -1+i]));

%!error conj ();
%!error conj (1, 2);

*/

DEFUN (cos, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} cos (@var{x})\n\
Compute the cosine for each element of @var{x} in radians.\n\
@seealso{acos, cosd, cosh}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).cos ();
  else
    print_usage ();

  return retval;
}

/*

%!test
%! rt2 = sqrt (2);
%! rt3 = sqrt (3);
%! x = [0, pi/6, pi/4, pi/3, pi/2, 2*pi/3, 3*pi/4, 5*pi/6, pi];
%! v = [1, rt3/2, rt2/2, 1/2, 0, -1/2, -rt2/2, -rt3/2, -1];
%! assert(cos (x), v, sqrt (eps));

%!test
%! rt2 = sqrt (2);
%! rt3 = sqrt (3);
%! x = single([0, pi/6, pi/4, pi/3, pi/2, 2*pi/3, 3*pi/4, 5*pi/6, pi]);
%! v = single([1, rt3/2, rt2/2, 1/2, 0, -1/2, -rt2/2, -rt3/2, -1]);
%! assert(cos (x), v, sqrt (eps('single')));

%!error cos ();
%!error cos (1, 2);

 */

DEFUN (cosh, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} cosh (@var{x})\n\
Compute the hyperbolic cosine for each element of @var{x}.\n\
@seealso{acosh, sinh, tanh}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).cosh ();
  else
    print_usage ();

  return retval;
}

/*

%!test
%! x = [0, pi/2*i, pi*i, 3*pi/2*i];
%! v = [1, 0, -1, 0];
%! assert(cosh (x), v, sqrt (eps));

%!test
%! x = single([0, pi/2*i, pi*i, 3*pi/2*i]);
%! v = single([1, 0, -1, 0]);
%! assert(cosh (x), v, sqrt (eps ('single')));

%!error cosh ();
%!error cosh (1, 2);

*/

DEFUN (erf, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} erf (@var{z})\n\
Compute the error function,\n\
@tex\n\
$$\n\
 {\\rm erf} (z) = {2 \\over \\sqrt{\\pi}}\\int_0^z e^{-t^2} dt\n\
$$\n\
@end tex\n\
@ifnottex\n\
\n\
@example\n\
@group\n\
@c spacing appears odd here, but is correct after Makeinfo\n\
                          z\n\
                         /\n\
erf (z) = (2/sqrt (pi)) | e^(-t^2) dt\n\
                         /\n\
                      t=0\n\
@end group\n\
@end example\n\
\n\
@end ifnottex\n\
@seealso{erfc, erfcx, erfinv}\n\
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

%!test
%! x=[0,.5,1];
%! v=[0, .520499877813047, .842700792949715];
%! assert(all(abs(erf(x)-v)<1.e-10) &&  all(abs(erf(-x)+v)<1.e-10) && all(abs(erfc(x)+v-1)<1.e-10) && all(abs(erfinv(v)-x)<1.e-10));

%!test
%! a = -1i*sqrt(single (-1/(6.4187*6.4187)));
%! assert (erf(a), erf(real(a)));

%!test
%! x=single ([0,.5,1]);
%! v=single ([0, .520499877813047, .842700792949715]);
%! assert(all(abs(erf(x)-v)<1.e-6) &&  all(abs(erf(-x)+v)<1.e-6) && all(abs(erfc(x)+v-1)<1.e-6) && all(abs(erfinv(v)-x)<1.e-6));

%% test/octave.test/arith/erf-2.m
%!error erf();

%% test/octave.test/arith/erf-3.m
%!error erf(1,2);



*/

DEFUN (erfinv, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} erfinv (@var{x})\n\
Compute the inverse error function, i.e., @var{y} such that\n\
\n\
@example\n\
  erf (@var{y}) == @var{x}\n\
@end example\n\
@seealso{erf, erfc, erfcx}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).erfinv ();
  else
    print_usage ();

  return retval;
}

/*
%% middle region
%!assert (erf (erfinv ([-0.9 -0.3 0 0.4 0.8])), [-0.9 -0.3 0 0.4 0.8], eps)
%!assert (erf (erfinv (single ([-0.9 -0.3 0 0.4 0.8]))), single ([-0.9 -0.3 0 0.4 0.8]), 1e-8)
%% tail region
%!assert (erf (erfinv ([-0.999 -0.99 0.9999 0.99999])), [-0.999 -0.99 0.9999 0.99999], eps)
%!assert (erf (erfinv (single ([-0.999 -0.99 0.9999 0.99999]))), single ([-0.999 -0.99 0.9999 0.99999]), 1e-8)
%% backward - loss of accuracy
%!assert (erfinv (erf ([-3 -1 -0.4 0.7 1.3 2.8])), [-3 -1 -0.4 0.7 1.3 2.8], -1e-12)
%!assert (erfinv (erf (single ([-3 -1 -0.4 0.7 1.3 2.8]))), single ([-3 -1 -0.4 0.7 1.3 2.8]), -1e-4)
%% exceptional
%!assert (erfinv ([-1, 1, 1.1, -2.1]), [-Inf, Inf, NaN, NaN])
%!error erfinv (1+2i)
*/

DEFUN (erfc, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} erfc (@var{z})\n\
Compute the complementary error function,\n\
@tex\n\
$1 - {\\rm erf} (z)$.\n\
@end tex\n\
@ifnottex\n\
@w{@code{1 - erf (@var{z})}}.\n\
@end ifnottex\n\
@seealso{erfcx, erf, erfinv}\n\
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

DEFUN (erfcx, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} erfcx (@var{z})\n\
Compute the scaled complementary error function,\n\
@tex\n\
$$\n\
 e^{z^2} {\\rm erfc} (z) \\equiv e^{z^2} (1 - {\\rm erf} (z))\n\
$$\n\
@end tex\n\
@ifnottex\n\
\n\
@example\n\
exp (z^2) * erfc (x)\n\
@end example\n\
\n\
@end ifnottex\n\
@seealso{erfc, erf, erfinv}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).erfcx ();
  else
    print_usage ();

  return retval;
}

/*

*/

DEFUN (exp, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} exp (@var{x})\n\
Compute\n\
@tex\n\
$e^{x}$\n\
@end tex\n\
@ifnottex\n\
@code{e^x}\n\
@end ifnottex\n\
for each element of @var{x}.  To compute the matrix\n\
exponential, see @ref{Linear Algebra}.\n\
@seealso{log}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).exp ();
  else
    print_usage ();

  return retval;
}

/*

%!assert(exp ([0, 1, -1, -1000]), [1, e, 1/e, 0], sqrt (eps));
%!assert(exp (1+i), e * (cos (1) + sin (1) * i), sqrt (eps));
%!assert(exp (single([0, 1, -1, -1000])), single([1, e, 1/e, 0]), sqrt (eps('single')));
%!assert(exp (single(1+i)), single (e * (cos (1) + sin (1) * i)), sqrt (eps('single')));

%!error exp ();
%!error exp (1, 2);

%!assert(exp (Inf) == Inf && exp (-Inf) == 0 && isnan (exp (NaN)));
%!assert(exp (Inf ('single')) == Inf('single') && exp (-Inf('single')) == 0 && isnan (exp (NaN('single'))));

*/

DEFUN (expm1, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} expm1 (@var{x})\n\
Compute\n\
@tex\n\
$ e^{x} - 1 $\n\
@end tex\n\
@ifnottex\n\
@code{exp (@var{x}) - 1}\n\
@end ifnottex\n\
accurately in the neighborhood of zero.\n\
@seealso{exp}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).expm1 ();
  else
    print_usage ();

  return retval;
}

DEFUN (isfinite, args, ,
    "-*- texinfo -*-\n\
@deftypefn  {Mapping Function} {} isfinite (@var{x})\n\
@deftypefnx {Mapping Function} {} finite (@var{x})\n\
Return a logical array which is true where the elements of @var{x} are\n\
finite values and false where they are not.\n\
For example:\n\
\n\
@example\n\
@group\n\
finite ([13, Inf, NA, NaN])\n\
     @result{} [ 1, 0, 0, 0 ]\n\
@end group\n\
@end example\n\
@seealso{isinf, isnan, isna}\n\
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
Truncate fractional portion of @var{x} and return the integer portion.  This\n\
is equivalent to rounding towards zero.  If @var{x} is complex, return\n\
@code{fix (real (@var{x})) + fix (imag (@var{x})) * I}.\n\
\n\
@example\n\
@group\n\
fix ([-2.7, 2.7])\n\
   @result{} -2   2\n\
@end group\n\
@end example\n\
@seealso{ceil, floor, round}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).fix ();
  else
    print_usage ();

  return retval;
}

/*

%!assert(fix ([1.1, 1, -1.1, -1]), [1, 1, -1, -1]);
%!assert(fix ([1.1+1.1i, 1+i, -1.1-1.1i, -1-i]), [1+i, 1+i, -1-i, -1-i]);
%!assert(fix (single([1.1, 1, -1.1, -1])), single([1, 1, -1, -1]));
%!assert(fix (single([1.1+1.1i, 1+i, -1.1-1.1i, -1-i])), single([1+i, 1+i, -1-i, -1-i]));

%!error fix ();
%!error fix (1, 2);

*/

DEFUN (floor, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} floor (@var{x})\n\
Return the largest integer not greater than @var{x}.  This is equivalent to\n\
rounding towards negative infinity.  If @var{x} is\n\
complex, return @code{floor (real (@var{x})) + floor (imag (@var{x})) * I}.\n\
\n\
@example\n\
@group\n\
floor ([-2.7, 2.7])\n\
     @result{} -3   2\n\
@end group\n\
@end example\n\
@seealso{ceil, round, fix}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).floor ();
  else
    print_usage ();

  return retval;
}

/*

%!assert(floor ([2, 1.1, -1.1, -1]), [2, 1, -2, -1]);
%!assert(floor ([2+2i, 1.1+1.1i, -1.1-1.1i, -1-i]), [2+2i, 1+i, -2-2i, -1-i]);
%!assert(floor (single ([2, 1.1, -1.1, -1])), single ([2, 1, -2, -1]));
%!assert(floor (single([2+2i, 1.1+1.1i, -1.1-1.1i, -1-i])), single([2+2i, 1+i, -2-2i, -1-i]));

%!error floor ();
%!error floor (1, 2);

*/

DEFUN (gamma, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} gamma (@var{z})\n\
Compute the Gamma function,\n\
@tex\n\
$$\n\
 \\Gamma (z) = \\int_0^\\infty t^{z-1} e^{-t} dt.\n\
$$\n\
@end tex\n\
@ifnottex\n\
\n\
@example\n\
@group\n\
@c spacing appears odd here, but is correct after Makeinfo\n\
              infinity\n\
             /\n\
gamma (z) = | t^(z-1) exp (-t) dt.\n\
             /\n\
          t=0\n\
@end group\n\
@end example\n\
\n\
@end ifnottex\n\
@seealso{gammainc, lgamma}\n\
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

%!test
%! x = [.5, 1, 1.5, 2, 3, 4, 5];
%! v = [sqrt(pi), 1, .5*sqrt(pi), 1, 2, 6, 24];
%! assert(gamma(x), v, sqrt(eps))

%!test
%! a = single(-1i*sqrt(-1/(6.4187*6.4187)));
%! assert (gamma(a), gamma(real(a)));

%!test
%! x = single([.5, 1, 1.5, 2, 3, 4, 5]);
%! v = single([sqrt(pi), 1, .5*sqrt(pi), 1, 2, 6, 24]);
%! assert(gamma(x), v, sqrt(eps('single')))

%!test
%! x = [-1, 0, 1, Inf];
%! v = [Inf, Inf, 1, Inf];
%! assert (gamma(x), v);
%! assert (gamma(single (x)), single (v));

%!error gamma();
%!error gamma(1,2);

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

/*

%!assert(imag (1), 0);
%!assert(imag (i), 1);
%!assert(imag (1+i), 1);
%!assert(imag ([i, 1; 1, i]), full (eye (2)));

%!assert(imag (single(1)), single(0));
%!assert(imag (single(i)), single(1));
%!assert(imag (single(1+i)), single(1));
%!assert(imag (single([i, 1; 1, i])), full (eye (2,'single')));

%!error imag ();
%!error imag (1, 2);

 */

DEFUNX ("isalnum", Fisalnum, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} isalnum (@var{s})\n\
Return a logical array which is true where the elements of @var{s} are\n\
letters or digits and false where they are not.  This is equivalent to\n\
(@code{isalpha (@var{s}) | isdigit (@var{s})}).\n\
@seealso{isalpha, isdigit, ispunct, isspace, iscntrl}\n\
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
Return a logical array which is true where the elements of @var{s} are\n\
letters and false where they are not.  This is equivalent to\n\
(@code{islower (@var{s}) | isupper (@var{s})}).\n\
@seealso{isdigit, ispunct, isspace, iscntrl, isalnum, islower, isupper}\n\
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
Return a logical array which is true where the elements of @var{s} are\n\
ASCII characters (in the range 0 to 127 decimal) and false where they are\n\
not.\n\
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
Return a logical array which is true where the elements of @var{s} are\n\
control characters and false where they are not.\n\
@seealso{ispunct, isspace, isalpha, isdigit}\n\
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
Return a logical array which is true where the elements of @var{s} are\n\
decimal digits (0-9) and false where they are not.\n\
@seealso{isxdigit, isalpha, isletter, ispunct, isspace, iscntrl}\n\
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
Return a logical array which is true where the elements of @var{x} are\n\
are infinite and false where they are not.\n\
For example:\n\
\n\
@example\n\
@group\n\
isinf ([13, Inf, NA, NaN])\n\
     @result{} [ 0, 1, 0, 0 ]\n\
@end group\n\
@end example\n\
@seealso{isfinite, isnan, isna}\n\
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
Return a logical array which is true where the elements of @var{s} are\n\
printable characters (but not the space character) and false where they are\n\
not.\n\
@seealso{isprint}\n\
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
Return a logical array which is true where the elements of @var{s} are\n\
lowercase letters and false where they are not.\n\
@seealso{isupper, isalpha, isletter, isalnum}\n\
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
Return a logical array which is true where the elements of @var{x} are\n\
NA (missing) values and false where they are not.\n\
For example:\n\
\n\
@example\n\
@group\n\
isna ([13, Inf, NA, NaN])\n\
     @result{} [ 0, 0, 1, 0 ]\n\
@end group\n\
@end example\n\
@seealso{isnan, isinf, isfinite}\n\
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
Return a logical array which is true where the elements of @var{x} are\n\
NaN values and false where they are not.\n\
NA values are also considered NaN values.  For example:\n\
\n\
@example\n\
@group\n\
isnan ([13, Inf, NA, NaN])\n\
     @result{} [ 0, 0, 1, 1 ]\n\
@end group\n\
@end example\n\
@seealso{isna, isinf, isfinite}\n\
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
Return a logical array which is true where the elements of @var{s} are\n\
printable characters (including the space character) and false where they\n\
are not.\n\
@seealso{isgraph}\n\
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
Return a logical array which is true where the elements of @var{s} are\n\
punctuation characters and false where they are not.\n\
@seealso{isalpha, isdigit, isspace, iscntrl}\n\
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
Return a logical array which is true where the elements of @var{s} are\n\
whitespace characters (space, formfeed, newline, carriage return, tab, and\n\
vertical tab) and false where they are not.\n\
@seealso{iscntrl, ispunct, isalpha, isdigit}\n\
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
Return a logical array which is true where the elements of @var{s} are\n\
uppercase letters and false where they are not.\n\
@seealso{islower, isalpha, isletter, isalnum}\n\
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
Return a logical array which is true where the elements of @var{s} are\n\
hexadecimal digits (0-9 and @nospell{a-fA-F}).\n\
@seealso{isdigit}\n\
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
@deftypefn  {Mapping Function} {} lgamma (@var{x})\n\
@deftypefnx {Mapping Function} {} gammaln (@var{x})\n\
Return the natural logarithm of the gamma function of @var{x}.\n\
@seealso{gamma, gammainc}\n\
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

%!test
%! x = [.5, 1, 1.5, 2, 3, 4, 5];
%! v = [sqrt(pi), 1, .5*sqrt(pi), 1, 2, 6, 24];
%! assert(lgamma(x), log(v), sqrt(eps))

%!test
%! a = single(-1i*sqrt(-1/(6.4187*6.4187)));
%! assert (lgamma(a), lgamma(real(a)));

%!test
%! x = single([.5, 1, 1.5, 2, 3, 4, 5]);
%! v = single([sqrt(pi), 1, .5*sqrt(pi), 1, 2, 6, 24]);
%! assert(lgamma(x), log(v), sqrt(eps ('single')))

%!test
%! x = [-1, 0, 1, Inf];
%! v = [Inf, Inf, 0, Inf];
%! assert (lgamma(x), v);
%! assert (lgamma(single (x)), single(v));

%!error lgamma();
%!error lgamma(1,2);

*/

DEFUN (log, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} log (@var{x})\n\
Compute the natural logarithm,\n\
@tex\n\
$\\ln{(x)},$\n\
@end tex\n\
@ifnottex\n\
@code{ln (@var{x})},\n\
@end ifnottex\n\
for each element of @var{x}.  To compute the\n\
matrix logarithm, see @ref{Linear Algebra}.\n\
@seealso{exp, log1p, log2, log10, logspace}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).log ();
  else
    print_usage ();

  return retval;
}

/*

%!assert(log ([1, e, e^2]), [0, 1, 2], sqrt (eps));
%!assert(log ([-0.5, -1.5, -2.5]), log([0.5, 1.5, 2.5]) + pi*1i, sqrt (eps));

%!assert(log (single([1, e, e^2])), single([0, 1, 2]), sqrt (eps('single')));
%!assert(log (single([-0.5, -1.5, -2.5])), single(log([0.5, 1.5, 2.5]) + pi*1i), 4*eps('single'));

%!error log ();
%!error log (1, 2);

 */

DEFUN (log10, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} log10 (@var{x})\n\
Compute the base-10 logarithm of each element of @var{x}.\n\
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

/*

%!assert(log10 ([0.01, 0.1, 1, 10, 100]), [-2, -1, 0, 1, 2], sqrt (eps));
%!assert(log10 (single([0.01, 0.1, 1, 10, 100])), single([-2, -1, 0, 1, 2]), sqrt (eps ('single')));

%!error log10 ();
%!error log10 (1, 2);

*/

DEFUN (log1p, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} log1p (@var{x})\n\
Compute\n\
@tex\n\
$\\ln{(1 + x)}$\n\
@end tex\n\
@ifnottex\n\
@code{log (1 + @var{x})}\n\
@end ifnottex\n\
accurately in the neighborhood of zero.\n\
@seealso{log, exp, expm1}\n\
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

/*

%!assert(real (1), 1);
%!assert(real (i), 0);
%!assert(real (1+i), 1);
%!assert(real ([1, i; i, 1]), full (eye (2)));

%!assert(real (single(1)), single(1));
%!assert(real (single(i)), single(0));
%!assert(real (single(1+i)), single(1));
%!assert(real (single([1, i; i, 1])), full (eye (2,'single')));

%!error real ();
%!error real (1, 2);

*/

DEFUN (round, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} round (@var{x})\n\
Return the integer nearest to @var{x}.  If @var{x} is complex, return\n\
@code{round (real (@var{x})) + round (imag (@var{x})) * I}. If there\n\
are two nearest integers, return the one further away from zero.\n\
\n\
@example\n\
@group\n\
round ([-2.7, 2.7])\n\
     @result{} -3   3\n\
@end group\n\
@end example\n\
@seealso{ceil, floor, fix, roundb}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).round ();
  else
    print_usage ();

  return retval;
}

/*

%!assert(round (1), 1);
%!assert(round (1.1), 1);
%!assert(round (5.5), 6);
%!assert(round (i), i);
%!assert(round (2.5+3.5i), 3+4i);
%!assert(round (-2.6), -3);
%!assert(round ([1.1, -2.4; -3.7, 7.1]), [1, -2; -4, 7]);

%!assert(round (single(1)), single(1));
%!assert(round (single(1.1)), single(1));
%!assert(round (single(5.5)), single(6));
%!assert(round (single(i)), single(i));
%!assert(round (single(2.5+3.5i)), single(3+4i));
%!assert(round (single(-2.6)), single(-3));
%!assert(round (single([1.1, -2.4; -3.7, 7.1])), single([1, -2; -4, 7]));

%!error round ();
%!error round (1, 2);

*/

DEFUN (roundb, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} roundb (@var{x})\n\
Return the integer nearest to @var{x}.  If there are two nearest\n\
integers, return the even one (banker's rounding).  If @var{x} is complex,\n\
return @code{roundb (real (@var{x})) + roundb (imag (@var{x})) * I}.\n\
@seealso{round}\n\
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
@tex\n\
$$\n\
{\\rm sign} (@var{x}) = \\cases{1,&$x>0$;\\cr 0,&$x=0$;\\cr -1,&$x<0$.\\cr}\n\
$$\n\
@end tex\n\
@ifnottex\n\
\n\
@example\n\
@group\n\
           -1, x < 0;\n\
sign (x) =  0, x = 0;\n\
            1, x > 0.\n\
@end group\n\
@end example\n\
\n\
@end ifnottex\n\
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

/*

%!assert(sign (-2) , -1);
%!assert(sign (3), 1);
%!assert(sign (0), 0);
%!assert(sign ([1, -pi; e, 0]), [1, -1; 1, 0]);

%!assert(sign (single(-2)) , single(-1));
%!assert(sign (single(3)), single(1));
%!assert(sign (single(0)), single(0));
%!assert(sign (single([1, -pi; e, 0])), single([1, -1; 1, 0]));

%!error sign ();
%!error sign (1, 2);

*/

DEFUN (sin, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} sin (@var{x})\n\
Compute the sine for each element of @var{x} in radians.\n\
@seealso{asin, sind, sinh}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).sin ();
  else
    print_usage ();

  return retval;
}

/*

%!test
%! rt2 = sqrt (2);
%! rt3 = sqrt (3);
%! x = [0, pi/6, pi/4, pi/3, pi/2, 2*pi/3, 3*pi/4, 5*pi/6, pi];
%! v = [0, 1/2, rt2/2, rt3/2, 1, rt3/2, rt2/2, 1/2, 0];
%! assert(sin (x), v, sqrt (eps));

%!test
%! rt2 = sqrt (2);
%! rt3 = sqrt (3);
%! x = single([0, pi/6, pi/4, pi/3, pi/2, 2*pi/3, 3*pi/4, 5*pi/6, pi]);
%! v = single([0, 1/2, rt2/2, rt3/2, 1, rt3/2, rt2/2, 1/2, 0]);
%! assert(sin (x), v, sqrt (eps('single')));

%!error sin ();
%!error sin (1, 2);

*/

DEFUN (sinh, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} sinh (@var{x})\n\
Compute the hyperbolic sine for each element of @var{x}.\n\
@seealso{asinh, cosh, tanh}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).sinh ();
  else
    print_usage ();

  return retval;
}

/*

%!test
%! x = [0, pi/2*i, pi*i, 3*pi/2*i];
%! v = [0, i, 0, -i];
%! assert(sinh (x), v, sqrt (eps));

%!test
%! x = single([0, pi/2*i, pi*i, 3*pi/2*i]);
%! v = single([0, i, 0, -i]);
%! assert(sinh (x), v, sqrt (eps('single')));

%!error sinh ();
%!error sinh (1, 2);

 */

DEFUN (sqrt, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} sqrt (@var{x})\n\
Compute the square root of each element of @var{x}.  If @var{x} is negative,\n\
a complex result is returned.  To compute the matrix square root, see\n\
@ref{Linear Algebra}.\n\
@seealso{realsqrt, nthroot}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).sqrt ();
  else
    print_usage ();

  return retval;
}

/*

%!assert(sqrt (4), 2)
%!assert(sqrt (-1), i)
%!assert(sqrt (1+i), exp (0.5 * log (1+i)), sqrt (eps));
%!assert(sqrt([4, -4; i, 1-i]), [2, 2i; exp(0.5 * log (i)), exp(0.5 * log (1-i))], sqrt(eps));

%!assert(sqrt (single(4)), single(2))
%!assert(sqrt (single(-1)), single(i))
%!assert(sqrt (single(1+i)), single(exp (0.5 * log (1+i))), sqrt (eps('single')));
%!assert(sqrt(single([4, -4; i, 1-i])), single([2, 2i; exp(0.5 * log (i)), exp(0.5 * log (1-i))]), sqrt(eps('single')));

%!error sqrt ();
%!error sqrt (1, 2);

*/

DEFUN (tan, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} tan (@var{z})\n\
Compute the tangent for each element of @var{x} in radians.\n\
@seealso{atan, tand, tanh}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).tan ();
  else
    print_usage ();

  return retval;
}

/*

%!test
%! rt2 = sqrt (2);
%! rt3 = sqrt (3);
%! x = [0, pi/6, pi/4, pi/3, 2*pi/3, 3*pi/4, 5*pi/6, pi];
%! v = [0, rt3/3, 1, rt3, -rt3, -1, -rt3/3, 0];
%! assert(tan (x), v,  sqrt (eps));

%!test
%! rt2 = sqrt (2);
%! rt3 = sqrt (3);
%! x = single([0, pi/6, pi/4, pi/3, 2*pi/3, 3*pi/4, 5*pi/6, pi]);
%! v = single([0, rt3/3, 1, rt3, -rt3, -1, -rt3/3, 0]);
%! assert(tan (x), v,  sqrt (eps('single')));

%!error tan ();
%!error tan (1, 2);

*/

DEFUN (tanh, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} tanh (@var{x})\n\
Compute hyperbolic tangent for each element of @var{x}.\n\
@seealso{atanh, sinh, cosh}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).tanh ();
  else
    print_usage ();

  return retval;
}

/*

%!test
%! x = [0, pi*i];
%! v = [0, 0];
%! assert(tanh (x), v, sqrt (eps));

%!test
%! x = single([0, pi*i]);
%! v = single([0, 0]);
%! assert(tanh (x), v, sqrt (eps('single')));

%!error tanh ();
%!error tanh (1, 2);

*/

DEFUNX ("toascii", Ftoascii, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} toascii (@var{s})\n\
Return ASCII representation of @var{s} in a matrix.  For example:\n\
\n\
@example\n\
@group\n\
toascii (\"ASCII\")\n\
     @result{} [ 65, 83, 67, 73, 73 ]\n\
@end group\n\
\n\
@end example\n\
@seealso{char}\n\
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
@deftypefn  {Mapping Function} {} tolower (@var{s})\n\
@deftypefnx {Mapping Function} {} lower (@var{s})\n\
Return a copy of the string or cell string @var{s}, with each uppercase\n\
character replaced by the corresponding lowercase one; non-alphabetic\n\
characters are left unchanged.  For example:\n\
\n\
@example\n\
@group\n\
tolower (\"MiXeD cAsE 123\")\n\
     @result{} \"mixed case 123\"\n\
@end group\n\
@end example\n\
@seealso{toupper}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).xtolower ();
  else
    print_usage ();

  return retval;
}

DEFALIAS (lower, tolower);

/*

%!error <Invalid call to tolower.*> tolower();
%!error <Invalid call to tolower.*> lower();
%!assert(tolower("OCTAVE"), "octave");
%!assert(tolower("123OCTave!_&"), "123octave!_&");
%!assert(tolower({"ABC", "DEF", {"GHI", {"JKL"}}}), {"abc", "def", {"ghi", {"jkl"}}});
%!assert(tolower(["ABC"; "DEF"]), ["abc"; "def"]);
%!assert(tolower({["ABC"; "DEF"]}), {["abc";"def"]});
%!assert(tolower(68), "d");
%!assert(tolower({[68, 68; 68, 68]}), {["dd";"dd"]});
%!test
%!  a(3,3,3,3) = "D";
%!  assert(tolower(a)(3,3,3,3), "d");

*/


DEFUNX ("toupper", Ftoupper, args, ,
    "-*- texinfo -*-\n\
@deftypefn  {Mapping Function} {} toupper (@var{s})\n\
@deftypefnx {Mapping Function} {} upper (@var{s})\n\
Return a copy of the string or cell string @var{s}, with each lowercase\n\
character replaced by the corresponding uppercase one; non-alphabetic\n\
characters are left unchanged.  For example:\n\
\n\
@example\n\
@group\n\
toupper (\"MiXeD cAsE 123\")\n\
     @result{} \"MIXED CASE 123\"\n\
@end group\n\
@end example\n\
@seealso{tolower}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).xtoupper ();
  else
    print_usage ();

  return retval;
}

DEFALIAS (upper, toupper);

/*

%!error <Invalid call to toupper.*> toupper();
%!error <Invalid call to toupper.*> upper();
%!assert(toupper("octave"), "OCTAVE");
%!assert(toupper("123OCTave!_&"), "123OCTAVE!_&");
%!assert(toupper({"abc", "def", {"ghi", {"jkl"}}}), {"ABC", "DEF", {"GHI", {"JKL"}}});
%!assert(toupper(["abc"; "def"]), ["ABC"; "DEF"]);
%!assert(toupper({["abc"; "def"]}), {["ABC";"DEF"]});
%!assert(toupper(100), "D");
%!assert(toupper({[100, 100; 100, 100]}), {["DD";"DD"]});
%!test
%!  a(3,3,3,3) = "d";
%!  assert(toupper(a)(3,3,3,3), "D");

*/

DEFALIAS (gammaln, lgamma);

DEFALIAS (finite, isfinite);
