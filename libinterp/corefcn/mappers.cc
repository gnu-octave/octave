////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1993-2023 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <cctype>

#include "lo-ieee.h"
#include "lo-specfun.h"
#include "lo-mappers.h"

#include "defun.h"
#include "error.h"
#include "variables.h"

OCTAVE_BEGIN_NAMESPACE(octave)

DEFUN (abs, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{z} =} abs (@var{x})
Compute the magnitude of @var{x}.

The magnitude is defined as
@tex
$|z| = \sqrt{x^2 + y^2}$.
@end tex
@ifnottex
|@var{z}| = @code{sqrt (x^2 + y^2)}.
@end ifnottex

For example:

@example
@group
abs (3 + 4i)
     @result{} 5
@end group
@end example
@seealso{arg}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).abs ());
}

/*
%!assert (abs (1), 1)
%!assert (abs (-3.5), 3.5)
%!assert (abs (3+4i), 5)
%!assert (abs (3-4i), 5)
%!assert (abs ([1.1, 3i; 3+4i, -3-4i]), [1.1, 3; 5, 5])

%!assert (abs (single (1)), single (1))
%!assert (abs (single (-3.5)), single (3.5))
%!assert (abs (single (3+4i)), single (5))
%!assert (abs (single (3-4i)), single (5))
%!assert (abs (single ([1.1, 3i; 3+4i, -3-4i])), single ([1.1, 3; 5, 5]))

%!error abs ()
%!error abs (1, 2)
*/

DEFUN (acos, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{y} =} acos (@var{x})
Compute the inverse cosine in radians for each element of @var{x}.
@seealso{cos, acosd}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).acos ());
}

/*
%!shared rt2, rt3
%! rt2 = sqrt (2);
%! rt3 = sqrt (3);

%!test
%! x = [1, rt3/2, rt2/2, 1/2, 0, -1/2, -rt2/2, -rt3/2, -1];
%! v = [0, pi/6, pi/4, pi/3, pi/2, 2*pi/3, 3*pi/4, 5*pi/6, pi];
%! assert (acos (x), v, sqrt (eps));

%!test
%! x = single ([1, rt3/2, rt2/2, 1/2, 0, -1/2, -rt2/2, -rt3/2, -1]);
%! v = single ([0, pi/6, pi/4, pi/3, pi/2, 2*pi/3, 3*pi/4, 5*pi/6, pi]);
%! assert (acos (x), v, sqrt (eps ("single")));

## Test values on either side of branch cut
%!test
%! rval = 0;
%! ival = 1.31695789692481671;
%! obs = acos ([2, 2-i*eps, 2+i*eps]);
%! exp = [rval + ival*i, rval + ival*i, rval - ival*i];
%! assert (obs, exp, 3*eps);
%! rval = pi;
%! obs = acos ([-2, -2-i*eps, -2+i*eps]);
%! exp = [rval - ival*i, rval + ival*i, rval - ival*i];
%! assert (obs, exp, 5*eps);
%! assert (acos ([2 0]),  [ival*i, pi/2], 3*eps);
%! assert (acos ([2 0i]), [ival*i, pi/2], 3*eps);

## Test large magnitude arguments (bug #45507)
## Test fails with older versions of libm, solution is to upgrade.
%!testif ; ! __have_feature__ ("LLVM_LIBCXX") && ! ispc ()  <*45507>
%! x = [1, -1, i, -i] .* 1e150;
%! v = [0, pi, pi/2, pi/2];
%! assert (real (acos (x)), v);

%!testif ; __have_feature__ ("LLVM_LIBCXX") || ispc ()  <52627>
%! ## Same test code as above, but intended for test statistics with libc++ or
%! ## on Windows.  Their trig/hyperbolic functions have huge tolerances.
%! x = [1, -1, i, -i] .* 1e150;
%! v = [0, pi, pi/2, pi/2];
%! assert (real (acos (x)), v);

%!error acos ()
%!error acos (1, 2)
*/

DEFUN (acosh, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{y} =} acosh (@var{x})
Compute the inverse hyperbolic cosine for each element of @var{x}.
@seealso{cosh}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).acosh ());
}

/*
%!testif ; ! ismac ()
%! x = [1, 0, -1, 0];
%! v = [0, pi/2*i, pi*i, pi/2*i];
%! assert (acosh (x), v, sqrt (eps));

%!testif ; ismac ()   <52627>
%! ## Same test code as above, but intended only for test statistics on Mac.
%! ## Mac trig/hyperbolic functions have huge tolerances.
%! x = [1, 0, -1, 0];
%! v = [0, pi/2*i, pi*i, pi/2*i];
%! assert (acosh (x), v, sqrt (eps));

## FIXME: std::acosh on Windows platforms, returns a result that differs
## by 1 in the last significant digit.  This is ~30*eps which is quite large.
## The decision now (9/15/2016) is to mark the test with a bug number so
## it is understood why it is failing, and wait for MinGw to improve their
## std library.
%!test <49091>
%! re = 2.99822295029797;
%! im = pi/2;
%! assert (acosh (-10i), re - i*im);

%!testif ; ! ismac ()
%! x = single ([1, 0, -1, 0]);
%! v = single ([0, pi/2*i, pi*i, pi/2*i]);
%! assert (acosh (x), v, sqrt (eps ("single")));

%!testif ; ismac ()   <52627>
%! ## Same test code as above, but intended only for test statistics on Mac.
%! ## Mac trig/hyperbolic functions have huge tolerances.
%! x = single ([1, 0, -1, 0]);
%! v = single ([0, pi/2*i, pi*i, pi/2*i]);
%! assert (acosh (x), v, sqrt (eps ("single")));

%!test <49091>
%! re = single (2.99822295029797);
%! im = single (pi/2);
%! assert (acosh (single (10i)), re + i*im, 5* eps ("single"));
%! assert (acosh (single (-10i)), re - i*im, 5* eps ("single"));

## Test large magnitude arguments (bug #45507)
## Test fails with older versions of libm, solution is to upgrade.
%!testif ; ! __have_feature__ ("LLVM_LIBCXX") && ! ispc ()  <*45507>
%! x = [1, -1, i, -i] .* 1e150;
%! v = [0, pi, pi/2, -pi/2];
%! assert (imag (acosh (x)), v);

%!testif ; __have_feature__ ("LLVM_LIBCXX") || ispc ()  <52627>
%! ## Same test code as above, but intended for test statistics with libc++ or
%! ## on Windows.  Their trig/hyperbolic functions have huge tolerances.
%! x = [1, -1, i, -i] .* 1e150;
%! v = [0, pi, pi/2, -pi/2];
%! assert (imag (acosh (x)), v);

%!error acosh ()
%!error acosh (1, 2)
*/

DEFUN (angle, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{theta} =} angle (@var{z})
@xref{XREFarg,,@code{arg}}.
@seealso{arg}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).arg ());
}

DEFUN (arg, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{theta} =} arg (@var{z})
@deftypefnx {} {@var{theta} =} angle (@var{z})
Compute the argument, i.e., angle of @var{z}.

This is defined as,
@tex
$\theta = atan2 (y, x),$
@end tex
@ifnottex
@var{theta} = @code{atan2 (@var{y}, @var{x})},
@end ifnottex
in radians.

For example:

@example
@group
arg (3 + 4i)
     @result{} 0.92730
@end group
@end example
@seealso{abs}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).arg ());
}

/*
%!assert (arg (1), 0)
%!assert (arg (i), pi/2)
%!assert (arg (-1), pi)
%!assert (arg (-i), -pi/2)
%!assert (arg ([1, i; -1, -i]), [0, pi/2; pi, -pi/2])

%!assert (arg (single (1)), single (0))
%!assert (arg (single (i)), single (pi/2))
%!test
%! if (ismac ())
%!   ## Avoid failing for a MacOS feature
%!   assert (arg (single (-1)), single (pi), 2* eps (single (1)));
%! else
%!   assert (arg (single (-1)), single (pi));
%! endif
%!assert (arg (single (-i)), single (-pi/2))
%!assert (arg (single ([1, i; -1, -i])),
%!        single ([0, pi/2; pi, -pi/2]), 2e1* eps ("single"))

%!error arg ()
%!error arg (1, 2)
*/

DEFUN (asin, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{y} =} asin (@var{x})
Compute the inverse sine in radians for each element of @var{x}.
@seealso{sin, asind}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).asin ());
}

/*
%!shared rt2, rt3
%! rt2 = sqrt (2);
%! rt3 = sqrt (3);

%!test
%! x = [0, 1/2, rt2/2, rt3/2, 1, rt3/2, rt2/2, 1/2, 0];
%! v = [0, pi/6, pi/4, pi/3, pi/2, pi/3, pi/4, pi/6, 0];
%! assert (asin (x), v, sqrt (eps));

%!test
%! x = single ([0, 1/2, rt2/2, rt3/2, 1, rt3/2, rt2/2, 1/2, 0]);
%! v = single ([0, pi/6, pi/4, pi/3, pi/2, pi/3, pi/4, pi/6, 0]);
%! assert (asin (x), v, sqrt (eps ("single")));

## Test values on either side of branch cut
%!test
%! rval = pi/2;
%! ival = 1.31695789692481635;
%! obs = asin ([2, 2-i*eps, 2+i*eps]);
%! exp = [rval - ival*i, rval - ival*i, rval + ival*i];
%! if (ismac ())
%!   ## Math libraries on macOS seem to implement asin with less accuracy.
%!   tol = 6*eps;
%! else
%!   tol = 2*eps;
%! endif
%! assert (obs, exp, tol);
%! obs = asin ([-2, -2-i*eps, -2+i*eps]);
%! exp = [-rval + ival*i, -rval - ival*i, -rval + ival*i];
%! assert (obs, exp, tol);
%! assert (asin ([2 0]),  [rval - ival*i, 0], tol);
%! assert (asin ([2 0i]), [rval - ival*i, 0], tol);

## Test large magnitude arguments (bug #45507)
## Test fails with older versions of libm, solution is to upgrade.
%!testif ; ! __have_feature__ ("LLVM_LIBCXX") && ! ispc ()  <*45507>
%! x = [1, -1, i, -i] .* 1e150;
%! v = [pi/2, -pi/2, 0, -0];
%! assert (real (asin (x)), v);

%!testif ; __have_feature__ ("LLVM_LIBCXX") || ispc ()  <52627>
%! ## Same test code as above, but intended for test statistics with libc++ or
%! ## on Windows.  Their trig/hyperbolic functions have huge tolerances.
%! x = [1, -1, i, -i] .* 1e150;
%! v = [pi/2, -pi/2, 0, -0];
%! assert (real (asin (x)), v);

%!error asin ()
%!error asin (1, 2)
*/

DEFUN (asinh, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{y} =} asinh (@var{x})
Compute the inverse hyperbolic sine for each element of @var{x}.
@seealso{sinh}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).asinh ());
}

/*
%!test
%! v = [0, pi/2*i, 0, -pi/2*i];
%! x = [0, i, 0, -i];
%! assert (asinh (x), v,  sqrt (eps));

%!test
%! v = single ([0, pi/2*i, 0, -pi/2*i]);
%! x = single ([0, i, 0, -i]);
%! assert (asinh (x), v,  sqrt (eps ("single")));

## Test large magnitude arguments (bug #45507)
## Test fails with older versions of libm, solution is to upgrade.
%!testif ; ! __have_feature__ ("LLVM_LIBCXX") && ! ispc ()  <*45507>
%! x = [1, -1, i, -i] .* 1e150;
%! v = [0, 0, pi/2, -pi/2];
%! assert (imag (asinh (x)), v);

%!testif ; __have_feature__ ("LLVM_LIBCXX") || ispc ()  <52627>
%! ## Same test code as above, but intended for test statistics with libc++ or
%! ## on Windows.  Their trig/hyperbolic functions have huge tolerances.
%! x = [1, -1, i, -i] .* 1e150;
%! v = [0, 0, pi/2, -pi/2];
%! assert (imag (asinh (x)), v);

%!error asinh ()
%!error asinh (1, 2)
*/

DEFUN (atan, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{y} =} atan (@var{x})
Compute the inverse tangent in radians for each element of @var{x}.
@seealso{tan, atand}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).atan ());
}

/*
%!shared rt2, rt3
%! rt2 = sqrt (2);
%! rt3 = sqrt (3);

%!test
%! v = [0, pi/6, pi/4, pi/3, -pi/3, -pi/4, -pi/6, 0];
%! x = [0, rt3/3, 1, rt3, -rt3, -1, -rt3/3, 0];
%! assert (atan (x), v, sqrt (eps));

%!test
%! v = single ([0, pi/6, pi/4, pi/3, -pi/3, -pi/4, -pi/6, 0]);
%! x = single ([0, rt3/3, 1, rt3, -rt3, -1, -rt3/3, 0]);
%! assert (atan (x), v, sqrt (eps ("single")));

## Test large magnitude arguments (bug #44310, bug #45507)
%!test <*44310>
%! x = [1, -1, i, -i] .* 1e150;
%! v = [pi/2, -pi/2, pi/2, -pi/2];
%! assert (real (atan (x)), v);
%! assert (imag (atan (x)), [0, 0, 0, 0], eps);

%!error atan ()
%!error atan (1, 2)
*/

DEFUN (atanh, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{y} =} atanh (@var{x})
Compute the inverse hyperbolic tangent for each element of @var{x}.
@seealso{tanh}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).atanh ());
}

/*
%!test
%! v = [0, 0];
%! x = [0, 0];
%! assert (atanh (x), v, sqrt (eps));

%!test
%! v = single ([0, 0]);
%! x = single ([0, 0]);
%! assert (atanh (x), v, sqrt (eps ("single")));

## Test large magnitude arguments (bug #44310, bug #45507)
%!test <*44310>
%! x = [1, -1, i, -i] .* 1e150;
%! v = [pi/2, pi/2, pi/2, -pi/2];
%! assert (imag (atanh (x)), v);
%! assert (real (atanh (x)), [0, 0, 0, 0], eps);

%!error atanh ()
%!error atanh (1, 2)
*/

DEFUN (cbrt, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{y} =} cbrt (@var{x})
Compute the real-valued cube root of each element of @var{x}.

Unlike @code{@var{x}^(1/3)}, the result will be negative if @var{x} is
negative.

If any element of @var{x} is complex, @code{cbrt} aborts with an error.
@seealso{nthroot}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).cbrt ());
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

%!error cbrt ()
%!error cbrt (1, 2)
*/

DEFUN (ceil, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{y} =} ceil (@var{x})
Return the smallest integer not less than @var{x}.

This is equivalent to rounding towards positive infinity.

If @var{x} is complex, return
@code{ceil (real (@var{x})) + ceil (imag (@var{x})) * I}.

@example
@group
ceil ([-2.7, 2.7])
    @result{} -2    3
@end group
@end example
@seealso{floor, round, fix}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).ceil ());
}

/*
## double precision
%!assert (ceil ([2, 1.1, -1.1, -1]), [2, 2, -1, -1])

## complex double precision
%!assert (ceil ([2+2i, 1.1+1.1i, -1.1-1.1i, -1-i]), [2+2i, 2+2i, -1-i, -1-i])

## single precision
%!assert (ceil (single ([2, 1.1, -1.1, -1])), single ([2, 2, -1, -1]))

## complex single precision
%!assert (ceil (single ([2+2i, 1.1+1.1i, -1.1-1.1i, -1-i])),
%!        single ([2+2i, 2+2i, -1-i, -1-i]))

%!error ceil ()
%!error ceil (1, 2)
*/

DEFUN (conj, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{zc} =} conj (@var{z})
Return the complex conjugate of @var{z}.

The complex conjugate is defined as
@tex
$\bar{z} = x - iy$.
@end tex
@ifnottex
@code{conj (@var{z})} = @var{x} - @var{i}@var{y}.
@end ifnottex
@seealso{real, imag}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).conj ());
}

/*
%!assert (conj (1), 1)
%!assert (conj (i), -i)
%!assert (conj (1+i), 1-i)
%!assert (conj (1-i), 1+i)
%!assert (conj ([-1, -i; -1+i, -1-i]), [-1, i; -1-i, -1+i])

%!assert (conj (single (1)), single (1))
%!assert (conj (single (i)), single (-i))
%!assert (conj (single (1+i)), single (1-i))
%!assert (conj (single (1-i)), single (1+i))
%!assert (conj (single ([-1, -i; -1+i, -1-i])), single ([-1, i; -1-i, -1+i]))

%!error conj ()
%!error conj (1, 2)
*/

DEFUN (cos, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{y} =} cos (@var{x})
Compute the cosine for each element of @var{x} in radians.
@seealso{acos, cosd, cosh}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).cos ());
}

/*
%!shared rt2, rt3
%! rt2 = sqrt (2);
%! rt3 = sqrt (3);

%!test
%! x = [0, pi/6, pi/4, pi/3, pi/2, 2*pi/3, 3*pi/4, 5*pi/6, pi];
%! v = [1, rt3/2, rt2/2, 1/2, 0, -1/2, -rt2/2, -rt3/2, -1];
%! assert (cos (x), v, sqrt (eps));

%!test
%! rt2 = sqrt (2);
%! rt3 = sqrt (3);
%! x = single ([0, pi/6, pi/4, pi/3, pi/2, 2*pi/3, 3*pi/4, 5*pi/6, pi]);
%! v = single ([1, rt3/2, rt2/2, 1/2, 0, -1/2, -rt2/2, -rt3/2, -1]);
%! assert (cos (x), v, sqrt (eps ("single")));

%!error cos ()
%!error cos (1, 2)
*/

DEFUN (cosh, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{y} =} cosh (@var{x})
Compute the hyperbolic cosine for each element of @var{x}.
@seealso{acosh, sinh, tanh}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).cosh ());
}

/*
%!test
%! x = [0, pi/2*i, pi*i, 3*pi/2*i];
%! v = [1, 0, -1, 0];
%! assert (cosh (x), v, sqrt (eps));

%!test
%! x = single ([0, pi/2*i, pi*i, 3*pi/2*i]);
%! v = single ([1, 0, -1, 0]);
%! assert (cosh (x), v, sqrt (eps ("single")));

%!error cosh ()
%!error cosh (1, 2)
*/

DEFUN (erf, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{v} =} erf (@var{z})
Compute the error function.

The error function is defined as
@tex
$$
 {\rm erf} (z) = {2 \over \sqrt{\pi}}\int_0^z e^{-t^2} dt
$$
@end tex
@ifnottex

@example
@group
                        z
              2        /
erf (z) = --------- *  | e^(-t^2) dt
          sqrt (pi)    /
                    t=0
@end group
@end example

@end ifnottex
@seealso{erfc, erfcx, erfi, dawson, erfinv, erfcinv}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).erf ());
}

/*
%!test
%! a = -1i* sqrt (-1/(6.4187*6.4187));
%! assert (erf (a), erf (real (a)));

%!test
%! x = [0,.5,1];
%! v = [0, .520499877813047, .842700792949715];
%! assert (erf (x), v, 1.e-10);
%! assert (erf (-x), -v, 1.e-10);
%! assert (erfc (x), 1-v, 1.e-10);
%! assert (erfinv (v), x, 1.e-10);

%!test
%! a = -1i* sqrt (single (-1/(6.4187*6.4187)));
%! assert (erf (a), erf (real (a)));

%!test
%! x = single ([0,.5,1]);
%! v = single ([0, .520499877813047, .842700792949715]);
%! assert (erf (x), v, 1.e-6);
%! assert (erf (-x), -v, 1.e-6);
%! assert (erfc (x), 1-v, 1.e-6);
%! assert (erfinv (v), x, 1.e-6);

%!test
%! x = [1+2i,-1+2i,1e-6+2e-6i,0+2i];
%! v = [-0.53664356577857-5.04914370344703i, 0.536643565778565-5.04914370344703i, 0.112837916709965e-5+0.225675833419178e-5i, 18.5648024145755526i];
%! assert (erf (x), v, -1.e-10);
%! assert (erf (-x), -v, -1.e-10);
%! assert (erfc (x), 1-v, -1.e-10);

%!error erf ()
%!error erf (1, 2)
*/

DEFUN (erfinv, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{y} =} erfinv (@var{x})
Compute the inverse error function.

The inverse error function is defined such that

@example
erf (@var{y}) == @var{x}
@end example
@seealso{erf, erfc, erfcx, erfi, dawson, erfcinv}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).erfinv ());
}

/*
## middle region
%!assert (erf (erfinv ([-0.9 -0.3 0 0.4 0.8])), [-0.9 -0.3 0 0.4 0.8], eps)
%!assert (erf (erfinv (single ([-0.9 -0.3 0 0.4 0.8]))),
%!        single ([-0.9 -0.3 0 0.4 0.8]), eps ("single"))
## tail region
%!assert (erf (erfinv ([-0.999 -0.99 0.9999 0.99999])),
%!        [-0.999 -0.99 0.9999 0.99999], eps)
%!assert (erf (erfinv (single ([-0.999 -0.99 0.9999 0.99999]))),
%!        single ([-0.999 -0.99 0.9999 0.99999]), eps ("single"))
## backward - loss of accuracy
%!assert (erfinv (erf ([-3 -1 -0.4 0.7 1.3 2.8])),
%!        [-3 -1 -0.4 0.7 1.3 2.8], -1e-12)
%!assert (erfinv (erf (single ([-3 -1 -0.4 0.7 1.3 2.8]))),
%!        single ([-3 -1 -0.4 0.7 1.3 2.8]), -1e-4)
## exceptional
%!assert (erfinv ([-1, 1, 1.1, -2.1]), [-Inf, Inf, NaN, NaN])
%!error erfinv (1+2i)

%!error erfinv ()
%!error erfinv (1, 2)
*/

DEFUN (erfcinv, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{y} =} erfcinv (@var{x})
Compute the inverse complementary error function.

The inverse complementary error function is defined such that

@example
erfc (@var{y}) == @var{x}
@end example
@seealso{erfc, erf, erfcx, erfi, dawson, erfinv}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).erfcinv ());
}

/*
## middle region
%!assert (erfc (erfcinv ([1.9 1.3 1 0.6 0.2])), [1.9 1.3 1 0.6 0.2], eps)
%!assert (erfc (erfcinv (single ([1.9 1.3 1 0.6 0.2]))),
%!        single ([1.9 1.3 1 0.6 0.2]), eps ("single"))
## tail region
%!assert (erfc (erfcinv ([0.001 0.01 1.9999 1.99999])),
%!        [0.001 0.01 1.9999 1.99999], eps)
%!assert (erfc (erfcinv (single ([0.001 0.01 1.9999 1.99999]))),
%!        single ([0.001 0.01 1.9999 1.99999]), eps ("single"))
## backward - loss of accuracy
%!assert (erfcinv (erfc ([-3 -1 -0.4 0.7 1.3 2.8])),
%!        [-3 -1 -0.4 0.7 1.3 2.8], -1e-12)
%!assert (erfcinv (erfc (single ([-3 -1 -0.4 0.7 1.3 2.8]))),
%!        single ([-3 -1 -0.4 0.7 1.3 2.8]), -1e-4)
## exceptional
%!assert (erfcinv ([2, 0, -0.1, 2.1]), [-Inf, Inf, NaN, NaN])
%!error erfcinv (1+2i)

%!error erfcinv ()
%!error erfcinv (1, 2)
*/

DEFUN (erfc, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{v} =} erfc (@var{z})
Compute the complementary error function.

The complementary error function is defined as
@tex
$1 - {\rm erf} (z)$.
@end tex
@ifnottex
@w{@code{1 - erf (@var{z})}}.
@end ifnottex
@seealso{erfcinv, erfcx, erfi, dawson, erf, erfinv}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).erfc ());
}

/*
%!test
%! a = -1i* sqrt (-1/(6.4187*6.4187));
%! assert (erfc (a), erfc (real (a)));

%!error erfc ()
%!error erfc (1, 2)
*/

DEFUN (erfcx, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{v} =} erfcx (@var{z})
Compute the scaled complementary error function.

The scaled complementary error function is defined as
@tex
$$
 e^{z^2} {\rm erfc} (z) \equiv e^{z^2} (1 - {\rm erf} (z))
$$
@end tex
@ifnottex

@example
exp (z^2) * erfc (z)
@end example

@end ifnottex
@seealso{erfc, erf, erfi, dawson, erfinv, erfcinv}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).erfcx ());
}

/*

%!test
%! x = [1+2i,-1+2i,1e-6+2e-6i,0+2i];
%! assert (erfcx (x), exp (x.^2) .* erfc (x), -1.e-10);

%!test
%! x = [100, 100+20i];
%! v = [0.0056416137829894329, 0.0054246791754558-0.00108483153786434i];
%! assert (erfcx (x), v, -1.e-10);

%!error erfcx ()
%!error erfcx (1, 2)
*/

DEFUN (erfi, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{v} =} erfi (@var{z})
Compute the imaginary error function.

The imaginary error function is defined as
@tex
$$
 -i {\rm erf} (iz)
$$
@end tex
@ifnottex

@example
-i * erf (i*z)
@end example

@end ifnottex
@seealso{erfc, erf, erfcx, dawson, erfinv, erfcinv}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).erfi ());
}

/*

%!test
%! x = [-0.1, 0.1, 1, 1+2i,-1+2i,1e-6+2e-6i,0+2i];
%! assert (erfi (x), -i * erf (i*x), -1.e-10);

%!error erfi ()
%!error erfi (1, 2)
*/

DEFUN (dawson, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{v} =} dawson (@var{z})
Compute the Dawson (scaled imaginary error) function.

The Dawson function is defined as
@tex
$$
 {\sqrt{\pi} \over 2} e^{-z^2} {\rm erfi} (z) \equiv -i {\sqrt{\pi} \over 2} e^{-z^2} {\rm erf} (iz)
$$
@end tex
@ifnottex

@example
(sqrt (pi) / 2) * exp (-z^2) * erfi (z)
@end example

@end ifnottex
@seealso{erfc, erf, erfcx, erfi, erfinv, erfcinv}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).dawson ());
}

/*

%!test
%! x = [0.1, 1, 1+2i,-1+2i,1e-4+2e-4i,0+2i];
%! v = [0.099335992397852861, 0.53807950691, -13.38892731648-11.828715104i, 13.38892731648-11.828715104i, 0.0001000000073333+0.000200000001333i, 48.160012114291i];
%! assert (dawson (x), v, -1.e-10);
%! assert (dawson (-x), -v, -1.e-10);

%!error dawson ()
%!error dawson (1, 2)
*/

DEFUN (exp, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{y} =} exp (@var{x})
Compute
@tex
$e^{x}$
@end tex
@ifnottex
@code{e^x}
@end ifnottex
for each element of @var{x}.

To compute the matrix exponential, @pxref{Linear Algebra}.
@seealso{log}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).exp ());
}

/*
%!assert (exp ([0, 1, -1, -1000]), [1, e, 1/e, 0], sqrt (eps))
%!assert (exp (1+i), e * (cos (1) + sin (1) * i), sqrt (eps))
%!assert (exp (single ([0, 1, -1, -1000])),
%!        single ([1, e, 1/e, 0]), sqrt (eps ("single")))
%!assert (exp (single (1+i)),
%!        single (e * (cos (1) + sin (1) * i)), sqrt (eps ("single")))

%!assert (exp ([Inf, -Inf, NaN]), [Inf 0 NaN])
%!assert (exp (single ([Inf, -Inf, NaN])), single ([Inf 0 NaN]))

%!error exp ()
%!error exp (1, 2)
*/

DEFUN (expm1, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{y} =} expm1 (@var{x})
Compute
@tex
$ e^{x} - 1 $
@end tex
@ifnottex
@code{exp (@var{x}) - 1}
@end ifnottex
accurately in the neighborhood of zero.
@seealso{exp}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).expm1 ());
}

/*
%!assert (expm1 (2*eps), 2*eps, 1e-29)

%!assert (expm1 ([Inf, -Inf, NaN]), [Inf -1 NaN])
%!assert (expm1 (single ([Inf, -Inf, NaN])), single ([Inf -1 NaN]))

%!error expm1 ()
%!error expm1 (1, 2)
*/

DEFUN (isfinite, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{tf} =} isfinite (@var{x})
Return a logical array which is true where the elements of @var{x} are
finite values and false where they are not.

For example:

@example
@group
isfinite ([13, Inf, NA, NaN])
     @result{} [ 1, 0, 0, 0 ]
@end group
@end example
@seealso{isinf, isnan, isna}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).isfinite ());
}

/*
%!assert (! isfinite (Inf))
%!assert (! isfinite (NaN))
%!assert (isfinite (rand (1,10)))

%!assert (! isfinite (single (Inf)))
%!assert (! isfinite (single (NaN)))
%!assert (isfinite (single (rand (1,10))))
%!assert (isfinite ('a'))

%!error isfinite ()
%!error isfinite (1, 2)
*/

DEFUN (fix, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{y} =} fix (@var{x})
Truncate fractional portion of @var{x} and return the integer portion.

This is equivalent to rounding towards zero.  If @var{x} is complex, return
@code{fix (real (@var{x})) + fix (imag (@var{x})) * I}.

@example
@group
fix ([-2.7, 2.7])
   @result{} -2    2
@end group
@end example
@seealso{ceil, floor, round}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).fix ());
}

/*
%!assert (fix ([1.1, 1, -1.1, -1]), [1, 1, -1, -1])
%!assert (fix ([1.1+1.1i, 1+i, -1.1-1.1i, -1-i]), [1+i, 1+i, -1-i, -1-i])
%!assert (fix (single ([1.1, 1, -1.1, -1])), single ([1, 1, -1, -1]))
%!assert (fix (single ([1.1+1.1i, 1+i, -1.1-1.1i, -1-i])),
%!        single ([1+i, 1+i, -1-i, -1-i]))

%!error fix ()
%!error fix (1, 2)
*/

DEFUN (floor, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{y} =} floor (@var{x})
Return the largest integer not greater than @var{x}.

This is equivalent to rounding towards negative infinity.  If @var{x} is
complex, return @code{floor (real (@var{x})) + floor (imag (@var{x})) * I}.

@example
@group
floor ([-2.7, 2.7])
     @result{} -3    2
@end group
@end example
@seealso{ceil, round, fix}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).floor ());
}

/*
%!assert (floor ([2, 1.1, -1.1, -1]), [2, 1, -2, -1])
%!assert (floor ([2+2i, 1.1+1.1i, -1.1-1.1i, -1-i]), [2+2i, 1+i, -2-2i, -1-i])
%!assert (floor (single ([2, 1.1, -1.1, -1])), single ([2, 1, -2, -1]))
%!assert (floor (single ([2+2i, 1.1+1.1i, -1.1-1.1i, -1-i])),
%!        single ([2+2i, 1+i, -2-2i, -1-i]))

%!error floor ()
%!error floor (1, 2)
*/

DEFUN (gamma, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{v} =} gamma (@var{z})
Compute the Gamma function.

The Gamma function is defined as
@tex
$$
 \Gamma (z) = \int_0^\infty t^{z-1} e^{-t} dt.
$$
@end tex
@ifnottex

@example
@group
             infinity
            /
gamma (z) = | t^(z-1) exp (-t) dt.
            /
         t=0
@end group
@end example

@end ifnottex

Programming Note: The gamma function can grow quite large even for small
input values.  In many cases it may be preferable to use the natural
logarithm of the gamma function (@code{gammaln}) in calculations to minimize
loss of precision.  The final result is then
@code{exp (@var{result_using_gammaln}).}
@seealso{gammainc, gammaln, factorial}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).gamma ());
}

/*
%!test
%! a = -1i* sqrt (-1/(6.4187*6.4187));
%! assert (gamma (a), gamma (real (a)));

%!test
%! x = [.5, 1, 1.5, 2, 3, 4, 5];
%! v = [sqrt(pi), 1, .5*sqrt(pi), 1, 2, 6, 24];
%! assert (gamma (x), v, sqrt (eps));

%!test
%! a = single (-1i* sqrt (-1/(6.4187*6.4187)));
%! assert (gamma (a), gamma (real (a)));

%!test
%! x = single ([.5, 1, 1.5, 2, 3, 4, 5]);
%! v = single ([sqrt(pi), 1, .5*sqrt(pi), 1, 2, 6, 24]);
%! assert (gamma (x), v, sqrt (eps ("single")));

%!test
%! ## Test exceptional values
%! x = [-Inf, -1, -0, 0, 1, Inf, NaN];
%! v = [Inf, Inf, -Inf, Inf, 1, Inf, NaN];
%! assert (gamma (x), v);
%! assert (gamma (single (x)), single (v));

%!error gamma ()
%!error gamma (1, 2)
*/

DEFUN (imag, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{y} =} imag (@var{z})
Return the imaginary part of @var{z} as a real number.
@seealso{real, conj}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).imag ());
}

/*
%!assert (imag (1), 0)
%!assert (imag (i), 1)
%!assert (imag (1+i), 1)
%!assert (imag ([i, 1; 1, i]), full (eye (2)))

%!assert (imag (single (1)), single (0))
%!assert (imag (single (i)), single (1))
%!assert (imag (single (1+i)), single (1))
%!assert (imag (single ([i, 1; 1, i])), full (eye (2,"single")))

%!error imag ()
%!error imag (1, 2)
*/

DEFUNX ("isalnum", Fisalnum, args, ,
        doc: /* -*- texinfo -*-
@deftypefn {} {@var{tf} =} isalnum (@var{s})
Return a logical array which is true where the elements of @var{s} are
letters or digits and false where they are not.

This is equivalent to (@code{isalpha (@var{s}) | isdigit (@var{s})}).
@seealso{isalpha, isdigit, ispunct, isspace, iscntrl}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).xisalnum ());
}

/*
%!test
%! charset = char (0:127);
%! result = false (1, 128);
%! result(double ("A":"Z") + 1) = true;
%! result(double ("0":"9") + 1) = true;
%! result(double ("a":"z") + 1) = true;
%! assert (isalnum (charset), result);
%!assert (isalnum(["Ä8Aa?"; "(Uß ;"]), logical ([1 1 1 1 1 0; 0 1 1 1 0 0]))

%!error isalnum ()
%!error isalnum (1, 2)
*/

DEFUNX ("isalpha", Fisalpha, args, ,
        doc: /* -*- texinfo -*-
@deftypefn {} {@var{tf} =} isalpha (@var{s})
Return a logical array which is true where the elements of @var{s} are
letters and false where they are not.

This is equivalent to (@code{islower (@var{s}) | isupper (@var{s})}).
@seealso{isdigit, ispunct, isspace, iscntrl, isalnum, islower, isupper}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).xisalpha ());
}

/*
%!test
%! charset = char (0:127);
%! result = false (1, 128);
%! result(double ("A":"Z") + 1) = true;
%! result(double ("a":"z") + 1) = true;
%! assert (isalpha (charset), result);
%!assert (isalpha("Ä8Aa(Uß ;"), logical ([1 1 0 1 1 0 1 1 1 0 0]))

%!error isalpha ()
%!error isalpha (1, 2)
*/

DEFUNX ("isascii", Fisascii, args, ,
        doc: /* -*- texinfo -*-
@deftypefn {} {@var{tf} =} isascii (@var{s})
Return a logical array which is true where the elements of @var{s} are
ASCII characters (in the range 0 to 127 decimal) and false where they are
not.
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).xisascii ());
}

/*
%!test
%! charset = char (0:127);
%! result = true (1, 128);
%! assert (isascii (charset), result);

%!error isascii ()
%!error isascii (1, 2)
*/

DEFUNX ("iscntrl", Fiscntrl, args, ,
        doc: /* -*- texinfo -*-
@deftypefn {} {@var{tf} =} iscntrl (@var{s})
Return a logical array which is true where the elements of @var{s} are
control characters and false where they are not.
@seealso{ispunct, isspace, isalpha, isdigit}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).xiscntrl ());
}

/*
%!test
%! charset = char (0:127);
%! result = false (1, 128);
%! result(1:32) = true;
%! result(128) = true;
%! assert (iscntrl (charset), result);

%!error iscntrl ()
%!error iscntrl (1, 2)
*/

DEFUNX ("isdigit", Fisdigit, args, ,
        doc: /* -*- texinfo -*-
@deftypefn {} {@var{tf} =} isdigit (@var{s})
Return a logical array which is true where the elements of @var{s} are
decimal digits (0-9) and false where they are not.
@seealso{isxdigit, isalpha, isletter, ispunct, isspace, iscntrl}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).xisdigit ());
}

/*
%!test
%! charset = char (0:127);
%! result = false (1, 128);
%! result(double ("0":"9") + 1) = true;
%! assert (isdigit (charset), result);
%!assert (isdigit("Ä8Aa(Uß ;"), logical ([0 0 1 0 0 0 0 0 0 0 0]))

%!error isdigit ()
%!error isdigit (1, 2)
*/

DEFUN (isinf, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{tf} =} isinf (@var{x})
Return a logical array which is true where the elements of @var{x} are
infinite and false where they are not.

For example:

@example
@group
isinf ([13, Inf, NA, NaN])
      @result{} [ 0, 1, 0, 0 ]
@end group
@end example
@seealso{isfinite, isnan, isna}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).isinf ());
}

/*
%!assert (isinf (Inf))
%!assert (! isinf (NaN))
%!assert (! isinf (NA))
%!assert (isinf (rand (1,10)), false (1,10))
%!assert (isinf ([NaN -Inf -1 0 1 Inf NA]),
%!        [false, true, false, false, false, true, false])

%!assert (isinf (single (Inf)))
%!assert (! isinf (single (NaN)))
%!assert (! isinf (single (NA)))
%!assert (isinf (single (rand (1,10))), false (1,10))
%!assert (isinf (single ([NaN -Inf -1 0 1 Inf NA])),
%!        [false, true, false, false, false, true, false])
%!assert (! isinf ('a'))

%!error isinf ()
%!error isinf (1, 2)
*/

DEFUNX ("isgraph", Fisgraph, args, ,
        doc: /* -*- texinfo -*-
@deftypefn {} {@var{tf} =} isgraph (@var{s})
Return a logical array which is true where the elements of @var{s} are
printable characters (but not the space character) and false where they are
not.
@seealso{isprint}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).xisgraph ());
}

/*
%!test
%! charset = char (0:127);
%! result = false (1, 128);
%! result(34:127) = true;
%! assert (isgraph (charset), result);
%!assert (isgraph("Ä8Aa(Uß ;"), logical ([1 1 1 1 1 1 1 1 1 0 1]))

%!error isgraph ()
%!error isgraph (1, 2)
*/

DEFUNX ("islower", Fislower, args, ,
        doc: /* -*- texinfo -*-
@deftypefn {} {@var{tf} =} islower (@var{s})
Return a logical array which is true where the elements of @var{s} are
lowercase letters and false where they are not.
@seealso{isupper, isalpha, isletter, isalnum}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).xislower ());
}

/*
%!test
%! charset = char (0:127);
%! result = false (1, 128);
%! result(double ("a":"z") + 1) = true;
%! assert (islower (charset), result);
%!assert (islower("Ä8Aa(Uß ;"), logical ([0 0 0 0 1 0 0 1 1 0 0]))

%!error islower ()
%!error islower (1, 2)
*/

DEFUN (isna, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{tf} =} isna (@var{x})
Return a logical array which is true where the elements of @var{x} are
NA (missing) values and false where they are not.

For example:

@example
@group
isna ([13, Inf, NA, NaN])
     @result{} [ 0, 0, 1, 0 ]
@end group
@end example
@seealso{isnan, isinf, isfinite}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).isna ());
}

/*
%!assert (! isna (Inf))
%!assert (! isna (NaN))
%!assert (isna (NA))
%!assert (isna (rand (1,10)), false (1,10))
%!assert (isna ([NaN -Inf -1 0 1 Inf NA]),
%!        [false, false, false, false, false, false, true])

%!assert (! isna (single (Inf)))
%!assert (! isna (single (NaN)))
%!assert (isna (single (NA)))
%!assert (isna (single (rand (1,10))), false (1,10))
%!assert (isna (single ([NaN -Inf -1 0 1 Inf NA])),
%!        [false, false, false, false, false, false, true])

%!error isna ()
%!error isna (1, 2)
*/

DEFUN (isnan, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{tf} =} isnan (@var{x})
Return a logical array which is true where the elements of @var{x} are
NaN values and false where they are not.

NA values are also considered NaN values.  For example:

@example
@group
isnan ([13, Inf, NA, NaN])
      @result{} [ 0, 0, 1, 1 ]
@end group
@end example
@seealso{isna, isinf, isfinite}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).isnan ());
}

/*
%!assert (! isnan (Inf))
%!assert (isnan (NaN))
%!assert (isnan (NA))
%!assert (isnan (rand (1,10)), false (1,10))
%!assert (isnan ([NaN -Inf -1 0 1 Inf NA]),
%!        [true, false, false, false, false, false, true])

%!assert (! isnan (single (Inf)))
%!assert (isnan (single (NaN)))
%!assert (isnan (single (NA)))
%!assert (isnan (single (rand (1,10))), false (1,10))
%!assert (isnan (single ([NaN -Inf -1 0 1 Inf NA])),
%!        [true, false, false, false, false, false, true])
%!assert (! isnan ('a'))

%!error isnan ()
%!error isnan (1, 2)
*/

DEFUNX ("isprint", Fisprint, args, ,
        doc: /* -*- texinfo -*-
@deftypefn {} {@var{tf} =} isprint (@var{s})
Return a logical array which is true where the elements of @var{s} are
printable characters (including the space character) and false where they
are not.
@seealso{isgraph}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).xisprint ());
}

/*
%!test
%! charset = char (0:127);
%! result = false (1, 128);
%! result(33:127) = true;
%! assert (isprint (charset), result);
%!assert (isprint("Ä8Aa(Uß ;"), logical ([1 1 1 1 1 1 1 1 1 1 1]))

%!error isprint ()
%!error isprint (1, 2)
*/

DEFUNX ("ispunct", Fispunct, args, ,
        doc: /* -*- texinfo -*-
@deftypefn {} {@var{tf} =} ispunct (@var{s})
Return a logical array which is true where the elements of @var{s} are
punctuation characters and false where they are not.
@seealso{isalpha, isdigit, isspace, iscntrl}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).xispunct ());
}

/*
%!test
%! charset = char (0:127);
%! result = false (1, 128);
%! result(34:48) = true;
%! result(59:65) = true;
%! result(92:97) = true;
%! result(124:127) = true;
%! assert (ispunct (charset), result);
%!assert (ispunct("Ä8Aa(Uß ;"), logical ([0 0 0 0 0 1 0 0 0 0 1]))

%!error ispunct ()
%!error ispunct (1, 2)
*/

DEFUNX ("isspace", Fisspace, args, ,
        doc: /* -*- texinfo -*-
@deftypefn {} {@var{tf} =} isspace (@var{s})
Return a logical array which is true where the elements of @var{s} are
whitespace characters (space, formfeed, newline, carriage return, tab, and
vertical tab) and false where they are not.
@seealso{iscntrl, ispunct, isalpha, isdigit}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).xisspace ());
}

/*
%!test
%! charset = char (0:127);
%! result = false (1, 128);
%! result(double (" \f\n\r\t\v") + 1) = true;
%! assert (isspace (charset), result);
%!assert (isspace("Ä8Aa(Uß ;"), logical ([0 0 0 0 0 0 0 0 0 1 0]))

%!error isspace ()
%!error isspace (1, 2)
*/

DEFUNX ("isupper", Fisupper, args, ,
        doc: /* -*- texinfo -*-
@deftypefn {} {@var{tf} =} isupper (@var{s})
Return a logical array which is true where the elements of @var{s} are
uppercase letters and false where they are not.
@seealso{islower, isalpha, isletter, isalnum}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).xisupper ());
}

/*
%!test
%! charset = char (0:127);
%! result = false (1, 128);
%! result(double ("A":"Z") + 1) = true;
%! assert (isupper (charset), result);
%!assert (isupper("Ä8Aa(Uß ;"), logical ([1 1 0 1 0 0 1 0 0 0 0]))

%!error isupper ()
%!error isupper (1, 2)
*/

DEFUNX ("isxdigit", Fisxdigit, args, ,
        doc: /* -*- texinfo -*-
@deftypefn {} {@var{tf} =} isxdigit (@var{s})
Return a logical array which is true where the elements of @var{s} are
hexadecimal digits (0-9 and @nospell{a-fA-F}).
@seealso{isdigit}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).xisxdigit ());
}

/*
%!test
%! charset = char (0:127);
%! result = false (1, 128);
%! result(double ("A":"F") + 1) = true;
%! result(double ("0":"9") + 1) = true;
%! result(double ("a":"f") + 1) = true;
%! assert (isxdigit (charset), result);
%!assert (isxdigit("Ä8Aa(Uß ;"), logical ([0 0 1 1 1 0 0 0 0 0 0]))

%!error isxdigit ()
%!error isxdigit (1, 2)
*/

DEFUN (lgamma, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{y} =} gammaln (@var{x})
@deftypefnx {} {@var{y} =} lgamma (@var{x})
Return the natural logarithm of the gamma function of @var{x}.

Programming Note: @code{lgamma} is an alias for @code{gammaln} and either name
can be used in Octave.
@seealso{gamma, gammainc}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).lgamma ());
}

/*
%!test
%! a = -1i* sqrt (-1/(6.4187*6.4187));
%! assert (gammaln (a), gammaln (real (a)));

%!test
%! x = [.5, 1, 1.5, 2, 3, 4, 5];
%! v = [sqrt(pi), 1, .5*sqrt(pi), 1, 2, 6, 24];
%! assert (gammaln (x), log (v), sqrt (eps));

%!test
%! a = single (-1i* sqrt (-1/(6.4187*6.4187)));
%! assert (gammaln (a), gammaln (real (a)));

%!test
%! x = single ([.5, 1, 1.5, 2, 3, 4, 5]);
%! v = single ([sqrt(pi), 1, .5*sqrt(pi), 1, 2, 6, 24]);
%! assert (gammaln (x), log (v), sqrt (eps ("single")));

%!test
%! x = [-1, 0, 1, Inf];
%! v = [Inf, Inf, 0, Inf];
%! assert (gammaln (x), v);
%! assert (gammaln (single (x)), single (v));

%!error gammaln ()
%!error gammaln (1,2)
*/

DEFUN (log, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{y} =} log (@var{x})
Compute the natural logarithm,
@tex
$\ln{(x)},$
@end tex
@ifnottex
@code{ln (@var{x})},
@end ifnottex
for each element of @var{x}.

To compute the matrix logarithm, @pxref{Linear Algebra}.
@seealso{exp, log1p, log2, log10, logspace}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).log ());
}

/*
%!assert (log ([1, e, e^2]), [0, 1, 2], sqrt (eps))
%!assert (log ([-0.5, -1.5, -2.5]), log ([0.5, 1.5, 2.5]) + pi*1i, sqrt (eps))

%!assert (log (single ([1, e, e^2])), single ([0, 1, 2]), sqrt (eps ("single")))
%!assert (log (single ([-0.5, -1.5, -2.5])),
%!        single (log ([0.5, 1.5, 2.5]) + pi*1i), 4* eps ("single"))

%!error log ()
%!error log (1, 2)
*/

DEFUN (log10, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{y} =} log10 (@var{x})
Compute the base-10 logarithm of each element of @var{x}.
@seealso{log, log2, logspace, exp}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).log10 ());
}

/*
%!assert (log10 ([0.01, 0.1, 1, 10, 100]), [-2, -1, 0, 1, 2], sqrt (eps))
%!assert (log10 (single ([0.01, 0.1, 1, 10, 100])),
%!        single ([-2, -1, 0, 1, 2]), sqrt (eps ("single")))

%!error log10 ()
%!error log10 (1, 2)
*/

DEFUN (log1p, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{y} =} log1p (@var{x})
Compute
@tex
$\ln{(1 + x)}$
@end tex
@ifnottex
@code{log (1 + @var{x})}
@end ifnottex
accurately in the neighborhood of zero.
@seealso{log, exp, expm1}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).log1p ());
}

/*
%!assert (log1p ([0, 2*eps, -2*eps]), [0, 2*eps, -2*eps], 1e-29)
%!assert (log1p (single ([0, 2*eps, -2*eps])), ...
%!        single ([0, 2*eps, -2*eps]), 1e-29)
## Compare to result from Wolfram Alpha rounded to 16 significant digits
%!assert <*62094> (log1p (0.1i), ...
%!                 0.004975165426584041 + 0.09966865249116203i, eps (0.2))
%!assert <*62094> (log1p (single (0.1i)), ...
%!                 single (0.004975165426584041 + 0.09966865249116203i), ...
%!                 eps (single (0.2)))

%!error log1p ()
%!error log1p (1, 2)
*/

DEFUN (real, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{x} =} real (@var{z})
Return the real part of @var{z}.
@seealso{imag, conj}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).real ());
}

/*
%!assert (real (1), 1)
%!assert (real (i), 0)
%!assert (real (1+i), 1)
%!assert (real ([1, i; i, 1]), full (eye (2)))

%!assert (real (single (1)), single (1))
%!assert (real (single (i)), single (0))
%!assert (real (single (1+i)), single (1))
%!assert (real (single ([1, i; i, 1])), full (eye (2, "single")))

%!error real ()
%!error real (1, 2)
*/

DEFUN (round, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{y} =} round (@var{x})
Return the integer nearest to @var{x}.

If @var{x} is complex, return
@code{round (real (@var{x})) + round (imag (@var{x})) * I}.  If there
are two nearest integers, return the one further away from zero.

@example
@group
round ([-2.7, 2.7])
     @result{} -3    3
@end group
@end example
@seealso{ceil, floor, fix, roundb}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).round ());
}

/*
%!assert (round (1), 1)
%!assert (round (1.1), 1)
%!assert (round (5.5), 6)
%!assert (round (i), i)
%!assert (round (2.5+3.5i), 3+4i)
%!assert (round (-2.6), -3)
%!assert (round ([1.1, -2.4; -3.7, 7.1]), [1, -2; -4, 7])

%!assert (round (single (1)), single (1))
%!assert (round (single (1.1)), single (1))
%!assert (round (single (5.5)), single (6))
%!assert (round (single (i)), single (i))
%!assert (round (single (2.5+3.5i)), single (3+4i))
%!assert (round (single (-2.6)), single (-3))
%!assert (round (single ([1.1, -2.4; -3.7, 7.1])), single ([1, -2; -4, 7]))

%!error round ()
%!error round (1, 2)
*/

DEFUN (roundb, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{y} =} roundb (@var{x})
Return the integer nearest to @var{x}.  If there are two nearest
integers, return the even one (banker's rounding).

If @var{x} is complex,
return @code{roundb (real (@var{x})) + roundb (imag (@var{x})) * I}.
@seealso{round}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).roundb ());
}

/*
%!assert (roundb (1), 1)
%!assert (roundb (1.1), 1)
%!assert (roundb (1.5), 2)
%!assert (roundb (4.5), 4)
%!assert (roundb (i), i)
%!assert (roundb (2.5+3.5i), 2+4i)
%!assert (roundb (-2.6), -3)
%!assert (roundb ([1.1, -2.4; -3.7, 7.1]), [1, -2; -4, 7])

%!assert (roundb (single (1)), single (1))
%!assert (roundb (single (1.1)), single (1))
%!assert (roundb (single (1.5)), single (2))
%!assert (roundb (single (4.5)), single (4))
%!assert (roundb (single (i)), single (i))
%!assert (roundb (single (2.5+3.5i)), single (2+4i))
%!assert (roundb (single (-2.6)), single (-3))
%!assert (roundb (single ([1.1, -2.4; -3.7, 7.1])), single ([1, -2; -4, 7]))

%!error roundb ()
%!error roundb (1, 2)
*/

DEFUN (sign, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{y} =} sign (@var{x})
Compute the @dfn{signum} function.

This is defined as
@tex
$$
{\rm sign} (@var{x}) = \cases{1,&$x>0$;\cr 0,&$x=0$;\cr -1,&$x<0$.\cr}
$$
@end tex
@ifnottex

@example
@group
           -1, x < 0;
sign (x) =  0, x = 0;
            1, x > 0.
@end group
@end example

@end ifnottex

For complex arguments, @code{sign} returns @code{x ./ abs (@var{x})}.

Note that @code{sign (-0.0)} is 0.  Although IEEE 754 floating point
allows zero to be signed, 0.0 and -0.0 compare equal.  If you must test
whether zero is signed, use the @code{signbit} function.
@seealso{signbit}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).signum ());
}

/*
%!assert (sign (-2) , -1)
%!assert (sign (0), 0)
%!assert (sign (3), 1)
%!assert (sign ([1, -pi; e, 0]), [1, -1; 1, 0])

%!assert (sign (single (-2)) , single (-1))
%!assert (sign (single (0)), single (0))
%!assert (sign (single (3)), single (1))
%!assert (sign (single ([1, -pi; e, 0])), single ([1, -1; 1, 0]))

%!error sign ()
%!error sign (1, 2)
*/

DEFUNX ("signbit", Fsignbit, args, ,
        doc: /* -*- texinfo -*-
@deftypefn {} {@var{y} =} signbit (@var{x})
Return logical true if the value of @var{x} has its sign bit set and false
otherwise.

This behavior is consistent with the other logical functions.
@xref{Logical Values}.  The behavior differs from the C language function
which returns nonzero if the sign bit is set.

This is not the same as @code{x < 0.0}, because IEEE 754 floating point
allows zero to be signed.  The comparison @code{-0.0 < 0.0} is false,
but @code{signbit (-0.0)} will return a nonzero value.
@seealso{sign}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  octave_value tmp = args(0).xsignbit ();

  return ovl (tmp != 0);
}

/*
%!assert (signbit (1) == 0)
%!assert (signbit (-2) != 0)
%!assert (signbit (0) == 0)
%!assert (signbit (-0) != 0)

%!assert (signbit (single (1)) == 0)
%!assert (signbit (single (-2)) != 0)
%!assert (signbit (single (0)) == 0)
%!assert (signbit (single (-0)) != 0)

%!error sign ()
%!error sign (1, 2)
*/

DEFUN (sin, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{y} =} sin (@var{x})
Compute the sine for each element of @var{x} in radians.
@seealso{asin, sind, sinh}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).sin ());
}

/*
%!shared rt2, rt3
%! rt2 = sqrt (2);
%! rt3 = sqrt (3);

%!test
%! x = [0, pi/6, pi/4, pi/3, pi/2, 2*pi/3, 3*pi/4, 5*pi/6, pi];
%! v = [0, 1/2, rt2/2, rt3/2, 1, rt3/2, rt2/2, 1/2, 0];
%! assert (sin (x), v, sqrt (eps));

%!test
%! x = single ([0, pi/6, pi/4, pi/3, pi/2, 2*pi/3, 3*pi/4, 5*pi/6, pi]);
%! v = single ([0, 1/2, rt2/2, rt3/2, 1, rt3/2, rt2/2, 1/2, 0]);
%! assert (sin (x), v, sqrt (eps ("single")));

%!error sin ()
%!error sin (1, 2)
*/

DEFUN (sinh, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{y} =} sinh (@var{x})
Compute the hyperbolic sine for each element of @var{x}.
@seealso{asinh, cosh, tanh}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).sinh ());
}

/*
%!test
%! x = [0, pi/2*i, pi*i, 3*pi/2*i];
%! v = [0, i, 0, -i];
%! assert (sinh (x), v, sqrt (eps));

%!test
%! x = single ([0, pi/2*i, pi*i, 3*pi/2*i]);
%! v = single ([0, i, 0, -i]);
%! assert (sinh (x), v, sqrt (eps ("single")));

%!error sinh ()
%!error sinh (1, 2)
*/

DEFUN (sqrt, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{y} =} sqrt (@var{x})
Compute the square root of each element of @var{x}.

If @var{x} is negative, a complex result is returned.

To compute the matrix square root, @pxref{Linear Algebra}.
@seealso{realsqrt, nthroot}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).sqrt ());
}

/*
%!assert (sqrt (4), 2)
%!assert (sqrt (-1), i)
%!assert (sqrt (1+i), exp (0.5 * log (1+i)), sqrt (eps))
%!assert (sqrt ([4, -4; i, 1-i]),
%!        [2, 2i; exp(0.5 * log (i)), exp(0.5 * log (1-i))], sqrt (eps))

%!assert (sqrt (single (4)), single (2))
%!assert (sqrt (single (-1)), single (i))
%!assert (sqrt (single (1+i)),
%!        single (exp (0.5 * log (1+i))), sqrt (eps ("single")))
%!assert (sqrt (single ([4, -4; i, 1-i])),
%!        single ([2, 2i; exp(0.5 * log (i)), exp(0.5 * log (1-i))]),
%!        sqrt (eps ("single")))

%!error sqrt ()
%!error sqrt (1, 2)
*/

DEFUN (tan, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{y} =} tan (@var{z})
Compute the tangent for each element of @var{x} in radians.
@seealso{atan, tand, tanh}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).tan ());
}

/*
%!shared rt2, rt3
%! rt2 = sqrt (2);
%! rt3 = sqrt (3);

%!test
%! x = [0, pi/6, pi/4, pi/3, 2*pi/3, 3*pi/4, 5*pi/6, pi];
%! v = [0, rt3/3, 1, rt3, -rt3, -1, -rt3/3, 0];
%! assert (tan (x), v,  sqrt (eps));

%!test
%! x = single ([0, pi/6, pi/4, pi/3, 2*pi/3, 3*pi/4, 5*pi/6, pi]);
%! v = single ([0, rt3/3, 1, rt3, -rt3, -1, -rt3/3, 0]);
%! assert (tan (x), v,  sqrt (eps ("single")));

%!error tan ()
%!error tan (1, 2)
*/

DEFUN (tanh, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{y} =} tanh (@var{x})
Compute hyperbolic tangent for each element of @var{x}.
@seealso{atanh, sinh, cosh}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).tanh ());
}

/*
%!test
%! x = [0, pi*i];
%! v = [0, 0];
%! assert (tanh (x), v, sqrt (eps));

%!test
%! x = single ([0, pi*i]);
%! v = single ([0, 0]);
%! assert (tanh (x), v, sqrt (eps ("single")));

%!error tanh ()
%!error tanh (1, 2)
*/

DEFUNX ("tolower", Ftolower, args, ,
        doc: /* -*- texinfo -*-
@deftypefn  {} {@var{y} =} tolower (@var{s})
@deftypefnx {} {@var{y} =} lower (@var{s})
Return a copy of the string or cell string @var{s}, with each uppercase
character replaced by the corresponding lowercase one; non-alphabetic
characters are left unchanged.

For example:

@example
@group
tolower ("MiXeD cAsE 123")
      @result{} "mixed case 123"
@end group
@end example

Programming Note: @code{lower} is an alias for @code{tolower} and either name
can be used in Octave.

@seealso{toupper}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).xtolower ());
}

DEFALIAS (lower, tolower);

/*
%!assert (tolower ("OCTAVE"), "octave")
%!assert (tolower ("123OCTave! _&"), "123octave! _&")
%!assert (tolower ({"ABC", "DEF", {"GHI", {"JKL"}}}),
%!        {"abc", "def", {"ghi", {"jkl"}}})
%!assert (tolower (["ABC"; "DEF"]), ["abc"; "def"])
%!assert (tolower ({["ABC"; "DEF"]}), {["abc";"def"]})
%!assert (tolower (["ABCÄÖÜSS"; "abcäöüß"]),
%!        ["abcäöüss"; "abcäöüß"])
%!assert (tolower (repmat ("ÄÖÜ", 2, 1, 3)), repmat ("äöü", 2, 1, 3))
%!assert (tolower (68), 68)
%!assert (tolower ({[68, 68; 68, 68]}), {[68, 68; 68, 68]})
%!assert (tolower (68i), 68i)
%!assert (tolower ({[68i, 68; 68, 68i]}), {[68i, 68; 68, 68i]})
%!assert (tolower (single (68i)), single (68i))
%!assert (tolower ({single([68i, 68; 68, 68i])}), {single([68i, 68; 68, 68i])})

%!test
%! classes = {@char, @double, @single, ...
%!            @int8, @int16, @int32, @int64, ...
%!            @uint8, @uint16, @uint32, @uint64};
%! for i = 1:numel (classes)
%!   cls = classes{i};
%!   assert (class (tolower (cls (97))), class (cls (97)));
%!   assert (class (tolower (cls ([98, 99]))), class (cls ([98, 99])));
%! endfor
%!test
%! a(3,3,3,3) = "D";
%! assert (tolower (a)(3,3,3,3), "d");

%!test
%! charset = char (0:127);
%! result = charset;
%! result (double ("A":"Z") + 1) = result (double ("a":"z") + 1);
%! assert (tolower (charset), result);

%!error <Invalid call to tolower> lower ()
%!error <Invalid call to tolower> tolower ()
%!error tolower (1, 2)
*/

DEFUNX ("toupper", Ftoupper, args, ,
        doc: /* -*- texinfo -*-
@deftypefn  {} {@var{y} =} toupper (@var{s})
@deftypefnx {} {@var{y} =} upper (@var{s})
Return a copy of the string or cell string @var{s}, with each lowercase
character replaced by the corresponding uppercase one; non-alphabetic
characters are left unchanged.

For example:

@example
@group
toupper ("MiXeD cAsE 123")
      @result{} "MIXED CASE 123"
@end group
@end example

Programming Note: @code{upper} is an alias for @code{toupper} and either name
can be used in Octave.

@seealso{tolower}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).xtoupper ());
}

DEFALIAS (upper, toupper);

/*
%!assert (toupper ("octave"), "OCTAVE")
%!assert (toupper ("123OCTave! _&"), "123OCTAVE! _&")
%!assert (toupper ({"abc", "def", {"ghi", {"jkl"}}}),
%!        {"ABC", "DEF", {"GHI", {"JKL"}}})
%!assert (toupper (["abc"; "def"]), ["ABC"; "DEF"])
%!assert (toupper ({["abc"; "def"]}), {["ABC";"DEF"]})
%!assert (toupper (["ABCÄÖÜSS"; "abcäöüß"]),
%!        ["ABCÄÖÜSS"; "ABCÄÖÜSS"])
%!assert (toupper (repmat ("äöü", 2, 1, 3)), repmat ("ÄÖÜ", 2, 1, 3))
%!assert (toupper (100), 100)
%!assert (toupper ({[100, 100; 100, 100]}), {[100, 100; 100, 100]})
%!assert (toupper (100i), 100i)
%!assert (toupper ({[100i, 100; 100, 100i]}), {[100i, 100; 100, 100i]})
%!assert (toupper (single (100i)), single (100i))
%!assert (toupper ({single([100i, 100; 100, 100i])}),
%!                 {single([100i, 100; 100, 100i])})

%!test
%! classes = {@char, @double, @single, ...
%!            @int8, @int16, @int32, @int64, ...
%!            @uint8, @uint16, @uint32, @uint64};
%! for i = 1:numel (classes)
%!   cls = classes{i};
%!   assert (class (toupper (cls (97))), class (cls (97)));
%!   assert (class (toupper (cls ([98, 99]))), class (cls ([98, 99])));
%! endfor
%!test
%! a(3,3,3,3) = "d";
%! assert (toupper (a)(3,3,3,3), "D");
%!test
%! charset = char (0:127);
%! result = charset;
%! result (double  ("a":"z") + 1) = result (double  ("A":"Z") + 1);
%! assert (toupper (charset), result);

%!error <Invalid call to toupper> toupper ()
%!error <Invalid call to toupper> upper ()
%!error toupper (1, 2)
*/

DEFALIAS (gammaln, lgamma);

OCTAVE_END_NAMESPACE(octave)
