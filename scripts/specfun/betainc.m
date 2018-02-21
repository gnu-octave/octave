## Copyright (C) 2018 Stefan Schlögl
## Copyright (C) 2018 Michele Ginesi
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## Authors: Michele Ginesi <michele.ginesi@gmail.com>

## -*- texinfo -*-
## @deftypefn {} {} betainc (@var{x}, @var{a}, @var{b})
## @deftypefnx {} {} betainc (@var{x}, @var{a}, @var{b}, @var{tail})
## Compute the incomplete beta function ratio.
##
## This is defined as
## @tex
## $$
## I_x (a, b) = {1 \over {B(a,b)}} \displaystyle{\int_0^x t^{a-1} (1-t)^{b-1} dt}
## $$
## @end tex
## @ifnottex
##
## @example
## @group
##                x
##               /
##              |
## I_x (a, b) = | t^(a-1) (1-t)^(b-1) dt
##              |
##              /
##             0
## @end group
## @end example
##
## @end ifnottex
##
## with @var{x} real in [0,1], @var{a} and @var{b} real and strictly positive.
## If one of the input has more than one components, then the others must be
## scalar or of compatible dimensions.
##
## By default or if @var{tail} is @qcode{"lower"} the incomplete beta function
## ratio integrated from 0 to @var{x} is computed. If @var{tail} is
## @qcode{"upper"} then the complementary function integrated from @var{x} to
## 1 is calculated. The two choices are related as
##
## betainc (@var{x}, @var{a}, @var{b}, @qcode{"lower"}) = 
## 1 - betainc (@var{x}, @var{a}, @var{b}, @qcode{"upper"}).
##
## Reference
##
## @nospell{A. Cuyt, V. Brevik Petersen, B. Verdonk, H. Waadeland, W.B. Jones}
## @cite{Handbook of Continued Fractions for Special Functions},
## ch. 18.
##
## @seealso{beta, betaincinv, betaln}
##
## @end deftypefn

function [y] = betainc (x, a, b, tail = "lower")

  if (nargin > 4 || nargin < 3)
    print_usage ();
  endif

  if (! isscalar (x) || ! isscalar (a) || ! isscalar (b))
    [err, x, a, b] = common_size (x, a, b);
    if (err > 0)
      error ("betainc: x, a and b must be of common size or scalars");
    endif
  endif

  if (iscomplex (x) || iscomplex (a) || iscomplex (b))
    error ("betainc: inputs must be real or integer");
  endif

  if (any (a <= 0))
    error ("betainc: a must be strictly positive");
  endif

    if (any (b <= 0))
    error ("betainc: b must be strictly positive");
  endif

  if (any (x > 1 | x < 0))
    error ("betainc: x must be between 0 and 1");
  endif

  if (isinteger (x))
    y = double (x);
  endif

  if (isinteger (a))
    a = double (a);
  endif

  if (isinteger (b))
    b = double (b);
  endif

  sz = size (x);
  x = x(:);
  a = a(:);
  b = b(:);
  y = zeros (size (x));

  # If any of the argument is single, also the output should be

  if (strcmpi (class (y), "single") || strcmpi (class (a), "single") || strcmpi (class (b), "single"))
    a = single (a);
    b = single (b);
    x = single (x);
    y = single (y);
  endif

  # In the following, we use the fact that the continued fraction we will
  # use is more efficient when x <= a / (a+b).
  # Moreover, to compute the upper version, which is defined as
  # I_x(a,b,"upper") = 1 - I_x(a,b) we used the property
  # I_x(a,b) + I_(1-x) (b,a) = 1.

  if (strcmpi (tail, "upper"))
    flag = (x < a./(a+b));
    x(!flag) = 1 - x(!flag);
    [a(!flag), b(!flag)] = deal (b(!flag), a(!flag));
  elseif (strcmpi (tail, "lower"))
    flag = (x > a./(a+b));
    x (flag) = 1 - x(flag);
    [a(flag), b(flag)] = deal (b(flag), a(flag));
  else
    error ("betainc: invalid value for flag")
  endif

  f = zeros (size (x), class (x));

  ## Continued fractions: CPVWJ, formula 18.5.20, modified Lentz algorithm
  ## implemented in a separate .cc file. This particular continued fraction
  ## gives (B(a,b) * I_x(a,b)) / (x^a * (1-x)^b).

  f = __betainc_lentz__ (x, a, b, strcmpi (class (x), "single"));

  # We divide the continued fraction by B(a,b) / (x^a * (1-x)^b)
  # to obtain I_x(a,b).
  y = a .* log (x) + b .* log1p (-x) + gammaln (a + b) - ...
    gammaln (a) - gammaln (b) + log (f);
  y = real (exp (y));
  y(flag) = 1 - y(flag);
  y = reshape (y, sz);

endfunction

## Tests from betainc.cc

# Double precision
%!test
%! a = [1, 1.5, 2, 3];
%! b = [4, 3, 2, 1];
%! v1 = betainc (1,a,b);
%! v2 = [1,1,1,1];
%! x = [.2, .4, .6, .8];
%! v3 = betainc (x, a, b);
%! v4 = 1 - betainc (1.-x, b, a);
%! assert (v1, v2, sqrt (eps));
%! assert (v3, v4, sqrt (eps));

# Single precision
%!test
%! a = single ([1, 1.5, 2, 3]);
%! b = single ([4, 3, 2, 1]);
%! v1 = betainc (1,a,b);
%! v2 = single ([1,1,1,1]);
%! x = single ([.2, .4, .6, .8]);
%! v3 = betainc (x, a, b);
%! v4 = 1 - betainc (1.-x, b, a);
%! assert (v1, v2, sqrt (eps ("single")));
%! assert (v3, v4, sqrt (eps ("single")));

# Mixed double/single precision
%!test
%! a = single ([1, 1.5, 2, 3]);
%! b = [4, 3, 2, 1];
%! v1 = betainc (1,a,b);
%! v2 = single ([1,1,1,1]);
%! x = [.2, .4, .6, .8];
%! v3 = betainc (x, a, b);
%! v4 = 1-betainc (1.-x, b, a);
%! assert (v1, v2, sqrt (eps ("single")));
%! assert (v3, v4, sqrt (eps ("single")));

## New test

%!test #<51157>
%! y = betainc([0.00780;0.00782;0.00784],250.005,49750.995);
%! y_ex = [0.999999999999989; 0.999999999999992; 0.999999999999995];
%! assert (y, y_ex, -1e-14);

%!assert (betainc (0.001, 20, 30), 2.750687665855991e-47, -3e-14);
%!assert (betainc (0.0001, 20, 30), 2.819953178893307e-67, -3e-14);
%!assert (betainc (0.99, 20, 30, "upper"), 1.5671643161872703e-47, -3e-14);
%!assert (betainc (0.999, 20, 30, "upper"), 1.850806276141535e-77, -3e-14);
%!assert (betainc (0.5, 200, 300), 0.9999964565197356, -1e-15);
%!assert (betainc (0.5, 200, 300, "upper"), 3.54348026439253e-06, -1e-13);

# Test trivial values

%!test
%! [a,b] = ndgrid (linspace(1e-4, 100, 20), linspace(1e-4, 100, 20));
%! assert (betainc (0,a,b), zeros(20));
%! assert (betainc (1,a,b), ones(20));

## Test input validation

%!error betainc ()
%!error betainc (1)
%!error betainc (1,2)
%!error betainc (1.1,1,1)
%!error betainc (-0.1,1,1)
%!error betainc (0.5,0,1)
%!error betainc (0.5,1,0)
%!error betainc (1,2,3,4)
%!error betainc (1,2)
%!error betainc (1,2,3,4,5)
%!error betainc (0.5i, 1, 2)
%!error betainc (0, 1i, 1)
%!error betainc (0, 1, 1i)
