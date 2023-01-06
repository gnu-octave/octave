########################################################################
##
## Copyright (C) 2018-2023 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.
##
########################################################################

## -*- texinfo -*-
## @deftypefn  {} {@var{I} =} betainc (@var{x}, @var{a}, @var{b})
## @deftypefnx {} {@var{I} =} betainc (@var{x}, @var{a}, @var{b}, @var{tail})
## Compute the incomplete beta function.
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
##                           x
##                          /
##                  1       |
## I_x (a, b) = ----------  | t^(a-1) (1-t)^(b-1) dt
##              beta (a,b)  |
##                          /
##                         0
## @end group
## @end example
##
## @end ifnottex
##
## with real @var{x} in the range [0,1].  The inputs @var{a} and @var{b} must
## be real and strictly positive (> 0).  If one of the inputs is not a scalar
## then the other inputs must be scalar or of compatible dimensions.
##
## By default, @var{tail} is @qcode{"lower"} and the incomplete beta function
## integrated from 0 to @var{x} is computed.  If @var{tail} is @qcode{"upper"}
## then the complementary function integrated from @var{x} to 1 is calculated.
## The two choices are related by
##
## betainc (@var{x}, @var{a}, @var{b}, @qcode{"upper"}) =
## 1 - betainc (@var{x}, @var{a}, @var{b}, @qcode{"lower"}).
##
## @code{betainc} uses a more sophisticated algorithm than subtraction to
## get numerically accurate results when the @qcode{"lower"} value is small.
##
## Reference: @nospell{A. Cuyt, V. Brevik Petersen, B. Verdonk, H. Waadeland,
## W.B. Jones}, @cite{Handbook of Continued Fractions for Special Functions},
## ch.@: 18.
##
## @seealso{beta, betaincinv, betaln}
## @end deftypefn

function I = betainc (x, a, b, tail = "lower")

  if (nargin < 3)
    print_usage ();
  endif

  [err, x, a, b] = common_size (x, a, b);
  if (err > 0)
    error ("betainc: X, A, and B must be of common size or scalars");
  endif

  if (iscomplex (x) || iscomplex (a) || iscomplex (b))
    error ("betainc: all inputs must be real");
  endif

  ## Remember original shape of data, but convert to column vector for calcs.
  orig_sz = size (x);
  x = x(:);
  a = a(:);
  b = b(:);

  if (any ((x < 0) | (x > 1)))
    error ("betainc: X must be in the range [0, 1]");
  endif

  if (any (a <= 0))
    error ("betainc: A must be strictly positive");
  endif

  if (any (b <= 0))
    error ("betainc: B must be strictly positive");
  endif

  ## If any of the arguments is single then the output should be as well.
  if (strcmp (class (x), "single") || strcmp (class (a), "single")
      || strcmp (class (b), "single"))
    a = single (a);
    b = single (b);
    x = single (x);
  endif

  ## Convert to floating point if necessary
  if (isinteger (x))
    I = double (x);
  endif
  if (isinteger (a))
    a = double (a);
  endif
  if (isinteger (b))
    b = double (b);
  endif

  ## Initialize output array
  I = zeros (size (x), class (x));

  ## Trivial cases (long code here trades memory for speed)
  a_one = (a == 1);
  b_one = (b == 1);
  a_b_one = a_one & b_one;
  a_not_one = ! a_one;
  b_not_one = ! b_one;
  non_trivial = a_not_one & b_not_one;
  a_one &= b_not_one;
  b_one &= a_not_one;

  if (strcmpi (tail, "lower"))
    I(a_b_one) = x(a_b_one);
    ## See bug #62329.
    ## equivalent to "1 - (1 - x(a_one)) .^ b(a_one)", but less roundoff error
    I(a_one) = - expm1 (log1p (- x(a_one)) .* b(a_one));
    I(b_one) = x(b_one) .^ a(b_one);
  elseif (strcmpi (tail, "upper"))
    I(a_b_one) = 1 - x(a_b_one);
    ## equivalent to "(1 - x(a_one)) .^ b(a_one)", but less roundoff error
    I(a_one) = exp (log1p (- x(a_one)) .* b(a_one));
    ## equivalent to "1 - x(b_one) .^ a(b_one)", but less roundoff error
    I(b_one) = - expm1 (log (x(b_one)) .* a(b_one));
  endif

  ## Non-Trivial cases
  ## In the following, we use the fact that the continued fraction Octave uses
  ## is more efficient when x <= a / (a + b).  Moreover, to compute the upper
  ## version, which is defined as I_x(a,b,"upper") = 1 - I_x(a,b) we use the
  ## property I_x(a,b) + I_(1-x) (b,a) = 1.

  x = x(non_trivial);
  a = a(non_trivial);
  b = b(non_trivial);

  if (strcmpi (tail, "lower"))
    fflag = (x > a./(a + b));
    x(fflag) = 1 - x(fflag);
    [a(fflag), b(fflag)] = deal (b(fflag), a(fflag));
  elseif (strcmpi (tail, "upper"))
    fflag = (x < (a ./ (a + b)));
    x(! fflag) = 1 - x(! fflag);
    [a(! fflag), b(! fflag)] = deal (b(! fflag), a(! fflag));
  else
    error ("betainc: invalid value for TAIL");
  endif

  f = zeros (size (x), class (x));

  ## Continued fractions: CPVWJ, formula 18.5.20, modified Lentz algorithm
  ## implemented in a separate .cc file.  This particular continued fraction
  ## gives (B(a,b) * I_x(a,b)) / (x^a * (1-x)^b).

  f = __betainc__ (x, a, b);

  ## Divide continued fraction by B(a,b) / (x^a * (1-x)^b) to obtain I_x(a,b).
  y_nt = a .* log (x) + b .* log1p (-x) ...
         + (gammaln (a + b) - gammaln (a) - gammaln (b)) + log (f);
  y_nt = real (exp (y_nt));
  y_nt(fflag) = 1 - y_nt(fflag);

  I(non_trivial) = y_nt;

  ## Restore original shape
  I = reshape (I, orig_sz);

endfunction


## Double precision
%!test
%! a = [1, 1.5, 2, 3];
%! b = [4, 3, 2, 1];
%! v1 = betainc (1, a, b);
%! v2 = [1,1,1,1];
%! x = [.2, .4, .6, .8];
%! v3 = betainc (x, a, b);
%! v4 = 1 - betainc (1-x, b, a);
%! assert (v1, v2, sqrt (eps));
%! assert (v3, v4, sqrt (eps));

## Single precision
%!test
%! a = single ([1, 1.5, 2, 3]);
%! b = single ([4, 3, 2, 1]);
%! v1 = betainc (1, a, b);
%! v2 = single ([1,1,1,1]);
%! x = single ([.2, .4, .6, .8]);
%! v3 = betainc (x, a, b);
%! v4 = 1 - betainc (1-x, b, a);
%! assert (v1, v2, sqrt (eps ("single")));
%! assert (v3, v4, sqrt (eps ("single")));

## Mixed double/single precision
%!test
%! a = single ([1, 1.5, 2, 3]);
%! b = [4, 3, 2, 1];
%! v1 = betainc (1,a,b);
%! v2 = single ([1,1,1,1]);
%! x = [.2, .4, .6, .8];
%! v3 = betainc (x, a, b);
%! v4 = 1 - betainc (1. - x, b, a);
%! assert (v1, v2, sqrt (eps ("single")));
%! assert (v3, v4, sqrt (eps ("single")));

%!test <*51157>
%! y = betainc ([0.00780;0.00782;0.00784],250.005,49750.995);
%! y_ex = [0.999999999999989; 0.999999999999992; 0.999999999999995];
%! assert (y, y_ex, -1e-14);

%!assert (betainc (0.001, 20, 30), 2.750687665855991e-47, -3e-14)
%!assert (betainc (0.0001, 20, 30), 2.819953178893307e-67, -7e-14)
%!assert <*54383> (betainc (0.99, 20, 30, "upper"),
%!                 1.5671643161872703e-47, -7e-14)
%!assert (betainc (0.999, 20, 30, "upper"), 1.850806276141535e-77, -7e-14)
%!assert (betainc (0.5, 200, 300), 0.9999964565197356, -1e-15)
%!assert (betainc (0.5, 200, 300, "upper"), 3.54348026439253e-06, -3e-13)

## Test trivial values
%!test
%! [a,b] = ndgrid (linspace (1e-4, 100, 20), linspace (1e-4, 100, 20));
%! assert (betainc (0, a, b), zeros (20));
%! assert (betainc (1, a, b), ones (20));
%! assert (betainc (0, a, b, "upper"), ones (20));
%! assert (betainc (1, a, b, "upper"), zeros (20));

%!test <*34405>
%! assert (betainc (NaN, 1, 2), NaN);
%! assert (betainc (0.5, 1, Inf), 1);

%!test <*62329>
%! assert (betainc (2e-20, 1, 0.5), 1e-20, -1e-15);
%! assert (betainc (2e-5, 1, 0.5), 2e-5 / (1 + sqrt (1 - 2e-5)), -1e-15);
%! assert (betainc (0.99, 1, 0.5, "upper"), 0.1, -1e-15);
%! assert (betainc (0.99, 0.5, 1, "upper"), - expm1 (log (0.99)/2), -1e-15);

## Test input validation
%!error <Invalid call> betainc ()
%!error <Invalid call> betainc (1)
%!error <Invalid call> betainc (1,2)
%!error <must be of common size or scalars> betainc (ones (2,2), ones (1,2), 1)
%!error <all inputs must be real> betainc (0.5i, 1, 2)
%!error <all inputs must be real> betainc (0, 1i, 1)
%!error <all inputs must be real> betainc (0, 1, 1i)
%!error <X must be in the range \[0, 1\]> betainc (-0.1,1,1)
%!error <X must be in the range \[0, 1\]> betainc (1.1,1,1)
%!error <X must be in the range \[0, 1\]>
%! x = ones (1, 1, 2);
%! x(1,1,2) = -1;
%! betainc (x,1,1);
%!error <A must be strictly positive> betainc (0.5,0,1)
%!error <A must be strictly positive>
%! a = ones (1, 1, 2);
%! a(1,1,2) = 0;
%! betainc (1,a,1);
%!error <B must be strictly positive> betainc (0.5,1,0)
%!error <B must be strictly positive>
%! b = ones (1, 1, 2);
%! b(1,1,2) = 0;
%! betainc (1,1,b);
%!error <invalid value for TAIL> betainc (1,2,3, "foobar")
