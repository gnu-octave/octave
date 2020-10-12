########################################################################
##
## Copyright (C) 2017-2020 The Octave Project Developers
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
## @deftypefn  {} {} betaincinv (@var{y}, @var{a}, @var{b})
## @deftypefnx {} {} betaincinv (@var{y}, @var{a}, @var{b}, "lower")
## @deftypefnx {} {} betaincinv (@var{y}, @var{a}, @var{b}, "upper")
## Compute the inverse of the normalized incomplete beta function.
##
## The normalized incomplete beta function is defined as
## @tex
## $$
##  I_x (a, b) = {1 \over {B(a,b)}} \displaystyle{\int_0^x t^{a-1} (1-t)^{b-1} dt}
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
## If two inputs are scalar, then @code{betaincinv (@var{y}, @var{a}, @var{b})}
## is returned for each of the other inputs.
##
## If two or more inputs are not scalar, the sizes of them must agree, and
## @code{betaincinv} is applied element-by-element.
##
## The variable @var{y} must be in the interval [0,1], while @var{a} and
## @var{b} must be real and strictly positive.
##
## By default, @var{tail} is @qcode{"lower"} and the inverse of the incomplete
## beta function integrated from 0 to @var{x} is computed.  If @var{tail} is
## @qcode{"upper"} then the complementary function integrated from @var{x} to 1
## is inverted.
##
## The function is computed by standard Newton's method, by solving
## @tex
## $$
##  y - I_x (a, b) = 0
## $$
## @end tex
## @ifnottex
##
## @example
## @var{y} - betainc (@var{x}, @var{a}, @var{b}) = 0
## @end example
##
## @end ifnottex
##
## @seealso{betainc, beta, betaln}
## @end deftypefn

function x = betaincinv (y, a, b, tail = "lower")

  if (nargin < 3)
    print_usage ();
  endif

  [err, y, a, b] = common_size (y, a, b);
  if (err > 0)
    error ("betaincinv: Y, A, and B must be of common size or scalars");
  endif

  if (iscomplex (y) || iscomplex (a) || iscomplex (b))
    error ("betaincinv: all inputs must be real");
  endif

  ## FIXME: Should there be isnumeric checking?  Right now it accepts char
  ##        arrays, but then produces a weird error later on.

  ## Remember original shape of data, but convert to column vector for calcs.
  orig_sz = size (y);
  y = y(:);
  a = a(:);
  b = b(:);

  if (any ((y < 0) | (y > 1)))
    error ("betaincinv: Y must be in the range [0, 1]");
  endif

  if (any (a <= 0))
    error ("betaincinv: A must be strictly positive");
  endif

  if (any (b <= 0))
    error ("betaincinv: B must be strictly positive");
  endif

  ## If any of the arguments is single then the output should be as well.
  if (strcmp (class (y), "single") || strcmp (class (a), "single")
      || strcmp (class (b), "single"))
    y = single (y);
    a = single (a);
    b = single (b);
  endif

  ## Convert to floating point if necessary
  if (isinteger (y))
    y = double (y);
  endif
  if (isinteger (a))
    a = double (a);
  endif
  if (isinteger (b))
    b = double (b);
  endif

  ## Initialize output array
  x = zeros (size (y), class (y));

  ## Parameters for the Newton method
  maxit = 20;
  tol = eps (class (y));

  if (strcmpi (tail, "lower"))
    p = y;
    q = 1 - y;
    x(y == 0) = 0;
    x(y == 1) = 1;
  elseif (strcmpi (tail, "upper"))
    p = 1 - y;
    q = y;
    x(y == 0) = 1;
    x(y == 1) = 0;
  else
    error ("betaincinv: invalid value for TAIL")
  endif

  ## Special values have been already computed.
  todo = (y != 0) & (y != 1);

  ## We will invert the lower version for p < 0.5 and the upper otherwise.
  i_low = (p < 0.5);
  i_upp = (! i_low);

  idx = todo & i_low;
  if (any (idx))
    n = nnz (idx);
    ## Function and derivative of the lower version.
    F = @(x, a, b, y) y - betainc (x, a, b);
    JF = @(x, a, b) - real (exp ((a-1) .* log (x) + (b-1) .* log1p (-x) + ...
                            gammaln (a+b) - gammaln (a) - gammaln (b)));

    ## Compute the initial guess with a bisection method of 10 steps.
    x0 = bisection_method (F, zeros (n,1), ones (n,1), ...
                           a(i_low), b(i_low), p(i_low), 10);

    ## Use Newton's method to iteratively find solution.
    x(i_low) = newton_method (F, JF, x0, a(i_low), b(i_low), p(i_low), ...
                              tol, maxit);
  endif

  idx = todo & i_upp;
  if (any (idx))
    n = nnz (idx);
    ## Function and derivative of the upper version.
    F = @(x, a, b, y) y - betainc (x, a, b, "upper");
    JF = @(x, a, b) real (exp ((a-1) .* log (x) + (b-1) .* log1p (-x) + ...
                          gammaln (a+b) - gammaln (a) - gammaln (b)));

    ## Compute the initial guess with a bisection method of 10 steps.
    x0 = bisection_method (F, zeros (n,1), ones (n,1), ...
                           a(i_upp), b(i_upp), q(i_upp), 10);

    ## Use Newton's method to iteratively find solution.
    x(i_upp) = newton_method (F, JF, x0, a(i_upp), b(i_upp), q(i_upp), ...
                              tol, maxit);
  endif

  ## Restore original shape
  x = reshape (x, orig_sz);

endfunction


## subfunctions: Bisection and Newton Methods
function xc = bisection_method (F, xl, xr, a, b, y, maxit)
  F_l = F (xl, a, b, y);
  F_r = F (xr, a, b, y);
  for it = 1:maxit
    xc = (xl + xr) / 2;
    F_c = F (xc, a, b, y);
    flag_l = ((F_c .* F_r) < 0);
    flag_r = ((F_c .* F_l) < 0);
    flag_c = (F_c == 0);
    xl(flag_l) = xc(flag_l);
    xr(flag_r) = xc(flag_r);
    xl(flag_c) = xr(flag_c) = xc(flag_c);
    F_l(flag_l) = F_c(flag_l);
    F_r(flag_r) = F_c(flag_r);
    F_l(flag_c) = F_r(flag_c) = 0;
  endfor
endfunction

function x = newton_method (F, JF, x0, a, b, y, tol, maxit);
  l = numel (y);
  res = -F (x0, a, b, y) ./ JF (x0, a, b);
  todo = (abs(res) >= tol * abs (x0));
  x = x0;
  it = 0;
  while (any (todo) && (it < maxit))
    it++;
    x(todo) += res(todo);
    res(todo) = -F(x(todo), a(todo), b(todo), y(todo)) ...
                ./ JF (x(todo), a(todo), b(todo));
    todo = (abs(res) >= tol * abs (x));
  endwhile
  x += res;
endfunction


%!test
%! x = linspace (0.1, 0.9, 11);
%! a = [2, 3, 4];
%! [x,a,b] = ndgrid (x,a,a);
%! xx = betaincinv (betainc (x, a, b), a, b);
%! assert (xx, x, 3e-15);

%!test
%! x = linspace (0.1, 0.9, 11);
%! a = [2, 3, 4];
%! [x,a,b] = ndgrid (x,a,a);
%! xx = betaincinv (betainc (x, a, b, "upper"), a, b, "upper");
%! assert (xx, x, 3e-15);

%!test
%! x = linspace (0.1, 0.9, 11);
%! a = [0.1:0.1:1];
%! [x,a,b] = ndgrid (x,a,a);
%! xx = betaincinv (betainc (x, a, b), a, b);
%! assert (xx, x, 5e-15);

%!test
%! x = linspace (0.1, 0.9, 11);
%! a = [0.1:0.1:1];
%! [x,a,b] = ndgrid (x,a,a);
%! xx = betaincinv (betainc (x, a, b, "upper"), a, b, "upper");
%! assert (xx, x, 5e-15);

## Test the conservation of the input class
%!assert (class (betaincinv (0.5, 1, 1)), "double")
%!assert (class (betaincinv (single (0.5), 1, 1)), "single")
%!assert (class (betaincinv (0.5, single (1), 1)), "single")
%!assert (class (betaincinv (int8 (0), 1, 1)), "double")
%!assert (class (betaincinv (0.5, int8 (1), 1)), "double")
%!assert (class (betaincinv (int8 (0), single (1), 1)), "single")
%!assert (class (betaincinv (single (0.5), int8 (1), 1)), "single")

## Test input validation
%!error <Invalid call> betaincinv ()
%!error <Invalid call> betaincinv (1)
%!error <Invalid call> betaincinv (1,2)
%!error <must be of common size or scalars>
%! betaincinv (ones (2,2), ones (1,2), 1);
%!error <all inputs must be real> betaincinv (0.5i, 1, 2)
%!error <all inputs must be real> betaincinv (0, 1i, 1)
%!error <all inputs must be real> betaincinv (0, 1, 1i)
%!error <Y must be in the range \[0, 1\]> betaincinv (-0.1,1,1)
%!error <Y must be in the range \[0, 1\]> betaincinv (1.1,1,1)
%!error <Y must be in the range \[0, 1\]>
%! y = ones (1, 1, 2);
%! y(1,1,2) = -1;
%! betaincinv (y,1,1);
%!error <A must be strictly positive> betaincinv (0.5,0,1)
%!error <A must be strictly positive>
%! a = ones (1, 1, 2);
%! a(1,1,2) = 0;
%! betaincinv (1,a,1);
%!error <B must be strictly positive> betaincinv (0.5,1,0)
%!error <B must be strictly positive>
%! b = ones (1, 1, 2);
%! b(1,1,2) = 0;
%! betaincinv (1,1,b);
%!error <invalid value for TAIL> betaincinv (1,2,3, "foobar")
