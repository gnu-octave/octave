## Copyright (C) 2017 Michele Ginesi
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## Author: Michele Ginesi <michele.ginesi@gmail.com>

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
## The variable @var{y} must be in the interval [0,1], while @var{a} and @var{b}
## must be real and strictly positive.
##
## By default the inverse of the incomplete beta function integrated from 0
## to @var{x} is computed.  If @qcode{"upper"} is given then the complementary
## function integrated from @var{x} to 1 is inverted.
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
## @group
##
## @var{y} - betainc (@var{x}, @var{a}, @var{b}) = 0
##
## @end group
## @end example
##
## @end ifnottex
##
##
## @seealso{beta, betainc, betaln}
## @end deftypefn

function [x] = betaincinv (y, a, b, tail = "lower")

  if (nargin > 4 || nargin < 3)
    print_usage ();
  endif

  if (! isscalar (y) || ! isscalar (a) || ! isscalar (b))
    [err, y, a, b] = common_size (y, a, b);
    if (err > 0)
      error ("betaincinv: y, a must be of common size or scalars");
    endif
  endif

  if (iscomplex (y) || iscomplex (a) || iscomplex (b))
    error ("betaincinv: inputs must be real or integer");
  endif

  if (any (a <= 0) | any (b <= 0))
    error ("betaincinv: a must be strictly positive");
  endif

  if (any (y > 1 | y < 0))
    error ("betaincinv: y must be between 0 and 1");
  endif

  if (isinteger (y))
    y = double (y);
  endif

  if (isinteger (a))
    a = double (a);
  endif

  if (isinteger (b))
    b = double (b);
  endif


  # Extract the size.
  sz = size (y);
  # Write the inputs as two column vectors.
  y = y(:);
  a = a(:);
  b = b(:);
  l = length (y);
  # Initialise the output.
  x = zeros (l, 1);

  # If one of the inputs is of single type, also the output should be

  if (strcmpi (class (y), "single") || strcmpi (class (a), "single") || strcmpi (class (b), "single"))
    a = single (a);
    b = single (b);
    y = single (y);
    x = single (x);
  endif

  # Parameters of the Newton method
  maxit = 20;
  tol = eps (class (y));

  if (strcmpi (tail, "upper"))
    p = 1 - y;
    q = y;
    x(y == 0) = 1;
    x(y == 1) = 0;
  elseif (strcmpi (tail, "lower"))
    p = y;
    q = 1 - y;
    x(y == 0) = 0;
    x(y == 1) = 1;
  else
    error ("betaincinv: invalid value for tail")
  endif

  # Trivial values have been already computed.
  i_miss = ((y != 0) & (y != 1));

  # We will invert the lower version for p < 0.5 and the upper otherwise.
  i_low = (p < 0.5);
  i_upp = (!i_low);

  len_low = nnz (i_miss & i_low);
  len_upp = nnz (i_miss & i_upp);

  if (any (i_miss & i_low));
    # Function and derivative of the lower version.
    F = @(x, a, b, y) y - betainc (x, a, b);
    JF = @(x, a, b) - real (exp ((a-1) .* log (x) + (b-1) .* log1p (-x) + ...
        gammaln (a+b) - gammaln (a) - gammaln (b)));
    # We compute the initial guess with a bisection method of 10 steps.
    x0 = bisection_method (F, zeros (len_low,1), ones (len_low,1),...
        a(i_low), b(i_low), p(i_low), 10);
    x(i_low) = newton_method (F, JF, x0, a(i_low), b(i_low), p(i_low), ...
        tol, maxit);
  endif

  if (any (i_miss & i_upp));
    # Function and derivative of the lower version.
    F = @(x, a, b, y) y - betainc (x, a, b, "upper");
    JF = @(x, a, b) real (exp ((a-1) .* log (x) + (b-1) .* log1p (-x) + ...
        gammaln (a+b) - gammaln (a) - gammaln (b)));
    # We compute the initial guess with a bisection method of 10 steps.
    x0 = bisection_method (F, zeros (len_upp,1), ones (len_upp,1),...
        a(i_upp), b(i_upp), q(i_upp), 10);
    x(i_upp) = newton_method (F, JF, x0, a(i_upp), b(i_upp), q(i_upp), ...
        tol, maxit);
  endif

  ## Re-organize the output.

  x = reshape (x, sz);

endfunction


## Subfunctions: Bisection and Newton Methods

function xc = bisection_method (F, xl, xr, a, b, y, maxit)
    F_l = feval (F, xl, a, b, y);
    F_r = feval (F, xr, a, b, y);
   for it = 1:maxit
    xc = (xl + xr) / 2;
    F_c = feval (F, xc, a, b, y);
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
  l = length (y);
  res = -feval (F, x0, a, b, y) ./ feval (JF, x0, a, b);
  i_miss = (abs(res) >= tol * abs (x0));
  x = x0;
  it = 0;
  while (any (i_miss) && (it < maxit))
    it++;
    x(i_miss) += res(i_miss);
    res(i_miss) = - feval (F, x(i_miss), a(i_miss), b(i_miss), ...
        y(i_miss)) ./ feval (JF, x(i_miss), a(i_miss), b(i_miss));
    i_miss = (abs(res) >= tol * abs (x));
  endwhile
  x += res;
endfunction


## Test

%!test
%! x = linspace(0.1, 0.9, 11);
%! a = [2, 3, 4];
%! [x,a,b] = ndgrid (x,a,a);
%! xx = betaincinv (betainc (x, a, b), a, b);
%! assert (xx, x, 3e-15);

%!test
%! x = linspace(0.1, 0.9, 11);
%! a = [2, 3, 4];
%! [x,a,b] = ndgrid (x,a,a);
%! xx = betaincinv (betainc (x, a, b, "upper"), a, b, "upper");
%! assert (xx, x, 3e-15);

%!test
%! x = linspace(0.1, 0.9, 11);
%! a = [0.1:0.1:1];
%! [x,a,b] = ndgrid (x,a,a);
%! xx = betaincinv (betainc (x, a, b), a, b);
%! assert (xx, x, 3e-15);

%!test
%! x = linspace(0.1, 0.9, 11);
%! a = [0.1:0.1:1];
%! [x,a,b] = ndgrid (x,a,a);
%! xx = betaincinv (betainc (x, a, b, "upper"), a, b, "upper");
%! assert (xx, x, 3e-15);

## Test on the conservation of the class
%!assert (class (betaincinv (0.5, 1, 1)), "double")
%!assert (class (betaincinv (single (0.5), 1, 1)), "single")
%!assert (class (betaincinv (0.5, single (1), 1)), "single")
%!assert (class (betaincinv (int8 (0), 1, 1)), "double")
%!assert (class (betaincinv (0.5, int8 (1), 1)), "double")
%!assert (class (betaincinv (int8 (0), single (1), 1)), "single")
%!assert (class (betaincinv (single (0.5), int8 (1), 1)), "single")

## Test input validation
%!error betaincinv ()
%!error betaincinv (1)
%!error betaincinv (1,2,3,4)
%!error betaincinv (1, "2")
%!error betaincinv (0.5i, 1, 2)
%!error betaincinv (0, 1i, 1)
%!error betaincinv (0, 1, 1i)
