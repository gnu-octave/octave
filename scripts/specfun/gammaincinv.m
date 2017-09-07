## Copyright (C) 2017 Michele Ginesi
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

## Author: Michele Ginesi <michele.ginesi@gmail.com>

## -*- texinfo -*-
## @deftypefn  {} {} gammaincinv (@var{y}, @var{a})
## @deftypefnx {} {} gammaincinv (@var{y}, @var{a}, @var{tail})
## Compute the inverse of the normalized incomplete gamma function.
##
## The normalized incomplete gamma function is defined as
## @tex
## $$
##  \gamma (x, a) = {1 \over {\Gamma (a)}}\displaystyle{\int_0^x t^{a-1} e^{-t} dt}
## $$
## @end tex
## @ifnottex
##
## @example
## @group
##                                 x
##                        1       /
## gammainc (x, a) = ---------    | exp (-t) t^(a-1) dt
##                   gamma (a)    /
##                             t=0
## @end group
## @end example
##
## @end ifnottex
##
## and @code{gammaincinv (gammainc (@var{x}, @var{a}), @var{a}) = @var{x}}
## for each nonnegative value of @var{x}.
## If @var{a} is scalar, then @code{gammaincinv (@var{y}, @var{a})} is
## returned for each element of @var{y} and vice versa.
##
## If neither @var{y} nor @var{a} is scalar, the sizes of @var{y} and
## @var{a} must agree, and @code{gammaincinv} is applied element-by-element.
## The elements of @var{y} must be in @math{[0,1]} and those of @var{a}
## must be positive.
##
## By default or if @var{tail} is @qcode{"lower"} the inverse of the
## incomplete gamma function integrated from 0 to @var{x} is computed.
## If @var{tail} is @qcode{"upper"}, then the complementary function
##  integrated from @var{x} to infinity is inverted.
##
## The function is computed by standard Newton's method, by solving
## @tex
## $$
##  y - \gamma (x, a) = 0
## $$
## @end tex
## @ifnottex
##
## @example
## @group
##
## @var{y} - gammainc (@var{x}, @var{a}) = 0
##
## @end group
## @end example
##
## @end ifnottex
##
## Reference: @nospell{A. Gil, J. Segura, and N. M. Temme},
## @cite{Efficient and accurate
## algorithms for the computation and inversion of the incomplete
## gamma function ratios},
## @nospell{SIAM J. Sci. Computing},
## pp. A2965--A2981, Vol 34, 2012.
##
## @seealso{gamma, gammainc, gammaln}
## @end deftypefn

function [x] = gammaincinv (y, a, tail = "lower")

  if (nargin >= 4 || nargin <= 1)
    print_usage ();
  endif

  if (! isscalar (y) || ! isscalar (a))
    [err, y, a] = common_size (y, a);
    if (err > 0)
      error ("gammaincinv: y, a must be of common size or scalars");
    endif
  endif

  if (any (a <= 0))
    error ("gammaincinv: a must be strictly positive");
  endif

  if (any (y > 1 | y < 0))
    error ("gammaincinv: y must be between 0 and 1");
  endif

  if (isinteger (y))
    y = double (y);
  endif

  if (isinteger (a))
    a = double (a);
  endif

  maxit = 20;
  # Extract the size.
  sz = size (y);
  # Write the inputs as two column vectors.
  y = y(:);
  a = a(:);
  l = length (y);
  # Initialise the output.
  x = zeros (l, 1);

  if (strcmpi (class (y), "single") || strcmpi (class (a), "single"))
    a = single (a);
    y = single (y);
    x = single (x);
  endif

  tol = eps (class (y));

  # special cases, a = 1 or y = 0, 1.

  if strcmpi (tail, "lower")
    x(a == 1) = - log (1 - y(a == 1));
    x(y == 0) = 0;
    x(y == 1) = Inf;
    p = y;
    q = 1 - p;
  elseif strcmpi (tail, "upper")
    x(a == 1) = - log (y(a == 1));
    x(y == 0) = Inf;
    x(y == 1) = 0;
    q = y;
    p = 1 - q;
  else
    error ("gammaincinv: invalid value for tail")
  endif

  i_miss = ((y != 0) & (y != 1) & (a != 1));

  ## Case 1: p small.

  i_flag_1 = p < ((0.2 * (1 + a)) .^ a) ./ gamma (1 + a);
  i_flag_1 = ((i_flag_1) & (i_miss));

  aa = a(i_flag_1);
  pp = p(i_flag_1);

  # Initial guess.

  r = ((pp .* gamma (1 + aa)) .^ (1 ./ aa));

  c2 = 1 ./ (aa + 1);
  c3 = (3  * aa + 5) ./ (2 * (aa + 1) .^2 .* (aa + 2));
  c4 = (8 * aa .^ 2 + 33 * aa + 31) ./ (3 * (aa + 1) .^ 3 .* (aa + 2) .* ...
    (aa + 3));
  c5 = (125 * aa .^ 4 + 1179 * aa .^ 3 + 3971 * aa.^2 + 5661 * aa + 2888) ...
    ./ (24 * (1 + aa) .^4 .* (aa + 2) .^ 2 .* (aa + 3) .* (aa + 4));

  x0 = r + c2 .* r .^ 2 + c3 .* r .^ 3 + c4 .* r .^4 + c5 .* r .^ 5;

  # For this case we invert the lower version.

  F = @(p, a, x) p - gammainc (x, a, "lower");
  JF = @(a, x) - exp (-gammaln (a) - x + (a - 1) .* log (x));
  x(i_flag_1) = newton_method (F, JF, pp, aa, x0, tol, maxit);

  i_miss = ((i_miss) & (! i_flag_1));

  ## Case 2: q small.

  i_flag_2 = ((q < exp (- 0.5 * a) ./ gamma (1 + a)) & (a < 10) & (a > 0));
  i_flag_2 = ((i_flag_2) & (i_miss));

  aa = a(i_flag_2);
  qq = q(i_flag_2);

  # Initial guess.

  x0 = (-log (qq) - gammaln (aa));

  # For this case, we invert the upper version.

  F = @(q, a, x) q - gammainc (x, a, "upper");
  JF = @(a, x) exp (- gammaln (a) - x) .* x .^ (a - 1);
  x(i_flag_2) = newton_method (F, JF, qq, aa, x0, tol, maxit);

  i_miss = ((i_miss) & (! i_flag_2));

  ## Case 3: a small.

  i_flag_3 = ((a > 0) & (a < 1));
  i_flag_3 = ((i_flag_3) & (i_miss));

  aa = a(i_flag_3);
  pp = p(i_flag_3);

  # Initial guess

  xl = ((pp .* gamma (aa + 1)) .^ (1 ./ aa));
  x0 = xl;

  # For this case, we invert the lower version.

  F = @(p, a, x) p - gammainc (x, a, "lower");
  JF = @(a, x) - exp (-gammaln (a) - x) .* x .^ (a - 1);
  x(i_flag_3) = newton_method (F, JF, pp, aa, x0, tol, maxit);

  i_miss = ((i_miss) & (! i_flag_3));

  ## Case 4: a large.

  i_flag_4 = i_miss;

  aa = a(i_flag_4);
  qq = q(i_flag_4);

  # Initial guess

  d = 1 ./ (9 * aa);
  t = 1 - d - norminv (qq) .* sqrt(d);
  x0 = aa .* (t .^ 3);

  # For this case, we invert the upper version.

  F = @(q, a, x) q - gammainc (x, a, "upper");
  JF = @(a, x) exp (- gammaln (a) - x + (a - 1) .* log (x));
  x(i_flag_4) = newton_method (F, JF, qq, aa, x0, tol, maxit);

  ## Reshape the output.

  x = reshape (x, sz);
endfunction

## Subfunction: Newton Method

function x = newton_method (F, JF, y, a, x0, tol, maxit);
  l = length (y);
  res = -feval (F, y, a, x0) ./ feval (JF, a, x0);
  i_miss = (abs(res) >= tol * abs (x0));
  x = x0;
  it = 0;
  while (any (i_miss) && (it < maxit))
    it++;
    x(i_miss) += res(i_miss);
    res(i_miss) = - feval (F, y(i_miss), a(i_miss), x(i_miss)) ./ ...
                    feval (JF, a(i_miss), x(i_miss));
    i_miss = (abs(res) >= tol * abs (x));
  endwhile
  x += res;
endfunction


## Test

%!test
%! x = [1e-10, 1e-09, 1e-08, 1e-07];
%! a = [2, 3, 4];
%! [x, a] = ndgrid (x, a);
%! xx = gammainc (gammaincinv (x, a), a);
%! assert (xx, x, -3e-14);

%!test
%! x = [1e-10, 1e-09, 1e-08, 1e-07];
%! a = [2, 3, 4];
%! [x, a] = ndgrid (x, a);
%! xx = gammainc (gammaincinv (x, a, "upper"), a, "upper");
%! assert (xx, x, -3e-14);

%!test
%! x = linspace (0, 1)';
%! a = [linspace(0.1, 1, 10), 2:5];
%! [x, a] = ndgrid (x, a);
%! xx = gammainc (gammaincinv (x, a), a);
%! assert (xx, x, -1e-13);

%!test
%! x = linspace (0, 1)';
%! a = [linspace(0.1, 1, 10), 2:5];
%! [x, a] = ndgrid (x, a);
%! xx = gammainc (gammaincinv (x, a, "upper"), a, "upper");
%! assert (xx, x, -1e-13);


## Test on the conservation of the class
%!assert (class (gammaincinv (0.5, 1)), "double")
%!assert (class (gammaincinv (single (0.5), 1)), "single")
%!assert (class (gammaincinv (0.5, single (1))), "single")
%!assert (class (gammaincinv (int8 (0), 1)), "double")
%!assert (class (gammaincinv (0.5, int8 (1))), "double")
%!assert (class (gammaincinv (int8 (0), single (1))), "single")
%!assert (class (gammaincinv (single (0.5), int8 (1))), "single")

## Test input validation
%!error gammaincinv ()
%!error gammaincinv (1)
%!error gammaincinv (1, 2, 3, 4)
%!error gammaincinv (1, "2")
