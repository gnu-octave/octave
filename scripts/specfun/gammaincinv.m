########################################################################
##
## Copyright (C) 2017-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{x} =} gammaincinv (@var{y}, @var{a})
## @deftypefnx {} {@var{x} =} gammaincinv (@var{y}, @var{a}, @var{tail})
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
## for each non-negative value of @var{x}.  If @var{a} is scalar then
## @code{gammaincinv (@var{y}, @var{a})} is returned for each element of
## @var{y} and vice versa.
##
## If neither @var{y} nor @var{a} is scalar then the sizes of @var{y} and
## @var{a} must agree, and @code{gammaincinv} is applied element-by-element.
## The variable @var{y} must be in the interval @math{[0,1]} while @var{a} must
## be real and positive.
##
## By default, @var{tail} is @qcode{"lower"} and the inverse of the incomplete
## gamma function integrated from 0 to @var{x} is computed.  If @var{tail} is
## @qcode{"upper"}, then the complementary function integrated from @var{x} to
## infinity is inverted.
##
## The function is computed with Newton's method by solving
## @tex
## $$
##  y - \gamma (x, a) = 0
## $$
## @end tex
## @ifnottex
##
## @example
## @var{y} - gammainc (@var{x}, @var{a}) = 0
## @end example
##
## @end ifnottex
##
## Reference: @nospell{A. Gil, J. Segura, and N. M. Temme}, @cite{Efficient and
## accurate algorithms for the computation and inversion of the incomplete
## gamma function ratios}, @nospell{SIAM J. Sci.@: Computing}, pp.@:
## A2965--A2981, Vol 34, 2012.
##
## @seealso{gammainc, gamma, gammaln}
## @end deftypefn

function x = gammaincinv (y, a, tail = "lower")

  if (nargin < 2)
    print_usage ();
  endif

  [err, y, a] = common_size (y, a);
  if (err > 0)
    error ("gammaincinv: Y and A must be of common size or scalars");
  endif

  if (iscomplex (y) || iscomplex (a))
    error ("gammaincinv: all inputs must be real");
  endif

  ## Remember original shape of data, but convert to column vector for calcs.
  orig_sz = size (y);
  y = y(:);
  a = a(:);

  if (any ((y < 0) | (y > 1)))
    error ("gammaincinv: Y must be in the range [0, 1]");
  endif

  if (any (a <= 0))
    error ("gammaincinv: A must be strictly positive");
  endif

  ## If any of the arguments is single then the output should be as well.
  if (strcmp (class (y), "single") || strcmp (class (a), "single"))
    y = single (y);
    a = single (a);
  endif

  ## Convert to floating point if necessary
  if (isinteger (y))
    y = double (y);
  endif
  if (isinteger (a))
    a = double (a);
  endif

  ## Initialize output array
  x = zeros (size (y), class (y));

  maxit = 20;
  tol = eps (class (y));

  ## Special cases, a = 1 or y = 0, 1.

  if (strcmpi (tail, "lower"))
    x(a == 1) = - log1p (- y(a == 1));
    x(y == 0) = 0;
    x(y == 1) = Inf;
    p = y;
    q = 1 - p;
  elseif (strcmpi (tail, "upper"))
    x(a == 1) = - log (y(a == 1));
    x(y == 0) = Inf;
    x(y == 1) = 0;
    q = y;
    p = 1 - q;
  else
    error ("gammaincinv: invalid value for TAIL");
  endif

  todo = (a != 1) & (y != 0) & (y != 1);

  ## Case 1: p small.

  i_flag_1 = todo & (p < ((0.2 * (1 + a)) .^ a) ./ gamma (1 + a));

  if (any (i_flag_1))
    aa = a(i_flag_1);
    pp = p(i_flag_1);

    ## Initial guess.

    r = (pp .* gamma (1 + aa)) .^ (1 ./ aa);

    c2 = 1 ./ (aa + 1);
    c3 = (3  * aa + 5) ./ (2 * (aa + 1) .^2 .* (aa + 2));
    c4 = (8 * aa .^ 2 + 33 * aa + 31) ./ (3 * (aa + 1) .^ 3 .* (aa + 2) .* ...
         (aa + 3));
    c5 = (125 * aa .^ 4 + 1179 * aa .^ 3 + 3971 * aa.^2 + 5661 * aa + 2888) ...
         ./ (24 * (1 + aa) .^4 .* (aa + 2) .^ 2 .* (aa + 3) .* (aa + 4));

    ## FIXME: Would polyval() be better here for more accuracy?
    x0 = r + c2 .* r .^ 2 + c3 .* r .^ 3 + c4 .* r .^4 + c5 .* r .^ 5;

    ## For this case we invert the lower version.

    F = @(p, a, x) p - gammainc (x, a, "lower");
    JF = @(a, x) - exp (- gammaln (a) - x + (a - 1) .* log (x));
    x(i_flag_1) = newton_method (F, JF, pp, aa, x0, tol, maxit);
  endif

  todo(i_flag_1) = false;

  ## Case 2: q small.

  i_flag_2 = (q < exp (- 0.5 * a) ./ gamma (1 + a)) & (a > 0) & (a < 10);
  i_flag_2 &= todo;

  if (any (i_flag_2))
    aa = a(i_flag_2);
    qq = q(i_flag_2);

    ## Initial guess.

    x0 = (-log (qq) - gammaln (aa));

    ## For this case, we invert the upper version.

    F = @(q, a, x) q - gammainc (x, a, "upper");
    JF = @(a, x) exp (- gammaln (a) - x) .* x .^ (a - 1);
    x(i_flag_2) = newton_method (F, JF, qq, aa, x0, tol, maxit);
  endif

  todo(i_flag_2) = false;

  ## Case 3: a small.

  i_flag_3 = todo & ((a > 0) & (a < 1));

  if (any (i_flag_3))
    aa = a(i_flag_3);
    pp = p(i_flag_3);

    ## Initial guess

    xl = (pp .* gamma (aa + 1)) .^ (1 ./ aa);
    x0 = xl;

    ## For this case, we invert the lower version.

    F = @(p, a, x) p - gammainc (x, a, "lower");
    JF = @(a, x) - exp (-gammaln (a) - x) .* x .^ (a - 1);
    x(i_flag_3) = newton_method (F, JF, pp, aa, x0, tol, maxit);
  endif

  todo(i_flag_3) = false;

  ## Case 4: a large.

  i_flag_4 = todo;

  if (any (i_flag_4))
    aa = a(i_flag_4);
    qq = q(i_flag_4);

    ## Initial guess

    d = 1 ./ (9 * aa);
    t = 1 - d + sqrt (2) * erfcinv (2 * qq) .* sqrt (d);
    x0 = aa .* (t .^ 3);

    ## For this case, we invert the upper version.

    F = @(q, a, x) q - gammainc (x, a, "upper");
    JF = @(a, x) exp (- gammaln (a) - x + (a - 1) .* log (x));
    x(i_flag_4) = newton_method (F, JF, qq, aa, x0, tol, maxit);
  endif

  ## Restore original shape
  x = reshape (x, orig_sz);

endfunction

## subfunction: Newton's Method
function x = newton_method (F, JF, y, a, x0, tol, maxit)

  l = numel (y);
  res = -F (y, a, x0) ./ JF (a, x0);
  todo = (abs (res) >= tol * abs (x0));
  x = x0;
  it = 0;
  while (any (todo) && (it++ < maxit))
    x(todo) += res(todo);
    res(todo) = -F (y(todo), a(todo), x(todo)) ./ JF (a(todo), x(todo));
    todo = (abs (res) >= tol * abs (x));
  endwhile
  x += res;

endfunction


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

%!test <*56453>
%! assert (gammaincinv (1e-15, 1) * 2, 2e-15, -1e-15);
%! assert (gammaincinv (1e-16, 1) * 2, 2e-16, -1e-15);

## Test the conservation of the input class
%!assert (class (gammaincinv (0.5, 1)), "double")
%!assert (class (gammaincinv (single (0.5), 1)), "single")
%!assert (class (gammaincinv (0.5, single (1))), "single")
%!assert (class (gammaincinv (int8 (0), 1)), "double")
%!assert (class (gammaincinv (0.5, int8 (1))), "double")
%!assert (class (gammaincinv (int8 (0), single (1))), "single")
%!assert (class (gammaincinv (single (0.5), int8 (1))), "single")

## Test input validation
%!error <Invalid call> gammaincinv ()
%!error <Invalid call> gammaincinv (1)
%!error <must be of common size or scalars>
%! gammaincinv (ones (2,2), ones (1,2), 1);
%!error <all inputs must be real> gammaincinv (0.5i, 1)
%!error <all inputs must be real> gammaincinv (0, 1i)
%!error <Y must be in the range \[0, 1\]> gammaincinv (-0.1,1)
%!error <Y must be in the range \[0, 1\]> gammaincinv (1.1,1)
%!error <Y must be in the range \[0, 1\]>
%! y = ones (1, 1, 2);
%! y(1,1,2) = -1;
%! gammaincinv (y,1);
%!error <A must be strictly positive> gammaincinv (0.5, 0)
%!error <A must be strictly positive>
%! a = ones (1, 1, 2);
%! a(1,1,2) = 0;
%! gammaincinv (1,a,1);
%!error <invalid value for TAIL> gammaincinv (1,2, "foobar")
