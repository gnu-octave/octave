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
## @deftypefn  {} {@var{x} =} betaincinv (@var{y}, @var{a}, @var{b})
## @deftypefnx {} {@var{x} =} betaincinv (@var{y}, @var{a}, @var{b}, "lower")
## @deftypefnx {} {@var{x} =} betaincinv (@var{y}, @var{a}, @var{b}, "upper")
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

  if (! (isfloat (y) && isfloat (a) && isfloat (b)
         && isreal (y) && isreal (a) && isreal (b)))
    error ("betaincinv: Y, A, and B must be real, floating point values");
  endif

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
  if (isa (y, "single") || isa (a, "single") || isa (b, "single"))
    y = single (y);
    a = single (a);
    b = single (b);
  endif

  if (strcmpi (tail, "lower"))
    ys = y;
  elseif (strcmpi (tail, "upper"))
    ys = 1 - y;  # only for computation of initial points, no loss of accuracy
  else
    error ("betaincinv: invalid value for TAIL");
  endif

  ## Choose starting point for Newton's Method to guarantee convergence.
  ## If (a-1)*(b-1) > 0, F has a point of inflection at x = (a-1)/(a+b-2).
  ## In this case, it is convex on (0,x) and concave on (x,1) if a>1; otherwise
  ## it is the other way round.  If (a-1)*(b-1) <= 0, there is no point of
  ## inflection, and it is everywhere convex for a>1 and concave otherwise.
  ## We thus choose our starting x for the Newton iterations so that we stay
  ## within a region of constant sign of curvature and on the correct side of
  ## the eventual solution, guaranteeing convergence.  Curvatures above are to
  ## be understood under the condition tail=="lower".

  ## Initialize output array
  x = x_i = y_i = zeros (size (y), class (y));

  ## Have point of inflection
  idx = find ((a - 1) .* (b - 1) > 0);
  if (! isempty (idx))
    x_i(idx) = (a(idx) - 1) ./ (a(idx) + b(idx) - 2);
    y_i(idx) = betainc (x_i(idx), a(idx), b(idx));
  endif

  ## Converge outwards
  tmpidx = find (a(idx) > 1);
  if (! isempty (tmpidx))
    x(idx(tmpidx)) = x_i(idx(tmpidx));
  endif
  ## Converge inwards
  ## To the left of inflection point
  tmpidx = idx(find ((a(idx) <= 1) & (y_i(idx) >= ys(idx))));
  if (! isempty (tmpidx))
    x(tmpidx) = (ys(tmpidx) ./ y_i(tmpidx)).^(1 ./ a(tmpidx)) .* x_i(tmpidx);
  endif
  ## To the right of inflection point
  tmpidx = idx(find ((a(idx) <= 1) & (y_i(idx) < ys(idx))));
  if (! isempty (tmpidx))
    x(tmpidx) = 1 - ...
                ((1 - ys(tmpidx)) ./ (1 - y_i(tmpidx))).^(1 ./ b(tmpidx)) ...
                .* (1 - x_i(tmpidx));
  endif

  ## Have no point of inflection
  idx = find ((a - 1) .* (b - 1) <= 0);

  ## Negative curvature
  tmpidx = idx(find (a(idx) < 1));
  if (! isempty (tmpidx))
    x(tmpidx) = (ys(tmpidx) .* beta (a(tmpidx), b(tmpidx)) .* a(tmpidx)) ...
                .^ (1 ./ a(tmpidx));
  endif
  ## Positive curvature
  tmpidx = idx(find (a(idx) >= 1));
  if (! isempty (tmpidx))
    x(tmpidx) = 1 - ...
                ((1 - ys(tmpidx)) .* beta (a(tmpidx), b(tmpidx)) .* b(tmpidx)) ...
                .^ (1 ./ b(tmpidx));
  endif

  ## Cleanup memory before continuing
  clear ys x_i y_i idx tmpidx

  if (strcmpi (tail, "lower"))
    x(y == 0) = 0;
    x(y == 1) = 1;
    F = @(x, a, b, y) y - betainc (x, a, b);
    JF = @(x, a, b, Bln) -exp ((a-1) .* log (x) + (b-1) .* log1p (-x) - Bln);
  else
    x(y == 0) = 1;
    x(y == 1) = 0;
    F = @(x, a, b, y) y - betainc (x, a, b, "upper");
    JF = @(x, a, b, Bln) exp ((a-1) .* log (x) + (b-1) .* log1p (-x) - Bln);
  endif

  x = newton_method (F, JF, x, a, b, y);

  ## Restore original shape
  x = reshape (x, orig_sz);

endfunction

function x = newton_method (F, JF, x, a, b, y)

  Bln = betaln (a, b);
  ## Exclude special values that have been already computed.
  todo = find ((y != 0) & (y != 1));
  step = -F (x(todo), a(todo), b(todo), y(todo)) ./ ...
         JF (x(todo), a(todo), b(todo), Bln(todo));
  x_old = x(todo);
  x(todo) += step;
  dx = x(todo) - x_old;
  idx = (dx != 0);
  todo = todo(idx);
  dx_old = dx(idx);
  while (! isempty (todo))
    step = -F (x(todo), a(todo), b(todo), y(todo)) ./ ...
           JF (x(todo), a(todo), b(todo), Bln(todo));
    x_old = x(todo);
    x(todo) += step;
    dx = x(todo) - x_old;
    idx = (abs (dx) < abs (dx_old));  # Converging if dx is getting smaller
    todo = todo(idx);
    dx_old = dx(idx);
  endwhile

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
%!assert (class (betaincinv (0.5, 1, single (1))), "single")

## Extreme values for y, a, b that really test the algorithm
%!assert (betaincinv ([0, 1], 1, 3), [0, 1])
%!assert <*60528> (betaincinv (1e-6, 1, 3), 3.3333344444450617e-7, 2*eps)
%!assert <*60528> (betaincinv (1-1e-6, 3, 1), 0.9999996666665555, 2*eps)
%!assert (betainc (betaincinv (0.9, 1e-3, 1), 1e-3, 1), 0.9, 2*eps)
%!assert (betainc (betaincinv (.01, 1, 1e-3), 1, 1e-3), .01, 6*eps)
%!assert (betainc (betaincinv (0.5, 100, 1), 100, 1), 0.5, 8*eps)
%!assert (betainc (betaincinv (0.5, 1, 100), 1, 100), 0.5, 22*eps)
%!assert (betaincinv ([0, 1], 1, 3, "upper"), [1, 0])
%!assert <*60528> (betaincinv (1e-6, 1, 3, "upper"), 0.99, 2*eps)
%!assert <*60528> (betaincinv (1-1e-6, 3, 1,"upper"), .01, 250*eps)
%!assert (betainc (betaincinv (0.1, 1e-3, 1, "upper"), 1e-3, 1, "upper"),
%!        0.1, 2*eps)
%!assert (betainc (betaincinv (.99, 1, 1e-3, "upper"), 1, 1e-3, "upper"),
%!        .99, 6*eps)
%!assert (betainc (betaincinv (0.5, 100, 1, "upper"), 100, 1, "upper"),
%!        0.5, 8*eps)
%!assert (betainc (betaincinv (0.5, 1, 100, "upper"), 1, 100, "upper"),
%!        0.5, 22*eps)

## Test input validation
%!error <Invalid call> betaincinv ()
%!error <Invalid call> betaincinv (1)
%!error <Invalid call> betaincinv (1,2)
%!error <must be of common size or scalars>
%! betaincinv (ones (2,2), ones (1,2), 1);
%!error <must be .* floating point> betaincinv ('a', 1, 2)
%!error <must be .* floating point> betaincinv (0, int8 (1), 1)
%!error <must be .* floating point> betaincinv (0, 1, true)
%!error <must be real> betaincinv (0.5i, 1, 2)
%!error <must be real> betaincinv (0, 1i, 1)
%!error <must be real> betaincinv (0, 1, 1i)
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
