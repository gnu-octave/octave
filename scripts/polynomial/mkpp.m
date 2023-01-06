########################################################################
##
## Copyright (C) 2000-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{pp} =} mkpp (@var{breaks}, @var{coefs})
## @deftypefnx {} {@var{pp} =} mkpp (@var{breaks}, @var{coefs}, @var{d})
##
## Construct a piecewise polynomial (pp) structure from sample points
## @var{breaks} and coefficients @var{coefs}.
##
## @var{breaks} must be a vector of strictly increasing values.  The number of
## intervals is given by @code{@var{ni} = length (@var{breaks}) - 1}.
##
## When @var{m} is the polynomial order @var{coefs} must be of size:
## @w{@var{ni}-by-(@var{m} + 1)}.
##
## The i-th row of @var{coefs}, @code{@var{coefs}(@var{i},:)}, contains the
## coefficients for the polynomial over the @var{i}-th interval, ordered from
## highest (@var{m}) to lowest (@var{0}) degree.
##
## @var{coefs} may also be a multi-dimensional array, specifying a
## vector-valued or array-valued polynomial.  In that case the polynomial
## order @var{m} is defined by the length of the last dimension of @var{coefs}.
## The size of first dimension(s) are given by the scalar or vector @var{d}.
## If @var{d} is not given it is set to @code{1}.  In this case
## @code{@var{p}(@var{r}, @var{i}, :)} contains the coefficients for the
## @var{r}-th polynomial defined on interval @var{i}.  In any case @var{coefs}
## is reshaped to a 2-D matrix of size @code{[@var{ni}*prod(@var{d}) @var{m}]}.
##
## Programming Note: @code{ppval} evaluates polynomials at
## @code{@var{xi} - @var{breaks}(i)}, i.e., it subtracts the lower endpoint of
## the current interval from @var{xi}.  This must be taken into account when
## creating piecewise polynomials objects with @code{mkpp}.
## @seealso{unmkpp, ppval, spline, pchip, ppder, ppint, ppjumps}
## @end deftypefn

function pp = mkpp (breaks, coefs, d)

  if (nargin < 2)
    print_usage ();
  endif

  ## Check BREAKS
  if (! isvector (breaks))
    error ("mkpp: BREAKS must be a vector");
  elseif (length (breaks) < 2)
    error ("mkpp: BREAKS must have at least one interval");
  endif

  len = length (breaks) - 1;

  pp = struct ("form", "pp",
               "breaks", breaks(:).',
               "coefs", [],
               "pieces", len,
               "order", prod (size (coefs)) / len,
               "dim", 1);

  if (nargin == 3)
    pp.dim = d;
    pp.order /= prod (d);
  endif

  dim_vec = [pp.pieces * prod(pp.dim), pp.order];
  pp.coefs = reshape (coefs, dim_vec);

endfunction


%!demo # linear interpolation
%! x = linspace (0, pi, 5)';
%! t = [sin(x), cos(x)];
%! m = diff (t) ./ (x(2)-x(1));
%! b = t(1:4,:);
%! pp = mkpp (x, [m(:),b(:)]);
%! xi = linspace (0, pi, 50);
%! plot (x, t, "x", xi, ppval (pp,xi));bb4af245dff7
%! legend ("control", "interp");

%!demo # piecewise polynomial shape
%! breaks = [0 1 2 3];
%! dim = 2;
%! coefs = zeros (dim, length (breaks) - 1, 4);
%! # 1st edge of the shape (x, x^2)
%! coefs(1,1,:) = [0 0 1 0];
%! coefs(2,1,:) = [0 1 0 0];
%! # 2nd edge of the shape (-3x, 1)
%! coefs(1,2,:) = [0 0 -3 1];
%! coefs(2,2,:) = [0 0 0 1];
%! # 3rd edge of the shape (2x - 2, -4(x -1/2)^3 + 1/2)
%! coefs(1,3,:) = [0 0 2 -2];
%! coefs(2,3,:) = [-4 6 -3 1];
%! pp = mkpp (breaks, coefs, dim);
%! t = linspace (0, 3, 100).';
%! xy = ppval (pp, t).';
%! patch (xy(:,1), xy(:,2), 'r');

%!shared b,c,pp
%! b = 1:3; c = 1:24; pp = mkpp (b,c);
%!assert (pp.pieces, 2)
%!assert (pp.order, 12)
%!assert (pp.dim, 1)
%!assert (size (pp.coefs), [2,12])
%! pp = mkpp (b,c,2);
%!assert (pp.pieces, 2)
%!assert (pp.order, 6)
%!assert (pp.dim, 2)
%!assert (size (pp.coefs), [4,6])
%! pp = mkpp (b,c,3);
%!assert (pp.pieces, 2)
%!assert (pp.order, 4)
%!assert (pp.dim, 3)
%!assert (size (pp.coefs), [6,4])
%! pp = mkpp (b,c,[2,3]);
%!assert (pp.pieces, 2)
%!assert (pp.order, 2)
%!assert (pp.dim, [2,3])
%!assert (size (pp.coefs), [12,2])
