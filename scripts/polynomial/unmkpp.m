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
## @deftypefn {} {[@var{x}, @var{p}, @var{n}, @var{k}, @var{d}] =} unmkpp (@var{pp})
##
## Extract the components of a piecewise polynomial structure @var{pp}.
##
## This function is the inverse of @code{mkpp}: it extracts the inputs to
## @code{mkpp} needed to create the piecewise polynomial structure @var{pp}.
## The code below makes this relation explicit:
##
## @example
## @group
## [breaks, coefs, numinter, order, dim] = unmkpp (pp);
## pp2  = mkpp (breaks, coefs, dim);
## @end group
## @end example
##
## The piecewise polynomial structure @code{pp2} obtained in this way, is
## identical to the original @code{pp}.  The same can be obtained by directly
## accessing the fields of the structure @code{pp}.
##
## The components are:
##
## @table @asis
## @item @var{x}
## Sample points or breaks.
##
## @item @var{p}
## Polynomial coefficients for points in sample interval.
## @code{@var{p}(@var{i}, :)} contains the coefficients for the polynomial
## over interval @var{i} ordered from highest to lowest degree.
## If @code{@var{d} > 1}, then @var{p} is a matrix of size
## @code{[@var{n}*prod(@var{d}) @var{m}]}, where the
## @code{@var{i} + (1:@var{d})} rows are the coefficients of all the @var{d}
## polynomials in the interval @var{i}.
##
## @item @var{n}
## Number of polynomial pieces or intervals,
## @code{@var{n} = length (@var{x}) - 1}.
##
## @item @var{k}
## Order of the polynomial plus 1.
##
## @item @var{d}
## Number of polynomials defined for each interval.
## @end table
##
## @seealso{mkpp, ppval, spline, pchip}
## @end deftypefn

function [x, P, n, k, d] = unmkpp (pp)

  if (nargin < 1)
    print_usage ();
  endif
  if (! (isstruct (pp) && isfield (pp, "form") && strcmp (pp.form, "pp")))
    error ("unmkpp: PP must be a piecewise polynomial structure");
  endif
  x = pp.breaks;
  P = pp.coefs;
  n = pp.pieces;
  k = pp.order;
  d = pp.dim;

endfunction


%!test
%! b = 1:3;
%! c = 1:24;
%! pp = mkpp (b,c);
%! [x, P, n, k, d] = unmkpp (pp);
%! assert (x, b);
%! assert (P, reshape (c, [2 12]));
%! assert (n, 2);
%! assert (k, 12);
%! assert (d, 1);

## Test input validation
%!error <Invalid call> unmkpp ()
%!error <piecewise polynomial structure> unmkpp (1)
%!error <piecewise polynomial structure> unmkpp (struct ("field1", "pp"))
%!error <piecewise polynomial structure> unmkpp (struct ("form", "not_a_pp"))
