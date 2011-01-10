## Copyright (C) 1995, 1996, 1997, 2005, 2006, 2007, 2008 Kurt Hornik
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

## -*- texinfo -*-
## @deftypefn {Function File} {} gaminv (@var{x}, @var{a}, @var{b})
## For each component of @var{x}, compute the quantile (the inverse of
## the CDF) at @var{x} of the Gamma distribution with parameters @var{a}
## and @var{b}.
## @seealso{gamma, gammaln, gammainc, gampdf, gamcdf, gamrnd}
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Quantile function of the Gamma distribution

function inv = gaminv (x, a, b)

  if (nargin != 3)
    print_usage ();
  endif

  if (!isscalar (a) || !isscalar(b))
    [retval, x, a, b] = common_size (x, a, b);
    if (retval > 0)
      error ("gaminv: X, A and B must be of common size or scalars");
    endif
  endif

  sz = size (x);
  inv = zeros (sz);

  k = find ((x < 0) | (x > 1) | isnan (x) | !(a > 0) | !(b > 0));
  if (any (k))
    inv (k) = NaN;
  endif

  k = find ((x == 1) & (a > 0) & (b > 0));
  if (any (k))
    inv (k) = Inf;
  endif

  k = find ((x > 0) & (x < 1) & (a > 0) & (b > 0));
  if (any (k))
    if (!isscalar(a) || !isscalar(b))
      a = a (k);
      b = b (k);
      y = a .* b;
    else
      y = a * b * ones (size (k));
    endif
    x = x (k);

    if (isa (x, "single"))
      myeps = eps ("single");
    else
      myeps = eps;
    endif

    l = find (x < myeps);
    if (any (l))
      y(l) = sqrt (myeps) * ones (length (l), 1);
    endif

    y_old = y;
    for i = 1 : 100
      h     = (gamcdf (y_old, a, b) - x) ./ gampdf (y_old, a, b);
      y_new = y_old - h;
      ind   = find (y_new <= myeps);
      if (any (ind))
        y_new (ind) = y_old (ind) / 10;
        h = y_old - y_new;
      endif
      if (max (abs (h)) < sqrt (myeps))
        break;
      endif
      y_old = y_new;
    endfor

    inv (k) = y_new;
  endif

endfunction
