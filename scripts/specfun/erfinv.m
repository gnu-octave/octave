## Copyright (C) 1995, 1996  Kurt Hornik
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, 59 Temple Place - Suite 330, Boston, MA
## 02111-1307, USA.

## -*- texinfo -*-
## @deftypefn {Mapping Function} {} erfinv (@var{z})
## Computes the inverse of the error function.
## @end deftypefn
##
## @seealso{erf and erfc}

## Author: KH <Kurt.Hornik@ci.tuwien.ac.at>
## Created: 27 September 1994
## Adapted-By: jwe

function [y, iterations] = erfinv (x)

  if (nargin != 1)
    usage ("erfinv (x)");
  endif

  maxit = 100;
  tol = eps;

  iterations = 0;

  sz = size (x);
  nel = numel (x);

  x = reshape (x, nel, 1);
  y = zeros (nel, 1);

  i = find ((x < -1) | (x > 1) | isnan(x));
  if any (i)
    y(i) = NaN * ones (length (i), 1);
  endif

  t = find (x == -1);
  y (t) = (-Inf) * ones (size (t));

  t = find (x == 1);
  y (t) = Inf * ones (size (t));

  i = find ((x > -1) & (x < 1));
  if any (i)
    s = sqrt (pi) / 2;
    z_old = ones (length (i), 1);
    z_new = sqrt (-log (1 - abs (x(i)))) .* sign (x(i));
    while (any (abs (erf (z_new) - x(i)) > tol * abs (x(i))))
      z_old = z_new;
      z_new = z_old - (erf (z_old) - x(i)) .* exp (z_old.^2) * s;
      if (++iterations > maxit)
        warning ("erfinv: iteration limit exceeded");
        break;
      endif
    endwhile
    y(i) = z_new;
  endif

  y = reshape (y, sz);

endfunction
