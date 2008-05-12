## Copyright (C) 1995, 1996, 1997, 1999, 2000, 2002, 2004, 2005, 2006,
##               2007 Kurt Hornik
## Copyright (C) 2008 Alois Schloegl
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
## @deftypefn {Mapping Function} {} erfinv (@var{z})
## Computes the inverse of the error function.
## @seealso{erf, erfc}
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Created: 27 September 1994
## Adapted-By: jwe

function [y, iterations] = erfinv (x)

  if (nargin != 1)
    print_usage ();
  endif

  maxit = 100;
  if (isa (x, "single"))
    tol = eps ("single");
  else
    tol = eps;
  endif

  iterations = 0;

  sz = size (x);
  nel = numel (x);

  x = reshape (x, nel, 1);
  y = zeros (nel, 1);

  ## x < -1 or x > 1 ==> NaN
  y(abs (x) >= 1) = NaN;
  y(x == -1) = -Inf;
  y(x == +1) = +Inf;

  i = find ((x > -1) & (x < 1));
  if (any (i))
    s = sqrt (pi) / 2;
    z = sqrt (-log (1 - abs (x(i)))) .* sign (x(i));
    while (any (abs (erf (z) - x(i)) > tol * abs (x(i))))
      z = z - (erf (z) - x(i)) .* exp (z.^2) * s;
      if (++iterations > maxit)
        warning ("erfinv: iteration limit exceeded");
        break;
      endif
    endwhile
    y(i) = z;
  endif

  y = reshape (y, sz);

endfunction
