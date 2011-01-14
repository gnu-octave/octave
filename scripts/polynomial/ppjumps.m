## Copyright (C) 2008-2011 VZLU Prague, a.s., Czech Republic
## 
## This file is part of Octave.
## 
## Octave is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version.
## 
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
## 
## You should have received a copy of the GNU General Public License
## along with this software; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn {Function File} {@var{jumps} =} ppjumps (@var{pp})
## Evaluates the boundary jumps of a piecewise polynomial.
## If there are n intervals, and the dimensionality of pp is d,
## the resulting array has dimensions @code{[d, n-1]}.
## @end deftypefn

function jumps = ppjumps (pp)
  if (nargin != 1)
    print_usage ();
  endif
  if (! isstruct (pp))
    error ("ppjumps: PP must be a structure");
  endif

  ## Extract info.
  x = pp.x;
  P = pp.P;
  d = pp.d;
  [nd, n, k] = size (P);

  ## Offsets.
  dx = diff (x(1:n)).';
  dx = dx(ones (1, nd), :); # spread (do nothing in 1D)

  ## Use Horner scheme to get limits from the left.
  llim = P(:,1:n-1,1);
  for i = 2:k;
    llim .*= dx;
    llim += P(:,1:n-1,i);
  endfor

  rlim = P(:,2:n,k); # limits from the right
  jumps = reshape (rlim - llim, [d, n-1]);

endfunction


