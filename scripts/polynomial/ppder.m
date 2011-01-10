## Copyright (C) 2008, 2009  VZLU Prague, a.s., Czech Republic
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
## @deftypefn {Function File} {@var{ppd} =} ppder (@var{pp})
## Computes the piecewise derivative of a piecewise polynomial struct @var{pp}.
## @seealso{mkpp,ppval}
## @end deftypefn

function ppd = ppder (pp)
  if (nargin != 1)
    print_usage ();
  endif
  if (! isstruct (pp))
    error ("ppder: PP must be a structure");
  endif

  [x, p, n, k, d] = unmkpp (pp);
  p = reshape (p, [], k);
  if (k <= 1)
    pd = zeros (rows (p), 1);
    k = 1;
  else
    k -= 1;
    pd = p(:,1:k) * diag (k:-1:1);
  endif
  ppd = mkpp (x, pd, d);
endfunction

