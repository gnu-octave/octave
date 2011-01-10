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
## @deftypefn {Function File} {@var{ppi} =} ppint (@var{pp}, @var{c})
## Computes the antiderivative of a piecewise polynomial struct @var{pp}.
## @var{c}, if given, is the constant of integration.
## @seealso{mkpp,ppval}
## @end deftypefn

function ppi = ppint (pp, c)
  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif
  if (! isstruct (pp))
    error ("ppint: PP must be a structure");
  endif

  [x, p, n, k, d] = unmkpp (pp);
  p = reshape (p, [], k);
  
  ## Get piecewise antiderivatives
  pi = p / diag (k:-1:1);
  k += 1;
  if (nargin == 1)
    pi(:,k) = 0;
  else
    pi(:,k) = repmat (c(:), n, 1);
  endif

  ppi = mkpp (x, pi, d);

  ## Adjust constants so the the result is continuous.

  jumps = reshape (ppjumps (ppi), prod (d), n-1);
  ppi.P(:,2:n,k) -= cumsum (jumps, 2);

endfunction

