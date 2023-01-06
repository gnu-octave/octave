########################################################################
##
## Copyright (C) 1995-2023 The Octave Project Developers
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
## @deftypefn {} {@var{savg} =} spencer (@var{x})
## Return @nospell{Spencer's} 15-point moving average of each column of
## @var{x}.
## @end deftypefn

function savg = spencer (x)

  if (nargin != 1)
    print_usage ();
  endif

  [xr, xc] = size (x);

  n = xr;
  c = xc;

  if (isvector (x))
   n = length (x);
   c = 1;
   x = reshape (x, n, 1);
  endif

  w = [-3, -6, -5, 3, 21, 46, 67, 74, 67, 46, 21, 3, -5, -6, -3] / 320;

  savg = fftfilt (w, x);
  savg = [zeros(7,c); savg(15:n,:); zeros(7,c);];

  savg = reshape (savg, xr, xc);

endfunction
