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
## @deftypefn {} {@var{retval} =} triangle_sw (@var{n}, @var{b})
## Triangular spectral window.
##
## Subfunction used for spectral density estimation.
## @seealso{spectral_xdf}
## @end deftypefn

function retval = triangle_sw (n, b)

  retval = zeros (n,1);
  retval(1) = 1 / b;

  l = (2:n)' - 1;
  l = 2 * pi * l / n;

  retval(2:n) = b * (sin (l / (2*b)) ./ sin (l / 2)).^2;

endfunction
