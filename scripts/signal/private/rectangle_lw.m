########################################################################
##
## Copyright (C) 1995-2022 The Octave Project Developers
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
## @deftypefn {} {} rectangle_lw (@var{n}, @var{b})
## Rectangular lag window.  Subfunction used for spectral density
## estimation.
## @end deftypefn

function retval = rectangle_lw (n, b)

  retval = zeros (n, 1);
  t = floor (1 / b);

  retval(1:t, 1) = ones (t, 1);

endfunction
