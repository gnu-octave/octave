########################################################################
##
## Copyright (C) 2013-2023 The Octave Project Developers
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
## @deftypefn {} {retval =} AbsRel_norm (@var{x}, @var{x_old}, @var{AbsTol}, @var{RelTol}, @var{normcontrol}, @var{y})
## Undocumented internal function.
## @end deftypefn

function retval = AbsRel_norm (x, x_old, AbsTol, RelTol, normcontrol, y = zeros (size (x)))

  if (normcontrol)
    sc = max (AbsTol(:), RelTol * max (sqrt (sumsq (x)), sqrt (sumsq (x_old))));
    retval = sqrt (sumsq ((x - y))) / sc;
  else
    sc = max (AbsTol(:), RelTol .* max (abs (x), abs (x_old)));
    retval = max (abs (x - y) ./ sc);
  endif

endfunction
