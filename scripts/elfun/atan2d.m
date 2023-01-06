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
## @deftypefn {} {@var{d} =} atan2d (@var{y}, @var{x})
## Compute atan (@var{y} / @var{x}) in degrees for corresponding elements
## from @var{y} and @var{x}.
## @seealso{tand, atan2}
## @end deftypefn

function d = atan2d (y, x)

  if (nargin != 2)
    print_usage ();
  endif

  d = 180 ./ pi .* atan2 (y, x);

endfunction


%!assert (atan2d (-1:.1:1, 1:-.1:-1),
%!        180/pi * atan2 (-1:.1:1, 1:-.1:-1), -10*eps)

%!error <Invalid call> atan2d ()
%!error <Invalid call> atan2d (1)
