########################################################################
##
## Copyright (C) 2006-2020 The Octave Project Developers
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
## @deftypefn {} {} sind (@var{x})
## Compute the sine for each element of @var{x} in degrees.
##
## The function is more exact than @code{sin} for multiples of 180 degrees and
## returns zero rather than a small value on the order of eps.
## @seealso{asind, sin}
## @end deftypefn

function y = sind (x)

  if (nargin < 1)
    print_usage ();
  endif

  ## Wrap multiples so that new domain is [-180, 180)
  x = mod (x-180, 360) - 180;

  ## Integer multiples of pi must be exactly zero
  x(x == -180) = 0;

  y = sin (x / 180 * pi);

endfunction


%!assert (sind (10:20:360), sin ([10:20:360] * pi/180), 5*eps)
%!assert (sind ([-360, -180, 0, 180, 360]) == 0)
%!assert (sind ([-270, -90, 90, 270]), [1, -1, 1, -1])

%!error <Invalid call> sind ()
