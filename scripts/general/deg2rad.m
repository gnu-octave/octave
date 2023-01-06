########################################################################
##
## Copyright (C) 2016-2023 The Octave Project Developers
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
## @deftypefn {} {@var{rad} =} deg2rad (@var{deg})
##
## Convert degrees to radians.
##
## The input @var{deg} must be a scalar, vector, or N-dimensional array of
## double or single floating point values.  @var{deg} may be complex in which
## case the real and imaginary components are converted separately.
##
## The output @var{rad} is the same size and shape as @var{deg} with degrees
## converted to radians using the conversion constant @code{pi/180}.
##
## Example:
##
## @example
## @group
## deg2rad ([0, 90, 180, 270, 360])
##   @result{}  0.00000   1.57080   3.14159   4.71239   6.28319
## @end group
## @end example
## @seealso{rad2deg}
## @end deftypefn

function rad = deg2rad (deg)

  if (nargin < 1)
    print_usage ();
  endif

  if (! isfloat (deg))
    error ("deg2rad: DEG must be a floating point class (double or single)");
  endif

  rad = deg * (pi / 180);

endfunction


%!assert (deg2rad (0), 0)
%!assert (deg2rad (90), pi/2)
%!assert (deg2rad (180), pi)
%!assert (deg2rad ([0, 90, 180, 270, 360]), pi*[0, 1/2, 1, 3/2, 2])

## Test input validation
%!error <Invalid call> deg2rad ()
%!error <DEG must be a floating point class> deg2rad (uint8 (1))
%!error <DEG must be a floating point class> deg2rad ("A")
