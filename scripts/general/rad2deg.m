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
## @deftypefn {} {@var{deg} =} rad2deg (@var{rad})
##
## Convert radians to degrees.
##
## The input @var{rad} must be a scalar, vector, or N-dimensional array of
## double or single floating point values.  @var{rad} may be complex in which
## case the real and imaginary components are converted separately.
##
## The output @var{deg} is the same size and shape as @var{rad} with radians
## converted to degrees using the conversion constant @code{180/pi}.
##
## Example:
##
## @example
## @group
## rad2deg ([0, pi/2, pi, 3/2*pi, 2*pi])
##   @result{}  0    90   180   270   360
## @end group
## @end example
## @seealso{deg2rad}
## @end deftypefn

function deg = rad2deg (rad)

  if (nargin < 1)
    print_usage ();
  endif

  if (! isfloat (rad))
    error ("rad2deg: RAD must be a floating point class (double or single)");
  endif

  deg = rad * (180/pi);

endfunction


%!assert (rad2deg (0), 0)
%!assert (rad2deg (pi/2), 90)
%!assert (rad2deg (pi), 180)
%!assert (rad2deg (pi*[0, 1/2, 1, 3/2, 2]), [0, 90, 180, 270, 360])

## Test input validation
%!error <Invalid call> rad2deg ()
%!error <RAD must be a floating point class> rad2deg (uint8 (1))
%!error <RAD must be a floating point class> rad2deg ("A")
