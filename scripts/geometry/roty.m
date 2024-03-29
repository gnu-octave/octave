########################################################################
##
## Copyright (C) 2019-2024 The Octave Project Developers
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
## @deftypefn {} {@var{T} =} roty (@var{angle})
##
## @code{roty} returns the 3x3 transformation matrix corresponding to an active
## rotation of a vector about the y-axis by the specified @var{angle}, given in
## degrees, where a positive angle corresponds to a counterclockwise
## rotation when viewing the z-x plane from the positive y side.
##
## The form of the transformation matrix is:
## @tex
## $$
## T = \left[\matrix{ \cos(angle) & 0 & \sin(angle) \cr
##                    0 & 1 & 0 \cr
##                    -\sin(angle) & 0 & \cos(angle)}\right].
## $$
## @end tex
## @ifnottex
##
## @example
## @group
##      |  cos(@var{angle})  0  sin(@var{angle}) |
##  T = |      0       1      0      |
##      | -sin(@var{angle})  0  cos(@var{angle}) |
## @end group
## @end example
## @end ifnottex
##
## This rotation matrix is intended to be used as a left-multiplying matrix
## when acting on a column vector, using the notation
## @code{@var{v} = @var{T}*@var{u}}.
## For example, a vector, @var{u}, pointing along the positive z-axis, rotated
## 90-degrees about the y-axis, will result in a vector pointing along the
## positive x-axis:
##
## @example
## @group
##   >> u = [0 0 1]'
##    u =
##       0
##       0
##       1
##
##    >> T = roty (90)
##    T =
##       0.00000   0.00000   1.00000
##       0.00000   1.00000   0.00000
##      -1.00000   0.00000   0.00000
##
##    >> v = T*u
##    v =
##       1.00000
##       0.00000
##       0.00000
## @end group
## @end example
##
## @seealso{rotx, rotz}
## @end deftypefn

function T = roty (angle)

  if (nargin < 1 || ! isscalar (angle))
    print_usage ();
  endif

  angle *= pi / 180;

  s = sin (angle);
  c = cos (angle);

  T = [c 0 s; 0 1 0; -s 0 c];

endfunction


## Function output tests
%!assert (roty (0), [1 0 0; 0 1 0; 0 0 1])
%!assert (roty (45), [sqrt(2) 0 sqrt(2); 0 2 0; -sqrt(2) 0 sqrt(2)]./2, 1e-12)
%!assert (roty (90), [0 0 1; 0 1 0; -1 0 0], 1e-12)
%!assert (roty (180), [-1 0 0; 0 1 0; 0 0 -1], 1e-12)

## Test input validation
%!error <Invalid call> roty ()
%!error <Invalid call> roty ([1 2 3])
