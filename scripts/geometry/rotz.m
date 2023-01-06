########################################################################
##
## Copyright (C) 2017-2023 The Octave Project Developers
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
## @deftypefn {} {@var{T} =} rotz (@var{angle})
##
## @code{rotz} returns the 3x3 transformation matrix corresponding to an active
## rotation of a vector about the z-axis by the specified @var{angle}, given in
## degrees, where a positive angle corresponds to a counterclockwise
## rotation when viewing the x-y plane from the positive z side.
##
## The form of the transformation matrix is:
## @tex
## $$
## T = \left[\matrix{ \cos(angle) & -\sin(angle) & 0 \cr
##                    \sin(angle) & \cos(angle) & 0 \cr
##                    0 & 0 & 1}\right].
## $$
## @end tex
## @ifnottex
##
## @example
## @group
##      | cos(@var{angle}) -sin(@var{angle}) 0 |
##  T = | sin(@var{angle})  cos(@var{angle}) 0 |
##      |     0           0      1 |
## @end group
## @end example
## @end ifnottex
##
## This rotation matrix is intended to be used as a left-multiplying matrix
## when acting on a column vector, using the notation
## @code{@var{v} = @var{T}*@var{u}}.
## For example, a vector, @var{u}, pointing along the positive x-axis, rotated
## 90-degrees about the z-axis, will result in a vector pointing along the
## positive y-axis:
##
## @example
## @group
##   >> u = [1 0 0]'
##    u =
##       1
##       0
##       0
##
##    >> T = rotz (90)
##    T =
##       0.00000  -1.00000   0.00000
##       1.00000   0.00000   0.00000
##       0.00000   0.00000   1.00000
##
##    >> v = T*u
##    v =
##       0.00000
##       1.00000
##       0.00000
## @end group
## @end example
##
## @seealso{rotx, roty}
## @end deftypefn

function T = rotz (angle)

  if (nargin < 1 || ! isscalar (angle))
    print_usage ();
  endif

  angle = angle * pi / 180;

  s = sin (angle);
  c = cos (angle);

  T = [c -s 0; s c 0; 0 0 1];

endfunction


## Function output tests
%!assert (rotz (0), [1 0 0; 0 1 0; 0 0 1])
%!assert (rotz (45), [(sqrt(2)/2).*[1 -1; 1 1] ,[0; 0]; 0, 0, 1], 1e-12)
%!assert (rotz (90), [0 -1 0; 1 0 0; 0 0 1], 1e-12)
%!assert (rotz (180), [-1 0 0; 0 -1 0; 0 0 1], 1e-12)

## Test input validation
%!error <Invalid call> rotz ()
%!error <Invalid call> rotz ([1 2 3])
