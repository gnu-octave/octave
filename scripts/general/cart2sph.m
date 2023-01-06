########################################################################
##
## Copyright (C) 2000-2023 The Octave Project Developers
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
## @deftypefn  {} {[@var{theta}, @var{phi}, @var{r}] =} cart2sph (@var{x}, @var{y}, @var{z})
## @deftypefnx {} {[@var{theta}, @var{phi}, @var{r}] =} cart2sph (@var{C})
## Transform Cartesian coordinates to spherical coordinates.
##
## The inputs @var{x}, @var{y}, and @var{z} must be the same shape, or scalar.
## If called with a single matrix argument then each row of @var{C} must
## represent a Cartesian coordinate triplet (@var{x}, @var{y}, @var{z}).
##
## The outputs @var{theta}, @var{phi}, @var{r} match the shape of the inputs.
## For a matrix input @var{C} the outputs will be column vectors with rows
## corresponding to the rows of the input matrix.
##
## @var{theta} describes the azimuth angle relative to the positive x-axis
## measured in the xy-plane.
##
## @var{phi} is the elevation angle measured relative to the xy-plane.
##
## @var{r} is the distance to the origin @w{(0, 0, 0)}.
##
## The coordinate transformation is computed using:
##
## @tex
## $$ \theta = \arctan \left ({y \over x} \right ) $$
## $$ \phi = \arctan \left ( {z \over {\sqrt{x^2+y^2}}} \right ) $$
## $$ r = \sqrt{x^2 + y^2 + z^2} $$
## @end tex
## @ifnottex
##
## @example
## @group
## @var{theta} = arctan (@var{y} / @var{x})
## @var{phi} = arctan (@var{z} / sqrt (@var{x}^2 + @var{y}^2))
## @var{r} = sqrt (@var{x}^2 + @var{y}^2 + @var{z}^2)
## @end group
## @end example
##
## @end ifnottex
##
## @c FIXME: Remove this note in Octave 9.1 (two releases after 7.1).
## Note: For @sc{matlab} compatibility, this function no longer returns a full
## coordinate matrix when called with a single return argument.
## @seealso{sph2cart, cart2pol, pol2cart}
## @end deftypefn

function [theta, phi, r] = cart2sph (x, y, z)

  if (nargin != 1 && nargin != 3)
    print_usage ();
  endif

  if (nargin == 1)
    if (! (isnumeric (x) && ismatrix (x)))
      error ("cart2sph: matrix input C must be a 2-D numeric array");
    elseif (columns (x) != 3 && numel (x) != 3)
      error ("cart2sph: matrix input C must be a 3-element vector or 3-column array");
    endif

    if (numel (x) == 3)
      z = x(3);
      y = x(2);
      x = x(1);
    else
      z = x(:,3);
      y = x(:,2);
      x = x(:,1);
    endif

  else
    if (! (isnumeric (x) && isnumeric (y) && isnumeric (z)))
      error ("cart2sph: X, Y, Z must be numeric arrays or scalars");
    endif
    [err, x, y, z] = common_size (x, y, z);
    if (err)
      error ("cart2sph: X, Y, Z must be the same size or scalars");
    endif
  endif

  theta = atan2 (y, x);
  phi = atan2 (z, sqrt (x .^ 2 + y .^ 2));
  r = sqrt (x .^ 2 + y .^ 2 + z .^ 2);

endfunction


%!test
%! x = [0, 1, 2];
%! y = [0, 1, 2];
%! z = [0, 1, 2];
%! [t, p, r] = cart2sph (x, y, z);
%! assert (t, [0, pi/4, pi/4], eps);
%! assert (p, [0, 1, 1]*atan (sqrt (0.5)), eps);
%! assert (r, [0, 1, 2]*sqrt (3), eps);

%!test
%! x = [0; 1; 2];
%! y = [0; 1; 2];
%! z = [0; 1; 2];
%! [t, p, r] = cart2sph (x, y, z);
%! assert (t, [0; pi/4; pi/4], eps);
%! assert (p, [0; 1; 1] * atan (sqrt (0.5)), eps);
%! assert (r, [0; 1; 2] * sqrt (3), eps);

%!test
%! x = 0;
%! y = [0, 1, 2];
%! z = [0, 1, 2];
%! [t, p, r] = cart2sph (x, y, z);
%! assert (t, [0, 1, 1] * pi/2, eps);
%! assert (p, [0, 1, 1] * pi/4, eps);
%! assert (r, [0, 1, 2] * sqrt (2), eps);

%!test
%! x = [0, 1, 2];
%! y = 0;
%! z = [0, 1, 2];
%! [t, p, r] = cart2sph (x, y, z);
%! assert (t, [0, 0, 0]);
%! assert (p, [0, 1, 1] * pi/4, eps);
%! assert (r, [0, 1, 2] * sqrt (2), eps);

%!test
%! x = [0, 1, 2];
%! y = [0, 1, 2];
%! z = 0;
%! [t, p, r] = cart2sph (x, y, z);
%! assert (t, [0, 1, 1] * pi/4, eps);
%! assert (p, [0, 0, 0]);
%! assert (r, [0, 1, 2] * sqrt (2), eps);

%!test
%! x = 0;
%! y = 0;
%! z = [0, 1, 2];
%! [t, p, r] = cart2sph (x, y, z);
%! assert (t, [0, 0, 0]);
%! assert (p, [0, 1, 1] * pi/2, eps);
%! assert (r, [0, 1, 2]);

%!test
%! C = [0, 0, 0; 1, 0, 1; 2, 0, 2];
%! [t, p, r] = cart2sph (C);
%! assert (t, [0; 0; 0]);
%! assert (p, [0; 1; 1] * pi/4, eps);
%! assert (r, [0; 1; 2] * sqrt (2), eps);

%!test
%! C = [0, 0, 0; 1, 0, 1; 2, 0, 2; 1, 0, 1];
%! [t, p, r] = cart2sph (C);
%! assert (t, [0; 0; 0; 0]);
%! assert (p, [0; 1; 1; 1] * pi/4, eps);
%! assert (r, [0; 1; 2; 1] * sqrt (2), eps);

%!test
%! [x, y, z] = meshgrid ([0, 1], [0, 1], [0, 1]);
%! [t, p, r] = cart2sph (x, y, z);
%! T(:, :, 1) = [0, 0; pi/2, pi/4];
%! T(:, :, 2) = T(:, :, 1);
%! P(:, :, 1) = zeros (2, 2);
%! P(:, :, 2) = [pi/2, pi/4; pi/4, acos(sqrt(2/3))];
%! R = sqrt (x .^ 2 + y .^ 2 + z .^ 2);
%! assert (t, T, eps);
%! assert (p, P, eps);
%! assert (r, R, eps);

## Test input validation
%!error <Invalid call> cart2sph ()
%!error <Invalid call> cart2sph (1,2)
%!error <matrix input C must be a 2-D numeric array> cart2sph ({1,2,3})
%!error <matrix input C must be a 2-D numeric array> cart2sph (ones (3,3,2))
%!error <matrix input C must be a 3-element> cart2sph ([1,2,3,4])
%!error <matrix input C must be a 3-element> cart2sph ([1,2,3,4; 1,2,3,4; 1,2,3,4])
%!error <must be numeric arrays or scalars> cart2sph ({1,2,3}, [1,2,3], [1,2,3])
%!error <must be numeric arrays or scalars> cart2sph ([1,2,3], {1,2,3}, [1,2,3])
%!error <must be numeric arrays or scalars> cart2sph ([1,2,3], [1,2,3], {1,2,3})
%!error <must be the same size or scalars> cart2sph ([1,2,3], [1,2,3], [1,2,3]')
%!error <must be the same size or scalars> cart2sph (ones (3,3,3), 1, ones (3,2,3))
%!error <must be the same size or scalars> cart2sph (ones (3,3,3), ones (3,2,3), 1)
