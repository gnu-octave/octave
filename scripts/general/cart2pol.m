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
## @deftypefn  {} {[@var{theta}, @var{r}] =} cart2pol (@var{x}, @var{y})
## @deftypefnx {} {[@var{theta}, @var{r}, @var{z}] =} cart2pol (@var{x}, @var{y}, @var{z})
## @deftypefnx {} {[@var{theta}, @var{r}] =} cart2pol (@var{C})
## @deftypefnx {} {[@var{theta}, @var{r}, @var{z}] =} cart2pol (@var{C})
##
## Transform Cartesian coordinates to polar or cylindrical coordinates.
##
## The inputs @var{x}, @var{y} (, and @var{z}) must be the same shape, or
## scalar.  If called with a single matrix argument then each row of @var{C}
## represents the Cartesian coordinate pair (@var{x}, @var{y}) or triplet
## (@var{x}, @var{y}, @var{z}).
##
## The outputs @var{theta}, @var{r} (, and @var{z}) match the shape of the
## inputs.  For a matrix input @var{C} the outputs will be column vectors with
## rows corresponding to the rows of the input matrix.
##
## @var{theta} describes the angle relative to the positive x-axis measured in
## the xy-plane.
##
## @var{r} is the distance to the z-axis @w{(0, 0, z)}.
##
## @var{z}, if present, is unchanged by the transformation.
##
## The coordinate transformation is computed using:
##
## @tex
## $$ \theta = \arctan \left ( {y \over x} \right ) $$
## $$ r = \sqrt{x^2 + y^2} $$
## $$ z = z $$
## @end tex
## @ifnottex
##
## @example
## @group
## @var{theta} = arctan (@var{y} / @var{x})
## @var{r} = sqrt (@var{x}^2 + @var{y}^2)
## @var{z} = @var{z}
## @end group
## @end example
##
## @end ifnottex
##
## @c FIXME: Remove this note in Octave 9.1 (two releases after 7.1).
## Note: For @sc{matlab} compatibility, this function no longer returns a full
## coordinate matrix when called with a single return argument.
## @seealso{pol2cart, cart2sph, sph2cart}
## @end deftypefn

function [theta, r, z] = cart2pol (x, y, z = [])

  if (nargin < 1)
    print_usage ();
  endif

  if (nargin == 1)
    if (! (isnumeric (x) && ismatrix (x)))
      error ("cart2pol: matrix input must be 2-D numeric array");
    endif
    if (isvector (x))
      n = numel (x);
      if (n != 2 && n != 3)
        error ("cart2pol: matrix input must be a 2- or 3-element vector or a 2- or 3-column array");
      endif
      if (n == 3)
        z = x(3);
      endif
      y = x(2);
      x = x(1);
    else
      ncols = columns (x);
      if (ncols != 2 && ncols != 3)
        error ("cart2pol: matrix input must be a 2- or 3-element vector or a 2- or 3-column array");
      endif

      if (ncols == 3)
        z = x(:,3);
      endif
      y = x(:,2);
      x = x(:,1);
    endif

  elseif (nargin == 2)
    if (! (isnumeric (x) && isnumeric (y)))
      error ("cart2pol: X, Y must be numeric arrays or scalars");
    endif
    [err, x, y] = common_size (x, y);
    if (err)
      error ("cart2pol: X, Y must be the same size or scalars");
    endif

  elseif (nargin == 3)
    if (! (isnumeric (x) && isnumeric (y) && isnumeric (z)))
      error ("cart2pol: X, Y, Z must be numeric arrays or scalars");
    endif
    [err, x, y, z] = common_size (x, y, z);
    if (err)
      error ("cart2pol: X, Y, Z must be the same size or scalars");
    endif
  endif

  theta = atan2 (y, x);
  r = sqrt (x .^ 2 + y .^ 2);

endfunction


%!test
%! x = [0, 1, 2];
%! y = 0;
%! [t, r] = cart2pol (x, y);
%! assert (t, [0, 0, 0]);
%! assert (r, x);

%!test
%! x = [0, 1, 2];
%! y = [0, 1, 2];
%! [t, r] = cart2pol (x, y);
%! assert (t, [0, pi/4, pi/4], eps);
%! assert (r, sqrt (2)*[0, 1, 2], eps);

%!test
%! x = [0, 1, 2]';
%! y = [0, 1, 2]';
%! [t, r] = cart2pol (x, y);
%! assert (t, [0; pi/4; pi/4], eps);
%! assert (r, sqrt (2)*[0; 1; 2], eps);

%!test
%! x = [0, 1, 2];
%! y = [0, 1, 2];
%! z = [0, 1, 2];
%! [t, r, z2] = cart2pol (x, y, z);
%! assert (t, [0, pi/4, pi/4], sqrt (eps));
%! assert (r, sqrt (2)*[0, 1, 2], sqrt (eps));
%! assert (z2, z);

%!test
%! x = [0, 1, 2];
%! y = 0;
%! z = 0;
%! [t, r, z2] = cart2pol (x, y, z);
%! assert (t, [0, 0, 0], eps);
%! assert (r, x, eps);
%! assert (z2, [0, 0, 0]);

%!test
%! x = 0;
%! y = [0, 1, 2];
%! z = 0;
%! [t, r, z2] = cart2pol (x, y, z);
%! assert (t, [0, 1, 1]*pi/2, eps);
%! assert (r, y, eps);
%! assert (z2, [0, 0, 0]);

%!test
%! x = 0;
%! y = 0;
%! z = [0, 1, 2];
%! [t, r, z2] = cart2pol (x, y, z);
%! assert (t, [0, 0, 0]);
%! assert (r, [0, 0, 0]);
%! assert (z2, z);

%!test
%! C = [0, 0; 1, 1; 2, 2];
%! [t, r] = cart2pol (C);
%! assert (t, [0; 1; 1]*pi/4, eps);
%! assert (r, [0; 1; 2]*sqrt(2), eps);

%!test
%! C = [0, 0, 0; 1, 1, 1; 2, 2, 2];
%! [t, r, z] = cart2pol (C);
%! assert (t, [0; 1; 1]*pi/4, eps);
%! assert (r, [0; 1; 2]*sqrt(2), eps);
%! assert (z, [0; 1; 2]);

%!test
%! C = [0, 0, 0; 1, 1, 1; 2, 2, 2;1, 1, 1];
%! [t, r, z] = cart2pol (C);
%! assert (t, [0; 1; 1; 1]*pi/4, eps);
%! assert (r, [0; 1; 2; 1]*sqrt(2), eps);
%! assert (z, [0; 1; 2; 1]);

%!test
%! x = zeros (1, 1, 1, 2);
%! x(1, 1, 1, 2) = sqrt (2);
%! y = x;
%! [t, r] = cart2pol (x, y);
%! T = zeros (1, 1, 1, 2);
%! T(1, 1, 1, 2) = pi/4;
%! R = zeros (1, 1, 1, 2);
%! R(1, 1, 1, 2) = 2;
%! assert (t, T, eps);
%! assert (r, R, eps);

%!test
%! [x, y, Z] = meshgrid ([0, 1], [0, 1], [0, 1]);
%! [t, r, z] = cart2pol (x, y, Z);
%! T(:, :, 1) = [0, 0; pi/2, pi/4];
%! T(:, :, 2) = T(:, :, 1);
%! R = sqrt (x.^2 + y.^2);
%! assert (t, T, eps);
%! assert (r, R, eps);
%! assert (z, Z);

## Test input validation
%!error <Invalid call> cart2pol ()
%!error cart2pol (1,2,3,4)
%!error <matrix input must be 2-D numeric array> cart2pol ({1,2,3})
%!error <matrix input must be 2-D numeric array> cart2pol (ones (3,3,2))
%!error <matrix input must be a 2- or 3-element> cart2pol ([1])
%!error <matrix input must be a 2- or 3-element> cart2pol ([1,2,3,4])
%!error <must be numeric arrays or scalars> cart2pol ({1,2,3}, [1,2,3])
%!error <must be numeric arrays or scalars> cart2pol ([1,2,3], {1,2,3})
%!error <must be the same size or scalars> cart2pol (ones (3,3,3), ones (3,2,3))
%!error <must be the same size or scalars> cart2pol ([1; 1], [2, 2])
%!error <must be the same size or scalars> cart2pol ([1; 1], [2, 2], [3, 3])
%!error <must be numeric arrays or scalars> cart2pol ({1,2,3}, [1,2,3], [1,2,3])
%!error <must be numeric arrays or scalars> cart2pol ([1,2,3], {1,2,3}, [1,2,3])
%!error <must be numeric arrays or scalars> cart2pol ([1,2,3], [1,2,3], {1,2,3})
%!error <must be the same size or scalars> cart2pol (ones (3,3,3), 1, ones (3,2,3))
%!error <must be the same size or scalars> cart2pol (ones (3,3,3), ones (3,2,3), 1)
