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
## @deftypefn  {} {[@var{x}, @var{y}] =} pol2cart (@var{theta}, @var{r})
## @deftypefnx {} {[@var{x}, @var{y}, @var{z}] =} pol2cart (@var{theta}, @var{r}, @var{z})
## @deftypefnx {} {[@var{x}, @var{y}] =} pol2cart (@var{P})
## @deftypefnx {} {[@var{x}, @var{y}, @var{z}] =} pol2cart (@var{P})
## Transform polar or cylindrical coordinates to Cartesian coordinates.
##
## The inputs @var{theta}, @var{r}, (and @var{z}) must be the same shape, or
## scalar.  If called with a single matrix argument then each row of @var{P}
## represents the polar coordinate pair (@var{theta}, @var{r}) or the
## cylindrical triplet (@var{theta}, @var{r}, @var{z}).
##
## The outputs @var{x}, @var{y} (, and @var{z}) match the shape of the inputs.
## For a matrix input @var{P} the outputs will be column vectors with rows
## corresponding to the rows of the input matrix.
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
## $$ x = r \cos \theta $$
## $$ y = r \sin \theta $$
## $$ z = z $$
## @end tex
## @ifnottex
##
## @example
## @group
## @var{x} = @var{r} * cos (@var{theta})
## @var{y} = @var{r} * sin (@var{theta})
## @var{z} = @var{z}
## @end group
## @end example
##
## @end ifnottex
## @c FIXME: Remove this note in Octave 9.1 (two releases after 7.1).
## Note: For @sc{matlab} compatibility, this function no longer returns a full
## coordinate matrix when called with a single return argument.
## @seealso{cart2pol, sph2cart, cart2sph}
## @end deftypefn

function [x, y, z] = pol2cart (theta, r, z = [])

  if (nargin < 1)
    print_usage ();
  endif

  if (nargin == 1)
    if (! (isnumeric (theta) && ismatrix (theta)))
      error ("cart2pol: matrix input P must be 2-D numeric array");
    endif
    if (isvector (theta))
      n = numel (theta);
      if (n != 2 && n != 3)
        error ("cart2pol: matrix input must be a 2- or 3-element vector or a 2- or 3-column array");
      endif
      if (n == 3)
        z = theta(3);
      endif
      r = theta(2);
      theta = theta(1);

    else
      ncols = columns (theta);
      if (ncols != 2 && ncols != 3)
        error ("cart2pol: matrix input must be a 2- or 3-element vector or a 2- or 3-column array");
      endif

      if (ncols == 3)
        z = theta(:,3);
      endif
      r = theta(:,2);
      theta = theta(:,1);
    endif

  elseif (nargin == 2)
    if (! (isnumeric (theta) && isnumeric (r)))
      error ("pol2cart: THETA, R must be numeric arrays or scalars");
    endif
    [err, theta, r] = common_size (theta, r);
    if (err)
      error ("pol2cart: THETA, R must be the same size or scalars");
    endif

  elseif (nargin == 3)
    if (! (isnumeric (theta) && isnumeric (r) && isnumeric (z)))
      error ("pol2cart: THETA, R, Z must be numeric arrays or scalars");
    endif
    [err, theta, r, z] = common_size (theta, r, z);
    if (err)
      error ("pol2cart: THETA, R, Z must be the same size or scalars");
    endif
  endif

  x = r .* cos (theta);
  y = r .* sin (theta);

endfunction


%!test
%! t = [0, 0.5, 1] * pi;
%! r = 1;
%! [x, y] = pol2cart (t, r);
%! assert (x, [1, 0, -1], eps);
%! assert (y, [0, 1,  0], eps);

%!test
%! t = [0, 1, 1] * pi/4;
%! r = sqrt (2) * [0, 1, 2];
%! [x, y] = pol2cart (t, r);
%! assert (x, [0, 1, 2], 2*eps);
%! assert (y, [0, 1, 2], 2*eps);

%!test
%! t = [0, 1, 1] * pi/4;
%! r = sqrt (2) * [0, 1, 2];
%! z = [0, 1, 2];
%! [x, y, z2] = pol2cart (t, r, z);
%! assert (x, [0, 1, 2], 2*eps);
%! assert (y, [0, 1, 2], 2*eps);
%! assert (z2, z);

%!test
%! t = [0; 1; 1] * pi/4;
%! r = sqrt (2) * [0; 1; 2];
%! z = [0; 1; 2];
%! [x, y, z2] = pol2cart (t, r, z);
%! assert (x, [0; 1; 2], 2*eps);
%! assert (y, [0; 1; 2], 2*eps);
%! assert (z2, z);


%!test
%! t = 0;
%! r = [0, 1, 2];
%! z = [0, 1, 2];
%! [x, y, z2] = pol2cart (t, r, z);
%! assert (x, [0, 1, 2], eps);
%! assert (y, [0, 0, 0], eps);
%! assert (z2, z);

%!test
%! t = [1, 1, 1]*pi/4;
%! r = 1;
%! z = [0, 1, 2];
%! [x, y, z2] = pol2cart (t, r, z);
%! assert (x, [1, 1, 1] / sqrt (2), eps);
%! assert (y, [1, 1, 1] / sqrt (2), eps);
%! assert (z2, z);

%!test
%! t = 0;
%! r = [1, 2, 3];
%! z = 1;
%! [x, y, z2] = pol2cart (t, r, z);
%! assert (x, [1, 2, 3], eps);
%! assert (y, [0, 0, 0] / sqrt (2), eps);
%! assert (z2, [1, 1, 1]);

%!test
%! P = [0, 0; pi/4, sqrt(2); pi/4, 2*sqrt(2)];
%! [x, y] = pol2cart(P);
%! assert (x, [0; 1; 2], 2*eps);
%! assert (y, [0; 1; 2], 2*eps);

%!test
%! P = [0, 0, 0; pi/4, sqrt(2), 1; pi/4, 2*sqrt(2), 2];
%! [x, y, z] = pol2cart(P);
%! assert (x, [0; 1; 2], 2*eps);
%! assert (y, [0; 1; 2], 2*eps);
%! assert (z, P(:,3), 2*eps);

%!test
%! P = [0, 0, 0; pi/4, sqrt(2), 1; pi/4, 2*sqrt(2), 2; 0, 0, 0];
%! [x, y, z] = pol2cart(P);
%! assert (x, [0; 1; 2; 0], 2*eps);
%! assert (y, [0; 1; 2; 0], 2*eps);
%! assert (z, P(:,3), 2*eps);

%!test
%! r = ones (1, 1, 1, 2);
%! r(1, 1, 1, 2) = 2;
%! t = pi/2 * r;
%! [x, y] = pol2cart (t, r);
%! X = zeros (1, 1, 1, 2);
%! X(1, 1, 1, 2) = -2;
%! Y = zeros (1, 1, 1, 2);
%! Y(1, 1, 1, 1) = 1;
%! assert (x, X, 2*eps);
%! assert (y, Y, 2*eps);

%!test
%! [t, r, Z] = meshgrid ([0, pi/2], [1, 2], [0, 1]);
%! [x, y, z] = pol2cart (t, r, Z);
%! X = zeros (2, 2, 2);
%! X(:, 1, 1) = [1; 2];
%! X(:, 1, 2) = [1; 2];
%! Y = zeros (2, 2, 2);
%! Y(:, 2, 1) = [1; 2];
%! Y(:, 2, 2) = [1; 2];
%! assert (x, X, eps);
%! assert (y, Y, eps);
%! assert (z, Z);

## Test input validation
%!error <Invalid call> pol2cart ()
%!error <matrix input P must be 2-D numeric array> pol2cart ({1,2,3})
%!error <matrix input P must be 2-D numeric array> pol2cart (ones (3,3,2))
%!error <matrix input must be a 2- or 3-element> pol2cart ([1])
%!error <matrix input must be a 2- or 3-element> pol2cart ([1,2,3,4])
%!error <must be numeric arrays or scalars> pol2cart ({1,2,3}, [1,2,3])
%!error <must be numeric arrays or scalars> pol2cart ([1,2,3], {1,2,3})
%!error <must be the same size or scalars> pol2cart (ones (3,3,3), ones (3,2,3))
%!error <must be the same size or scalars> pol2cart ([1; 1], [2, 2])
%!error <must be the same size or scalars> pol2cart ([1; 1], [2, 2], [3, 3])
%!error <must be numeric arrays or scalars> pol2cart ({1,2,3}, [1,2,3], [1,2,3])
%!error <must be numeric arrays or scalars> pol2cart ([1,2,3], {1,2,3}, [1,2,3])
%!error <must be numeric arrays or scalars> pol2cart ([1,2,3], [1,2,3], {1,2,3})
%!error <must be the same size or scalars> pol2cart (ones (3,3,3), 1, ones (3,2,3))
%!error <must be the same size or scalars> pol2cart (ones (3,3,3), ones (3,2,3), 1)
