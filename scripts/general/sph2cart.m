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
## @deftypefn  {} {[@var{x}, @var{y}, @var{z}] =} sph2cart (@var{theta}, @var{phi}, @var{r})
## @deftypefnx {} {[@var{x}, @var{y}, @var{z}] =} sph2cart (@var{S})
## Transform spherical coordinates to Cartesian coordinates.
##
## The inputs @var{theta}, @var{phi}, and @var{r} must be the same shape, or
## scalar.  If called with a single matrix argument then each row of @var{S}
## must represent a spherical coordinate triplet (@var{theta}, @var{phi},
## @var{r}).
##
## The outputs @var{x}, @var{y}, @var{z} match the shape of the inputs.  For a
## matrix input @var{S} the outputs are column vectors with rows corresponding
## to the rows of the input matrix.
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
## $$ x = r \cos \phi  \cos \theta $$
## $$ y = r \cos \phi  \sin \theta $$
## $$ z = r \sin \phi $$
## @end tex
## @ifnottex
##
## @example
## @group
## @var{x} = r * cos (@var{phi}) * cos (@var{theta})
## @var{y} = r * cos (@var{phi}) * sin (@var{theta})
## @var{z} = r * sin (@var{phi})
## @end group
## @end example
##
## @end ifnottex
## @c FIXME: Remove this note in Octave 9.1 (two releases after 7.1).
## Note: For @sc{matlab} compatibility, this function no longer returns a full
## coordinate matrix when called with a single return argument.
## @seealso{cart2sph, pol2cart, cart2pol}
## @end deftypefn

function [x, y, z] = sph2cart (theta, phi, r)

  if (nargin != 1 && nargin != 3)
    print_usage ();
  endif

  if (nargin == 1)
    if (! (isnumeric (theta) && ismatrix (theta)))
      error ("sph2cart: matrix input must be a 2-D numeric array");
    endif
    if (columns (theta) != 3 && numel (theta) != 3)
      error ("sph2cart: matrix input must be a 3-element vector or 3-column array");
    endif

    if (numel (theta) == 3)
      r = theta(3);
      phi = theta(2);
      theta = theta(1);
    else
      r = theta(:,3);
      phi = theta(:,2);
      theta = theta(:,1);
    endif

  else
    if (! (isnumeric (theta) && isnumeric (phi) && isnumeric (r)))
      error ("sph2cart: THETA, PHI, R must be numeric arrays or scalars");
    endif
    [err, theta, phi, r] = common_size (theta, phi, r);
    if (err)
      error ("sph2cart: THETA, PHI, R must be the same size or scalars");
    endif
  endif

  x = r .* cos (phi) .* cos (theta);
  y = r .* cos (phi) .* sin (theta);
  z = r .* sin (phi);

endfunction


%!test
%! t = [0, 0, 0];
%! p = [0, 0, 0];
%! r = [0, 1, 2];
%! [x, y, z] = sph2cart (t, p, r);
%! assert (x, r);
%! assert (y, [0, 0, 0]);
%! assert (z, [0, 0, 0]);

%!test
%! t = [0; 0; 0];
%! p = [0; 0; 0];
%! r = [0; 1; 2];
%! [x, y, z] = sph2cart (t, p, r);
%! assert (x, [0; 1; 2]);
%! assert (y, [0; 0; 0]);
%! assert (z, [0; 0; 0]);

%!test
%! t = 0;
%! p = [0, 0, 0];
%! r = [0, 1, 2];
%! [x, y, z] = sph2cart (t, p, r);
%! assert (x, [0, 1, 2]);
%! assert (y, [0, 0, 0]);
%! assert (z, [0, 0, 0]);

%!test
%! t = [0, 0, 0];
%! p = 0;
%! r = [0, 1, 2];
%! [x, y, z] = sph2cart (t, p, r);
%! assert (x, r);
%! assert (y, [0, 0, 0]);
%! assert (z, [0, 0, 0]);

%!test
%! t = [0, 0.5, 1]*pi;
%! p = [0, 0, 0];
%! r = 1;
%! [x, y, z] = sph2cart (t, p, r);
%! assert (x, [1, 0, -1], eps);
%! assert (y, [0, 1, 0], eps);
%! assert (z, [0, 0, 0], eps);

%!test
%! [x, y, z] = sph2cart ([0 0 0], 0, 1);
%! assert (x, [1, 1, 1], eps);
%! assert (y, [0, 0, 0], eps);
%! assert (z, [0, 0, 0], eps);

%!test
%! S = [ 0, 0, 1; 0.5*pi, 0, 1; pi, 0, 1];
%! [x, y, z] = sph2cart (S);
%! assert (x, [1; 0; -1], eps);
%! assert (y, [0; 1; 0], eps);
%! assert (z, [0; 0; 0], eps);

%!test
%! S = [ 0, 0, 1; 0.5*pi, 0, 1; pi, 0, 1; pi, pi, 1];
%! [x, y, z] = sph2cart (S);
%! assert (x, [1; 0; -1; 1], eps);
%! assert (y, [0; 1; 0; 0], eps);
%! assert (z, [0; 0; 0; 0], eps);


%!test
%! [t, p, r] = meshgrid ([0, pi/2], [0, pi/2], [0, 1]);
%! [x, y, z] = sph2cart (t, p, r);
%! X = zeros (2, 2, 2);
%! X(1, 1, 2) = 1;
%! Y = zeros (2, 2, 2);
%! Y(1, 2, 2) = 1;
%! Z = zeros (2, 2, 2);
%! Z(2, :, 2) = [1 1];
%! assert (x, X, eps);
%! assert (y, Y, eps);
%! assert (z, Z);

## Test input validation
%!error <Invalid call> sph2cart ()
%!error <Invalid call> sph2cart (1,2)
%!error <matrix input must be a 2-D numeric array> sph2cart ({1,2,3})
%!error <matrix input must be a 2-D numeric array> sph2cart (ones (3,3,2))
%!error <matrix input must be a 3-element> sph2cart ([1,2,3,4])
%!error <matrix input must be a 3-element> sph2cart ([1,2,3,4; 1,2,3,4; 1,2,3,4])
%!error <must be numeric arrays or scalars> sph2cart ({1,2,3}, [1,2,3], [1,2,3])
%!error <must be numeric arrays or scalars> sph2cart ([1,2,3], {1,2,3}, [1,2,3])
%!error <must be numeric arrays or scalars> sph2cart ([1,2,3], [1,2,3], {1,2,3})
%!error <must be the same size or scalars> sph2cart ([1,2,3], [1,2,3], [1,2,3]')
%!error <must be the same size or scalars> sph2cart (ones (3,3,3), 1, ones (3,2,3))
%!error <must be the same size or scalars> sph2cart (ones (3,3,3), ones (3,2,3), 1)
