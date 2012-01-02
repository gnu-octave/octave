## Copyright (C) 2000-2012 Kai Habel
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {Function File} {[@var{x}, @var{y}, @var{z}] =} sph2cart (@var{theta}, @var{phi}, @var{r})
## @deftypefnx {Function File} {[@var{x}, @var{y}, @var{z}] =} sph2cart (@var{S})
## @deftypefnx {Function File} {C =} sph2cart (@dots{})
## Transform spherical to Cartesian coordinates.
##
## @var{theta} describes the angle relative to the positive x-axis.
## @var{phi} is the angle relative to the xy-plane.
## @var{r} is the distance to the origin @w{(0, 0, 0)}.
## @var{theta}, @var{phi}, and @var{r} must be the same shape, or scalar.
## If called with a single matrix argument then each row of @var{s}
## represents the spherical coordinate (@var{theta}, @var{phi}, @var{r}).
##
## If only a single return argument is requested then return a matrix
## @var{C} where each row represents one Cartesian coordinate
## (@var{x}, @var{y}, @var{z}).
## @seealso{cart2sph, pol2cart, cart2pol}
## @end deftypefn

## Author: Kai Habel <kai.habel@gmx.de>
## Adapted-by: jwe

function [x, y, z] = sph2cart (theta, phi, r)

  if (nargin != 1 && nargin != 3)
    print_usage ();
  endif

  if (nargin == 1)
    if (ismatrix (theta) && columns (theta) == 3)
      r = theta(:,3);
      phi = theta(:,2);
      theta = theta(:,1);
    else
      error ("sph2cart: matrix input must have 3 columns [THETA, PHI, R]");
    endif
  elseif (nargin == 3)
    if (! ((ismatrix (theta) && ismatrix (phi) && ismatrix (r))
            && (size_equal (theta, phi) || isscalar (theta) || isscalar (phi))
            && (size_equal (theta, r) || isscalar (theta) || isscalar (r))
            && (size_equal (phi, r) || isscalar (phi) || isscalar (r))))
      error ("sph2cart: THETA, PHI, and R must be matrices of the same size, or scalar");
    endif
  endif

  x = r .* cos (phi) .* cos (theta);
  y = r .* cos (phi) .* sin (theta);
  z = r .* sin (phi);

  if (nargout <= 1)
    x = [x, y, z];
  endif

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
%! t = 0;
%! p = [0, 0, 0];
%! r = [0, 1, 2];
%! [x, y, z] = sph2cart (t, p, r);
%! assert (x, r);
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
%! S = [ 0, 0, 1; 0.5*pi, 0, 1; pi, 0, 1];
%! C = [ 1, 0, 0; 0, 1, 0; -1, 0, 0];
%! assert (sph2cart(S), C, eps);

