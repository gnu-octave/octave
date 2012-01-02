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
## @deftypefn  {Function File} {[@var{x}, @var{y}] =} pol2cart (@var{theta}, @var{r})
## @deftypefnx {Function File} {[@var{x}, @var{y}, @var{z}] =} pol2cart (@var{theta}, @var{r}, @var{z})
## @deftypefnx {Function File} {[@var{x}, @var{y}] =} pol2cart (@var{p})
## @deftypefnx {Function File} {[@var{x}, @var{y}, @var{z}] =} pol2cart (@var{p})
## @deftypefnx {Function File} {@var{C} =} pol2cart (@dots{})
## Transform polar or cylindrical to Cartesian coordinates.
##
## @var{theta}, @var{r}, (and @var{z}) must be the same shape, or scalar.
## @var{theta} describes the angle relative to the positive x-axis.
## @var{r} is the distance to the z-axis (0, 0, z).
## If called with a single matrix argument then each row of @var{p}
## represents the polar/(cylindrical) coordinate (@var{x}, @var{y} (, @var{z})).
##
## If only a single return argument is requested then return a matrix
## @var{C} where each row represents one Cartesian coordinate
## (@var{x}, @var{y} (, @var{z})).
## @seealso{cart2pol, sph2cart, cart2sph}
## @end deftypefn

## Author: Kai Habel <kai.habel@gmx.de>
## Adapted-by: jwe

function [x, y, z] = pol2cart (theta, r, z)

  if (nargin < 1 || nargin > 3)
    print_usage ();
  endif

  if (nargin == 1)
    if (ismatrix (theta) && (columns (theta) == 2 || columns (theta) == 3))
      if (columns (theta) == 3)
        z = theta(:,3);
      else
        z = [];
      endif
      r = theta(:,2);
      theta = theta(:,1);
    else
      error ("pol2car: matrix input must have 2 or 3 columns [THETA, R (, Z)]");
    endif
  elseif (nargin == 2)
    if (! ((ismatrix (theta) && ismatrix (r))
            && (size_equal (theta, r) || isscalar (theta) || isscalar (r))))
      error ("pol2cart: arguments must be matrices of same size, or scalar");
    endif
  elseif (nargin == 3)
    if (! ((ismatrix (theta) && ismatrix (r) && ismatrix (z))
            && (size_equal (theta, r) || isscalar (theta) || isscalar (r))
            && (size_equal (theta, z) || isscalar (theta) || isscalar (z))
            && (size_equal (r, z) || isscalar (r) || isscalar (z))))
      error ("pol2cart: arguments must be matrices of same size, or scalar");
    endif
  endif

  x = r .* cos (theta);
  y = r .* sin (theta);

  if (nargout <= 1)
    x  = [x, y, z];
  endif

endfunction

%!test
%! t = [0, 0.5, 1] * pi;
%! r = 1;
%! [x, y] = pol2cart (t, r);
%! assert (x, [1, 0, -1], sqrt(eps));
%! assert (y, [0, 1,  0], sqrt(eps));

%!test
%! t = [0, 1, 1] * pi/4;
%! r = sqrt(2) * [0, 1, 2];
%! [x, y] = pol2cart (t, r);
%! assert (x, [0, 1, 2], sqrt(eps));
%! assert (y, [0, 1, 2], sqrt(eps));

%!test
%! t = [0, 1, 1] * pi/4;
%! r = sqrt(2) * [0, 1, 2];
%! z = [0, 1, 2];
%! [x, y, z2] = pol2cart (t, r, z);
%! assert (x, [0, 1, 2], sqrt(eps));
%! assert (y, [0, 1, 2], sqrt(eps));
%! assert (z, z2);

%!test
%! t = 0;
%! r = [0, 1, 2];
%! z = [0, 1, 2];
%! [x, y, z2] = pol2cart (t, r, z);
%! assert (x, [0, 1, 2], sqrt(eps));
%! assert (y, [0, 0, 0], sqrt(eps));
%! assert (z, z2);

%!test
%! t = [1, 1, 1]*pi/4;
%! r = 1;
%! z = [0, 1, 2];
%! [x, y, z2] = pol2cart (t, r, z);
%! assert (x, [1, 1, 1] / sqrt(2), eps);
%! assert (y, [1, 1, 1] / sqrt(2), eps);
%! assert (z, z2);

%!test
%! t = 0;
%! r = [1, 2, 3];
%! z = 1;
%! [x, y, z2] = pol2cart (t, r, z);
%! assert (x, [1, 2, 3], eps);
%! assert (y, [0, 0, 0] / sqrt(2), eps);
%! assert (z, z2);

%!test
%! P = [0, 0; pi/4, sqrt(2); pi/4, 2*sqrt(2)];
%! C = [0, 0; 1, 1; 2, 2];
%! assert (pol2cart(P), C, sqrt(eps));

%!test
%! P = [0, 0, 0; pi/4, sqrt(2), 1; pi/4, 2*sqrt(2), 2];
%! C = [0, 0, 0; 1, 1, 1; 2, 2, 2];
%! assert (pol2cart(P), C, sqrt(eps));

