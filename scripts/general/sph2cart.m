## Copyright (C) 2000, 2001, 2002, 2004, 2005, 2006, 2007, 2009 Kai Habel
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
## @deftypefn {Function File} {[@var{x}, @var{y}, @var{z}] =} sph2cart (@var{theta}, @var{phi}, @var{r})
## Transform spherical to Cartesian coordinates.
## @var{x}, @var{y} and @var{z} must be the same shape, or scalar.
## @var{theta} describes the angle relative to the positive x-axis.
## @var{phi} is the angle relative to the xy-plane.
## @var{r} is the distance to the origin @w{(0, 0, 0)}.
## @seealso{pol2cart, cart2pol, cart2sph}
## @end deftypefn

## Author: Kai Habel <kai.habel@gmx.de>
## Adapted-by: jwe

function [x, y, z] = sph2cart (theta, phi, r)

  if (nargin != 3)
    print_usage ();
  endif

  if ((ismatrix (theta) && ismatrix (phi) && ismatrix (r))
      && (size_equal (theta, phi) || isscalar (theta) || isscalar (phi))
      && (size_equal (theta, r) || isscalar (theta) || isscalar (r))
      && (size_equal (phi, r) || isscalar (phi) || isscalar (r)))

    x = r .* cos (phi) .* cos (theta);
    y = r .* cos (phi) .* sin (theta);
    z = r .* sin (phi);

  else
    error ("sph2cart: arguments must be matrices of same size, or scalar");
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

