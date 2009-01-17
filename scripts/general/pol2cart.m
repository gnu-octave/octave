## Copyright (C) 2000, 2001, 2002, 2004, 2005, 2006, 2007 Kai Habel
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
## @deftypefn {Function File} {[@var{x}, @var{y}] =} pol2cart (@var{theta}, @var{r})
## @deftypefnx {Function File} {[@var{x}, @var{y}, @var{z}] =} pol2cart (@var{theta}, @var{r}, @var{z})
## Transform polar or cylindrical to cartesian coordinates.
## @var{theta}, @var{r} (and @var{z}) must be of same shape, or scalar.
## @var{theta} describes the angle relative to the x-axis.
## @var{r} is the distance to the z-axis (0, 0, z).
## @seealso{cart2pol, cart2sph, sph2cart}
## @end deftypefn

## Author: Kai Habel <kai.habel@gmx.de>
## Adapted-by: jwe

function [x, y, z] = pol2cart (theta, r, z)

  if (nargin < 2 || nargin > 3)
    error ("pol2cart: number of arguments must be 2 or 3");
  endif

  if (nargin == 2 && nargout > 2)
    error ("pol2cart: number of output arguments must not be greater than number of input arguments");
  endif

   if ((ismatrix (theta) && ismatrix (r) && (nargin == 2 || ismatrix (z)))
       && (size_equal (theta, r) || isscalar (theta) || isscalar (r))
       && (nargin == 2 || size_equal (theta, z) || isscalar (theta) || isscalar (z))
       && (nargin == 2 || size_equal (r, z) || isscalar (r) || isscalar (z)))

    x = cos (theta) .* r;
    y = sin (theta) .* r;

  else
    error ("pol2cart: arguments must be matrices of same size, or scalar");
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

