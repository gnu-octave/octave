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
## @deftypefn  {Function File} {[@var{theta}, @var{r}] =} cart2pol (@var{x}, @var{y})
## @deftypefnx {Function File} {[@var{theta}, @var{r}, @var{z}] =} cart2pol (@var{x}, @var{y}, @var{z})
## Transform Cartesian to polar or cylindrical coordinates.
## @var{x}, @var{y} (and @var{z}) must be the same shape, or scalar.
## @var{theta} describes the angle relative to the positive x-axis.
## @var{r} is the distance to the z-axis @w{(0, 0, z)}.
## @seealso{pol2cart, cart2sph, sph2cart}
## @end deftypefn

## Author: Kai Habel <kai.habel@gmx.de>
## Adapted-by: jwe

function [theta, r, z] = cart2pol (x, y, z)

  if (nargin < 2 || nargin > 3)
    error ("cart2pol: number of arguments must be 2 or 3");
  endif

  if (nargin == 2 && nargout > 2)
    error ("cart2pol: number of output arguments must not be greater than number of input arguments");
  endif

  if ((ismatrix (x) && ismatrix (y) && (nargin == 2 || ismatrix (z)))
      && (size_equal (x, y) || isscalar (x) || isscalar (y))
      && (nargin == 2 || size_equal (x, z) || isscalar (x) || isscalar (z))
      && (nargin == 2 || size_equal (y, z) || isscalar (y) || isscalar (z)))
  
    theta = atan2 (y, x);
    r = sqrt (x .^ 2 + y .^ 2);

  else
    error ("cart2pol: arguments must be matrices of same size, or scalar");
  endif

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
%! assert (t, [0, pi/4, pi/4], sqrt(eps));
%! assert (r, sqrt(2)*[0, 1, 2], sqrt(eps));

%!test
%! x = [0, 1, 2];
%! y = [0, 1, 2];
%! z = [0, 1, 2];
%! [t, r, z2] = cart2pol (x, y, z);
%! assert (t, [0, pi/4, pi/4], sqrt(eps));
%! assert (r, sqrt(2)*[0, 1, 2], sqrt(eps));
%! assert (z, z2);

%!test
%! x = [0, 1, 2];
%! y = 0;
%! z = 0;
%! [t, r, z2] = cart2pol (x, y, z);
%! assert (t, [0, 0, 0], eps);
%! assert (r, x, eps);
%! assert (z, z2);

%!test
%! x = 0;
%! y = [0, 1, 2];
%! z = 0;
%! [t, r, z2] = cart2pol (x, y, z);
%! assert (t, [0, 1, 1]*pi/2, eps);
%! assert (r, y, eps);
%! assert (z, z2);

%!test
%! x = 0;
%! y = 0;
%! z = [0, 1, 2];
%! [t, r, z2] = cart2pol (x, y, z);
%! assert (t, 0);
%! assert (r, 0);
%! assert (z, z2);

