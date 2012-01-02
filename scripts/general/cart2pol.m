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
## @deftypefn  {Function File} {[@var{theta}, @var{r}] =} cart2pol (@var{x}, @var{y})
## @deftypefnx {Function File} {[@var{theta}, @var{r}, @var{z}] =} cart2pol (@var{x}, @var{y}, @var{z})
## @deftypefnx {Function File} {[@var{theta}, @var{r}] =} cart2pol (@var{c})
## @deftypefnx {Function File} {[@var{theta}, @var{r}, @var{z}] =} cart2pol (@var{c})
## @deftypefnx {Function File} {@var{p} =} cart2pol (@dots{})
##
## Transform Cartesian to polar or cylindrical coordinates.
##
## @var{theta} describes the angle relative to the positive x-axis.
## @var{r} is the distance to the z-axis @w{(0, 0, z)}.
## @var{x}, @var{y} (and @var{z}) must be the same shape, or scalar.
## If called with a single matrix argument then each row of @var{c}
## represents the Cartesian coordinate (@var{x}, @var{y} (, @var{z})).
##
## If only a single return argument is requested then return a matrix
## @var{p} where each row represents one polar/(cylindrical) coordinate
## (@var{theta}, @var{phi} (, @var{z})).
## @seealso{pol2cart, cart2sph, sph2cart}
## @end deftypefn

## Author: Kai Habel <kai.habel@gmx.de>
## Adapted-by: jwe

function [theta, r, z] = cart2pol (x, y, z)

  if (nargin < 1 || nargin > 3)
    print_usage ();
  endif

  if (nargin == 1)
    if (ismatrix (x) && (columns (x) == 2 || columns (x) == 3))
      if (columns (x) == 3)
        z = x(:,3);
      else
        z = [];
      endif
      y = x(:,2);
      x = x(:,1);
    else
      error ("cart2pol: matrix input must have 2 or 3 columns [X, Y (, Z)]");
    endif
  elseif (nargin == 2)
    if (! ((ismatrix (x) && ismatrix (y))
            && (size_equal (x, y) || isscalar (x) || isscalar (y))))
      error ("cart2pol: arguments must be matrices of same size, or scalar");
    endif
  elseif (nargin == 3)
    if (! ((ismatrix (x) && ismatrix (y) && ismatrix (z))
            && (size_equal (x, y) || isscalar (x) || isscalar (y))
            && (size_equal (x, z) || isscalar (x) || isscalar (z))
            && (size_equal (y, z) || isscalar (y) || isscalar (z))))
      error ("cart2pol: arguments must be matrices of same size, or scalar");
    endif
  endif

  theta = atan2 (y, x);
  r = sqrt (x .^ 2 + y .^ 2);

  if (nargout <= 1)
    theta = [theta, r, z];
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

%!test
%! C = [0, 0; 1, 1; 2, 2];
%! P = [0, 0; pi/4, sqrt(2); pi/4, 2*sqrt(2)];
%! assert (cart2pol (C), P, sqrt(eps));

%!test
%! C = [0, 0, 0; 1, 1, 1; 2, 2, 2];
%! P = [0, 0, 0; pi/4, sqrt(2), 1; pi/4, 2*sqrt(2), 2];
%! assert (cart2pol (C), P, sqrt(eps));

