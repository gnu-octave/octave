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
## @deftypefn  {Function File} {[@var{theta}, @var{phi}, @var{r}] =} cart2sph (@var{x}, @var{y}, @var{z})
## @deftypefnx {Function File} {[@var{theta}, @var{phi}, @var{r}] =} cart2sph (@var{C})
## @deftypefnx {Function File} {@var{S} =} cart2sph (@dots{})
## Transform Cartesian to spherical coordinates.
##
## @var{theta} describes the angle relative to the positive x-axis.
## @var{phi} is the angle relative to the xy-plane.
## @var{r} is the distance to the origin @w{(0, 0, 0)}.
## @var{x}, @var{y}, and @var{z} must be the same shape, or scalar.
## If called with a single matrix argument then each row of @var{c}
## represents the Cartesian coordinate (@var{x}, @var{y}, @var{z}).
##
## If only a single return argument is requested then return a matrix
## @var{s} where each row represents one spherical coordinate
## (@var{theta}, @var{phi}, @var{r}).
## @seealso{sph2cart, cart2pol, pol2cart}
## @end deftypefn

## Author: Kai Habel <kai.habel@gmx.de>
## Adapted-by: jwe

function [theta, phi, r] = cart2sph (x, y, z)

  if (nargin != 1 && nargin != 3)
    print_usage ();
  endif

  if (nargin == 1)
    if (ismatrix (x) && columns (x) == 3)
      z = x(:,3);
      y = x(:,2);
      x = x(:,1);
    else
      error ("cart2sph: matrix input must have 3 columns [X, Y, Z]");
    endif
  elseif (nargin == 3)
    if (! ((ismatrix (x) && ismatrix (y) && ismatrix (z))
            && (size_equal (x, y) || isscalar (x) || isscalar (y))
            && (size_equal (x, z) || isscalar (x) || isscalar (z))
            && (size_equal (y, z) || isscalar (y) || isscalar (z))))
      error ("cart2sph: X, Y, Z must be matrices of the same size, or scalar");
    endif
  endif

  theta = atan2 (y, x);
  phi = atan2 (z, sqrt (x .^ 2 + y .^ 2));
  r = sqrt (x .^ 2 + y .^ 2 + z .^ 2);

  if (nargout <= 1)
    theta = [theta, phi, r];
  endif

endfunction

%!test
%! x = [0, 1, 2];
%! y = [0, 1, 2];
%! z = [0, 1, 2];
%! [t, p, r] = cart2sph (x, y, z);
%! assert (t, [0, pi/4, pi/4], eps);
%! assert (p, [0, 1, 1]*atan(sqrt(0.5)), eps);
%! assert (r, [0, 1, 2]*sqrt(3), eps);

%!test
%! x = 0;
%! y = [0, 1, 2];
%! z = [0, 1, 2];
%! [t, p, r] = cart2sph (x, y, z);
%! assert (t, [0, 1, 1] * pi/2, eps);
%! assert (p, [0, 1, 1] * pi/4, eps);
%! assert (r, [0, 1, 2] * sqrt(2), eps);

%!test
%! x = [0, 1, 2];
%! y = 0;
%! z = [0, 1, 2];
%! [t, p, r] = cart2sph (x, y, z);
%! assert (t, [0, 0, 0]);
%! assert (p, [0, 1, 1] * pi/4);
%! assert (r, [0, 1, 2] * sqrt(2));

%!test
%! x = [0, 1, 2];
%! y = [0, 1, 2];
%! z = 0;
%! [t, p, r] = cart2sph (x, y, z);
%! assert (t, [0, 1, 1] * pi/4);
%! assert (p, [0, 0, 0]);
%! assert (r, [0, 1, 2] * sqrt(2));

%!test
%! C = [0, 0, 0; 1, 0, 1; 2, 0, 2];
%! S = [0, 0, 0; 0, pi/4, sqrt(2); 0, pi/4, 2*sqrt(2)];
%! assert (cart2sph(C), S, eps);
