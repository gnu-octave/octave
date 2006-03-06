## Copyright (C) 2000  Kai Habel
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {[@var{theta}, @var{r}] =} cart2pol (@var{x}, @var{y})
## @deftypefnx {Function File} {[@var{theta}, @var{r}, @var{z}] =} cart2pol (@var{x}, @var{y}, @var{z})
## Transform cartesian to polar or cylindrical coordinates.
## @var{x}, @var{y} (and @var{z}) must be of same shape.
## @var{theta} describes the angle relative to the x - axis.
## @var{r} is the distance to the z - axis (0, 0, z).
## @seealso{pol2cart, cart2sph, sph2cart}
## @end deftypefn

## Author: Kai Habel <kai.habel@gmx.de>
## Adapted-by: jwe

function [Theta, R, Z] = cart2pol (X, Y, Z)

  if (nargin < 2 || nargin > 3)
    error ("cart2pol: number of arguments must be 2 or 3");
  endif

  if (nargin == 2 && nargout > 2)
    error ("cart2pol: number of output arguments must not be greater than number of input arguments");
  endif

  if ((! (ismatrix (X) && ismatrix (Y)))
      || (size (X) != size (Y))
      || (nargin == 3 && (! (size (X) == size (Z) && ismatrix (Z)))))
    error ("cart2pol: arguments must be matrices of same size");
  endif

  Theta = atan2 (Y, X);
  R = sqrt (X .^ 2 + Y .^ 2);

endfunction
