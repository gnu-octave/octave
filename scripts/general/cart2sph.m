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
## @deftypefn {Function File} {[@var{theta}, @var{phi}, @var{r}] =} cart2sph (@var{x}, @var{y}, @var{z})
## Transform cartesian to spherical coordinates.
## @var{x}, @var{y} and @var{z} must be of same shape.
## @var{theta} describes the angle relative to the x - axis.
## @var{phi} is the angle relative to the xy - plane.
## @var{r} is the distance to the origin (0, 0, 0).
## @seealso{pol2cart, cart2pol, sph2cart}
## @end deftypefn

## Author: Kai Habel <kai.habel@gmx.de>
## Adapted-by: jwe

function [Theta, Phi, R] = cart2sph (X, Y, Z)

  if (nargin != 3)
    usage ("[Theta, Phi, R] = cart2sph (X, Y, Z)")
  endif

  if ((! (ismatrix (X) && ismatrix (Y) && ismatrix (Z)))
      || size (X) != size (Y)
      || size (X) != size (Z))
    error ("cart2sph: arguments must be matrices of same size");
  endif

  Theta = atan2 (Y, X);
  Phi = atan2 (Z, sqrt (X .^ 2 + Y .^ 2));
  R = sqrt (X .^ 2 + Y .^ 2 + Z .^ 2);

endfunction
