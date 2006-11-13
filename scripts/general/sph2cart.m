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
## @deftypefn {Function File} {[@var{x}, @var{y}, @var{z}] =} sph2cart (@var{theta}, @var{phi}, @var{r})
## Transform spherical to cartesian coordinates.
## @var{x}, @var{y} and @var{z} must be of same shape.
## @var{theta} describes the angle relative to the x-axis.
## @var{phi} is the angle relative to the xy-plane.
## @var{r} is the distance to the origin (0, 0, 0).
## @seealso{pol2cart, cart2pol, cart2sph}
## @end deftypefn

## Author: Kai Habel <kai.habel@gmx.de>
## Adapted-by: jwe

function [X, Y, Z] = sph2cart (Theta, Phi, R)

  if (nargin != 3)
    print_usage ();
  endif

  if ((! (ismatrix (Theta) && ismatrix (Phi) && ismatrix (R)))
      || (! size_equal (Theta, Phi))
      || (! size_equal (Theta, R)))
    error ("sph2cart: arguments must be matrices of same size");
  endif

  X = R .* cos (Phi) .* cos (Theta);
  Y = R .* cos (Phi) .* sin (Theta);
  Z = R .* sin (Phi);

endfunction
