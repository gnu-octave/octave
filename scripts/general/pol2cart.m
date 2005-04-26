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
## @deftypefn {Function File} {} [@var{x}, @var{y}] = pol2cart (@var{theta}, @var{r})
## @deftypefnx {Function File} {} [@var{x}, @var{y}, @var{z}] = pol2cart (@var{theta}, @var{r}, @var{z})
## Transform polar or cylindrical to cartesian coordinates.
## @var{theta}, @var{r} (and @var{z}) must be of same shape.
## @var{theta} describes the angle relative to the x - axis.
## @var{r} is the distance to the z - axis (0, 0, z).
## @end deftypefn
##
## @seealso{cart2pol, cart2sph, sph2cart}

## Author: Kai Habel <kai.habel@gmx.de>
## Adapted-by: jwe

function [X, Y, Z] = pol2cart (Theta, R, Z)

  if (nargin < 2 || nargin > 3)
    error ("pol2cart: number of arguments must be 2 or 3");
  endif

  if (nargin == 2 && nargout > 2)
    error ("pol2cart: number of output arguments must not be greater than number of input arguments");
  endif

  if ((! (ismatrix (Theta) && ismatrix (R)))
      || (size (Theta) != size (R))
      || (nargin == 3 && (! (size (R) == size (Z) && ismatrix (Z)))))
    error ("pol2cart: arguments must be matrices of same size");
  endif

  X = cos (Theta) .* R;
  Y = sin (Theta) .* R;

endfunction
