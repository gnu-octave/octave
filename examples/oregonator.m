## Copyright (C) 1997, 1998, 2007 John W. Eaton
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

## The `oregonator'.
##
## Reference:
##
##   Oscillations in chemical systems.  IV.  Limit cycle behavior in a
##   model of a real chemical reaction. Richard J. Field and Richard
##   M. Noyes, The Journal of Chemical Physics, Volume 60 Number 5,
##   March 1974.

function dx = oregonator (x, t)

  dx = zeros (3, 1);

  dx(1) = 77.27*(x(2) - x(1)*x(2) + x(1) - 8.375e-06*x(1)^2);
  dx(2) = (x(3) - x(1)*x(2) - x(2)) / 77.27;
  dx(3) = 0.161*(x(1) - x(3));

end
