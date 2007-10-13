/*

Copyright (C) 1997, 1998, 2000, 2002, 2007 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#include <octave/oct.h>

DEFUN_DLD (oregonator, args, ,
  "The `oregonator'.\n\
\n\
Reference:\n\
\n\
  Oscillations in chemical systems.  IV.  Limit cycle behavior in a\n\
  model of a real chemical reaction. Richard J. Field and Richard\n\
  M. Noyes, The Journal of Chemical Physics, Volume 60 Number 5,\n\
  March 1974.")
{
  ColumnVector dx (3);

  ColumnVector x (args(0).vector_value ());

  dx(0) = 77.27 * (x(1) - x(0)*x(1) + x(0) - 8.375e-06*pow (x(0), 2.0));
  dx(1) = (x(2) - x(0)*x(1) - x(1)) / 77.27;
  dx(2) = 0.161*(x(0) - x(2));

  return octave_value (dx);
}
