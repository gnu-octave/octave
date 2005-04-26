## Copyright (C) 1998 Auburn University.  All rights reserved.
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
## @deftypefn {Function File} {} qderiv (omega)
## Derivative of a quaternion.
##
## Let Q be a quaternion to transform a vector from a fixed frame to
## a rotating frame.  If the rotating frame is rotating about the 
## [x, y, z] axes at angular rates [wx, wy, wz], then the derivative
## of Q is given by
##
## @example
## Q' = qderivmat (omega) * Q
## @end example
##
## If the passive convention is used (rotate the frame, not the vector),
## then
##
## @example
## Q' = -qderivmat (omega) * Q
## @end example
## @end deftypefn

## Author: A. S. Hodel <a.s.hodel@eng.auburn.edu>
## Adapted-By: jwe

function Dmat = qderivmat (Omega)

  Omega = vec (Omega);

  if (length (Omega) != 3)
    error ("qderivmat: Omega must be a length 3 vector");
  endif

  Dmat = 0.5 * [      0.0,  Omega(3), -Omega(2),  Omega(1);
                -Omega(3),       0.0,  Omega(1),  Omega(2);
                 Omega(2), -Omega(1),       0.0,  Omega(3);
                -Omega(1), -Omega(2), -Omega(3),       0.0 ];
endfunction
