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
## @deftypefn {Function File} {} qmult (@var{a}, @var{b})
## Multiply two quaternions.
##
## @example
## [w, x, y, z] = w*i + x*j + y*k + z
## @end example
##
## @noindent
## identities:
##
## @example
## i^2 = j^2 = k^2 = -1
## ij = k                 jk = i
## ki = j                 kj = -i
## ji = -k                ik = -j
## @end example
## @end deftypefn

## Author: A. S. Hodel <a.s.hodel@eng.auburn.edu>
## Adapted-By: jwe

function retval = qmult (a, b)
  
  [a1, b1, c1, d1] = quaternion (a);
  [a2, b2, c2, d2] = quaternion (b);
  
  ri = b1*c2 - c1*b2 + d1*a2 + a1*d2;
  rj = c1*a2 - a1*c2 + d1*b2 + b1*d2;
  rk = a1*b2 - b1*a2 + d1*c2 + c1*d2;
  rr = -(a1*a2 + b1*b2 + c1*c2) + d1*d2;
  
  retval = quaternion (ri, rj, rk, rr);

endfunction

