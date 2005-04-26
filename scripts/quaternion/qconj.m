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
## @deftypefn {Function File} {} qconj (@var{q})
## Conjugate of a quaternion.
##
## @example
## q = [w, x, y, z] = w*i + x*j + y*k + z
## qconj (q) = -w*i -x*j -y*k + z
## @end example
## @end deftypefn

## Author: A. S. Hodel <a.s.hodel@eng.auburn.edu>
## Adapted-By: jwe

function retval = qconj (q)

  [a, b, c, d] = quaternion (q);

  retval = quaternion (-a, -b, -c, d);

endfunction

