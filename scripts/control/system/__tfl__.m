## Copyright (C) 1996 Auburn University.  All rights reserved.
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by the
## Free Software Foundation; either version 2, or (at your option) any
## later version.
##
## Octave is distributed in the hope that it will be useful, but WITHOUT
## ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
## FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
## for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301 USA.

## Undocumented internal function.

## -*- texinfo -*-
## @deftypefn {Function File} {} __tfl__ (@var{vec})
## used internally in tf.
## strip leading zero coefficients to get the true polynomial length
## @end deftypefn

function vec = __tfl__ (vec)

  while (length (vec) > 1 && vec(1) == 0)
    vec = vec (2:end);
  endwhile

  if (vec(1) == 0)
    warning ("tf: polynomial has no nonzero coefficients!")
  endif

endfunction
