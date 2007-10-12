## Copyright (C) 1996, 2004, 2005, 2007
##               Auburn University.  All rights reserved.
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
