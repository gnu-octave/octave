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
## Software Foundation, 59 Temple Place, Suite 330, Boston, MA 02111 USA.

## -*- texinfo -*-
## @deftypefn {Function File} {@var{vec} =} tf2sysl (@var{vec})
## used internally in @ref{tf2sys}.
## strip leading zero coefficients to get the true polynomial length
## @end deftypefn

function vec = tf2sysl (vec)

  while( (length(vec) > 1) & (vec(1) == 0) )
    vec = vec(2:length(vec));
  endwhile

  if(vec(1) == 0)
    warning("tf2sys: polynomial has no nonzero coefficients!")
  endif

endfunction
