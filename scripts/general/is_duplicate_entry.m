## Copyright (C) 1996, 1997 A. S. Hodel <scotte@eng.auburn.edu>
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
## Software Foundation, 59 Temple Place - Suite 330, Boston, MA
## 02111-1307, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} is_duplicate_entry (@var{x})
## Return non-zero if any entries in @var{x} are duplicates of one
## another.
## @end deftypefn

## Author: A. S. Hodel <scotte@eng.auburn.edu>

function retval = is_duplicate_entry (x)

  if (nargin == 1)
    if (is_matrix (x))
      [m, n] = size (x);
      lx = m*n;
      lx1 = lx-1;
      x = sort (reshape (x, 1, lx));
      dx = x(1:lx1) - x(2:lx);
      retval = sum (dx == 0);
    else
      error ("is_duplicate_entry: expecting matrix argument");
    endif
  else
    usage ("is_duplicate_entry (x)");
  endif

endfunction

