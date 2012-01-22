## Copyright (C) 1996-2012 A. S. Hodel
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

## -*- texinfo -*-
## @deftypefn {Function File} {} is_duplicate_entry (@var{x})
## Return non-zero if any entries in @var{x} are duplicates of one
## another.
## @seealso{unique}
## @end deftypefn

## Author: A. S. Hodel <scotte@eng.auburn.edu>

function retval = is_duplicate_entry (x)

  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:deprecated-function",
             "is_duplicate_entry is obsolete and will be removed from a future version of Octave; see the function unique for equivalent functionality");
  endif


  if (nargin == 1)
    if (ismatrix (x))
      lx = numel (x);
      lx1 = lx-1;
      x = sort (reshape (x, 1, lx));
      dx = x(1:lx1) - x(2:lx);
      retval = sum (dx == 0);
    else
      error ("is_duplicate_entry: expecting matrix argument");
    endif
  else
    print_usage ();
  endif

endfunction

