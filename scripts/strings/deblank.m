### Copyright (C) 1996 Kurt Hornik
### 
### This file is part of Octave.
###
### Octave is free software; you can redistribute it and/or modify it
### under the terms of the GNU General Public License as published by
### the Free Software Foundation; either version 2, or (at your option)
### any later version.
###
### Octave is distributed in the hope that it will be useful, but
### WITHOUT ANY WARRANTY; without even the implied warranty of
### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
### General Public License for more details.
###
### You should have received a copy of the GNU General Public License
### along with Octave; see the file COPYING.  If not, write to the Free
### Software Foundation, 59 Temple Place - Suite 330, Boston, MA
### 02111-1307, USA.

## usage:  deblank (s)
##
## Remove trailing blanks from the string s.

function t = deblank (s)
  
## Original version by Kurt Hornik <Kurt.Hornik@ci.tuwien.ac.at>.

  if (nargin != 1)
    usage ("deblank (s)");
  endif
  
  if (isstr (s))

    [nr, nc] = size (s);
    len = nr * nc;

    if (len == 0)
      t = s;
    else
      s = reshape (s, 1, len);
      k = max (find (s != " "));
      t = s (1:k);
    endif

  else
    error ("deblank: expecting string argument");
  endif

endfunction
