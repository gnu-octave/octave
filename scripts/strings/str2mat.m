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

## usage:  m = str2mat (s1, ...)
##
## Forms the matrix M containing the strings S1, ... as its rows.
## Each string is padded with blanks in order to form a valid matrix.

function m = str2mat (...)
  
  ## Original version by Kurt Hornik <Kurt.Hornik@ci.tuwien.ac.at>.

  if (nargin == 0)
    usage ("str2mat (s1, ...)");
  endif

  nc = 0;

  va_start ();

  for k = 1 : nargin
    s = va_arg ();
    if (isstr (s))
      tmp = columns (s);
    else
      error ("str2mat: all arguments must be strings");
    endif

    if (tmp > nc)
      nc = tmp;
    endif
  endfor

  m = setstr (ones (nargin, nc) * toascii (" "));

  va_start ();

  for k = 1 : nargin
    s = va_arg ();
    tmp = columns (s);
    if (tmp > 0)
      m (k, 1:tmp) = s;
    endif
  endfor
  
endfunction
