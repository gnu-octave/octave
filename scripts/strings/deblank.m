## Copyright (C) 1996 Kurt Hornik
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
## @deftypefn {Function File} {} deblank (@var{s})
## Removes the trailing blanks from the string @var{s}. 
## @end deftypefn

## Author: Kurt Hornik <Kurt.Hornik@ci.tuwien.ac.at>
## Adapted-By: jwe

function t = deblank (s)

  if (nargin != 1)
    usage ("deblank (s)");
  endif

  if (isstr (s))

    [nr, nc] = size (s);
    len = nr * nc;

    if (len == 0)
      t = s;
    elseif (s == " ")
      t = "";
    else
      s = reshape (s, 1, len);
      k = ceil (max (find (s != " ")) / nr) * nr;
      t = reshape (s (1:k), nr, k / nr);
    endif

  else
    error ("deblank: expecting string argument");
  endif

endfunction
