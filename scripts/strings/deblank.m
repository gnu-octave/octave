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
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} deblank (@var{s})
## Removes the trailing blanks and nulls from the string @var{s}.
## If @var{s} is a matrix, @var{deblank} trims each row to the 
## length of longest string.
## @end deftypefn

## Author: Kurt Hornik <Kurt.Hornik@ci.tuwien.ac.at>
## Adapted-By: jwe

function t = deblank (s)

  if (nargin != 1)
    usage ("deblank (s)");
  endif

  if (isstr (s))

    k = find (! isspace (s) & s != "\0");
    if (isempty (s) || isempty (k))
      t = "";
    else
      t = s(:,1:ceil (max (k) / rows (s)));
    endif

  else
    error ("deblank: expecting string argument");
  endif

endfunction
