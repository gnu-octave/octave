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
## @deftypefn {Function File} {} hex2dec (@var{s})
## Return the decimal number corresponding to the hexadecimal number stored
## in the string @var{s}.  For example,
##
## @example
## hex2dec ("12B")
##      @result{} 299
## hex2dec ("12b")
##      @result{} 299
## @end example
## @end deftypefn

## Author: Kurt Hornik <Kurt.Hornik@ci.tuwien.ac.at>
## Adapted-By: jwe

function d = hex2dec (h)

  if (nargin != 1)
    usage ("hex2dec (x)");
  endif

  if (isstr (h))
    nr = rows (h);
    d = zeros (nr, 1);
    for i = 1:nr
      s = h (i, :);
      if (isxdigit (s))
        tmp = sscanf (s, "%x");
        if (isempty (tmp))
          error ("hex2dec: invalid conversion");
        else
          d (i) = tmp;
        endif
      else
        error ("hex2dec: argument must be a string of hexadecimal digits");
      endif
    endfor
  else
    error ("hex2dec: expecting a string argument");
  endif

endfunction
