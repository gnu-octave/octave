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
## @deftypefn {Function File} {} dec2hex (@var{n})
## Return the hexadecimal number corresponding to the nonnegative decimal
## number @var{n}, as a string.  For example,
##
## @example
## dec2hex (2748)
##      @result{} "abc"
## @end example
## @end deftypefn

## Author: Kurt Hornik <Kurt.Hornik@ci.tuwien.ac.at>
## Adapted-By: jwe

function h = dec2hex (d)

  if (nargin != 1)
    usage ("dec2hex (d)");
  endif

  [nr, nc] = size (d);

  len = nr * nc;

  d = reshape (d, 1, len);

  eleo = empty_list_elements_ok;
  unwind_protect
    empty_list_elements_ok = 1;
    h = "";
    for i = 1:len
      tmp = d (i);
      if (tmp == round (tmp))
        h = sprintf ("%s%x", h, tmp);
      else
        error ("dec2hex: invalid conversion");
      endif
    endfor
  unwind_protect_cleanup
    empty_list_elements_ok = eleo;
  end_unwind_protect

endfunction
