## Copyright (C) 1996, 1997 John W. Eaton
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
## @deftypefn {Function File} {} texas_lotto ()
## Pick 6 unique numbers between 1 and 50 that are guaranteed to win
## the Texas Lotto.
## @end deftypefn
##
## @seealso{rand}

## Author: jwe

function picks = texas_lotto ()

  if (nargin != 0)
    warning ("win_texas_lotto: ignoring extra arguments");
  endif

  picks = zeros (1,6);
  picks (1) = round (50-49*(1-rand));
  n = 2;
  while (n < 7)
    tmp = round (50-49*(1-rand));
    equal = 0;
    for i = 1:n
      if (tmp == picks (i))
        equal = 1;
        break;
      endif
    endfor
    if (! equal)
      picks (n) = tmp;
      n++;
    endif
  endwhile

  picks = sort (picks);

endfunction
