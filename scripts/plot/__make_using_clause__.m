## Copyright (C) 2006 John W. Eaton
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

function usingstr = __make_using_clause__ (x)
  cols = columns (x);
  if (cols > 0)
    usingstr = strcat (gnuplot_command_using, " ($1)");
    for k = 2:cols
      usingstr = sprintf ("%s:($%d)", usingstr, k);
    endfor
  else
    usingstr = "";
  endif
endfunction
