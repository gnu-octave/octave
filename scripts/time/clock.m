### Copyright (C) 1996 John W. Eaton
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

function retval = clock ()

  ## usage: clock ()
  ##
  ## Return current date and time in vector with elements
  ##
  ##   [ year, month, day-of-month, hour, minute, second ]

  tm = localtime (time ());

  retval = zeros (1, 6);

  retval(1) = tm.year + 1900;
  retval(2) = tm.mon + 1;
  retval(3) = tm.mday;
  retval(4) = tm.hour;
  retval(5) = tm.min;
  retval(6) = tm.sec + tm.usec / 1e6;

endfunction
