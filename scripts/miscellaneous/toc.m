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

function secs = toc ()

  ## usage: toc
  ##
  ## Return the difference between the current wall-clock time and the
  ## time that the function tic () was last called, in seconds.
  ##
  ## See also: tic, clock, etime, cputime

  if (nargin != 0)
    warning ("toc: ignoring extra arguments");
  endif

  global _time_tic_called;

  if (exist ("_time_tic_called"))
    secs = etime (clock (), _time_tic_called);
  else
    warning ("toc called before timer set");
    secs = [];
  endif

endfunction
