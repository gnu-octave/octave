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

function setaudio (w_type, value)
  
  ## usage: setaudio ([w_type [, value]])
  ##
  ## executes the shell command `mixer [w_type [, value]]'

  ## Written by AW (Andreas.Weingessel@ci.tuwien.ac.at) on Oct 5, 1994
  ## Updated by AW on Nov 3, 1994
  ## Copyright Department of Probability Theory and Statistics TU Wien

  if (nargin == 0)
    system ("mixer");
  elseif (nargin == 1)
    system (sprintf ("mixer %s", w_type));
  elseif (nargin == 2)
    system (sprintf ("mixer %s %d", w_type, value));
  else
    usage ("setaudio ([w_type [, value]])");
  endif
  
endfunction
