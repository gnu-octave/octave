## Copyright (C) 1997 John W. Eaton
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

## usage: p = path (...)
##
## Modify or display Octave's LOADPATH.
##
## If nargin and nargout are 0, display the elements of Octave's
## LOADPATH in an easy to read format.
##
## If nargin is zero and nargout is greater than zero, return the
## current value of LOADPATH.
##
## If nargin is greater than zero, concatenate the arguments, separating
## them with ":".  Set LOADPATH to the result and also return it.
##
## No checks are made for duplicate elements.

## Author: jwe

function p = path (...)

  if (nargin == 0)
    if (nargout == 0)
      stdout << "\nLOADPATH contains the following directories:\n\n  ";
      stdout << strrep (LOADPATH, ":", "\n  ") << "\n\n";
    else    
      p = LOADPATH;
    endif
  else
    p = va_arg ();
    for i = 2:nargin
      p = sprintf ("%s:%s", p, va_arg ());
    endfor
    LOADPATH = p;
  endif

endfunction
