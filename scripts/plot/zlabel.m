## Copyright (C) 1996 John W. Eaton
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

## usage: zlabel (text)
##
## Defines a label for the z-axis of a plot.  The label will appear the
## next time a plot is displayed.
##
## See other plotting commands also.

## Author: Vinayak Dutt <Dutt.Vinayak@mayo.EDU>
## Created: 3 July 95
## Adapted-By: jwe

function zlabel (text)

  if (nargin != 1)
    usage ("zlabel (text)");
  endif

  if (isstr (text))
    eval (sprintf ("set zlabel \"%s\"", text));
  else
    error ("error: zlabel: text must be a string");
  endif

endfunction
