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

## usage:  dezero (s)
##
## Remove trailing blank entries and all zero entries from the string s.

## Author: Kurt Hornik <Kurt.Hornik@ci.tuwien.ac.at>
## Adapted-By: jwe
## Adapted from deblank by A. S. Hodel (a.s.hodel@eng.auburn.edu)
## 	(the name dezero is a reference to the Fermilab D0 experiment,
##      where my sister did her PhD research) 
## $Revision: 1.1.1.1 $
## $Log: dezero.m,v $
## Revision 1.1.1.1  1998/05/19 20:24:13  jwe
##
## Revision 1.3  1997/03/11 14:42:41  scotte
## fixed implicit_str_to_num_ok bug a.s.hodel@eng.auburn.edu
##
## Revision 1.2  1997/03/03 22:52:20  hodel
## fixed problem with conversion to/from numerical value
## a.s.hodel@eng.auburn.edu
##
## Revision 1.1  1997/02/12 11:34:56  hodel
## Initial revision
##
## Revision 1.3  1997/02/07 15:24:35  scotte
## fixed to remove all null characters, then call deblank
##

function t = dezero (s)

  if (nargin != 1)
    usage ("dezero (s)");
  elseif (isstr (s))

    save_val = implicit_str_to_num_ok;
    implicit_str_to_num_ok = 1;

    #disp("dezero: entry, s=")
    #s
    #disp("/dezero")

    [nr, nc] = size (s);
    len = nr * nc;

    if (len == 0)
      t = s;
    else

      #disp("dezero: 1, s=")
      #s
      #disp("/dezero")

      s = reshape (s, 1, len);

      #disp("dezero: 2, s=")
      #s
      #disp("/dezero")

      # need to remove zeros first, then call deblank
      s = 1*s;
      t = deblank(setstr(s(find(s != 0) )));
    endif

    implicit_str_to_num_ok = save_val;

  else
    error ("dezero: expecting string argument");
  endif

endfunction
