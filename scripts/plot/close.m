## Copyright (C) 2002 John W. Eaton
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

## -*- texinfo -*-
## @deftypefn {Command} {} close
## @deftypefnx {Command} {} close all
## Close the plot window(s).
## @end deftypefn

## Author: jwe

## PKG_ADD: mark_as_command close

function retval = close (arg1, arg2)

  static warned_all = false;
  static warned_name = false;
  static warned_handle = false;

  if (nargin == 0)
    if (! warned_all)
      warned_all = true;
      warning ("close: unable to close only current plot window");
    endif
    closeplot;
  elseif (nargin == 1)
    if (ischar (arg1))
      if (strcmp (arg1, "all"))
	closeplot;
      else
	if (! warned_name)
	  warned_name = true;
	  warning ("close: unable to close plot windows by name");
	endif
      endif
    else
      if (! warned_handle)
	warned_handle = true;
	warning ("close: unable to close plot windows by handle");
      endif
    endif
  elseif (nargin == 2
	  && ischar (arg1) && strcmp (arg1, "all")
	  && ischar (arg2) && strcmp (arg2, "hidden"))
    closeplot;
  else
    usage ("close [all]");
  endif

  if (nargout > 0)
    retval = 1;
  endif

endfunction
