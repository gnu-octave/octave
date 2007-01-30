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
## @deftypefnx {Command} {} close (@var{n})
## @deftypefnx {Command} {} close all
## @deftypefnx {Command} {} close all hidden
## Close the plot window(s).
## @end deftypefn

## Author: jwe

## PKG_ADD: mark_as_command close

function retval = close (arg1, arg2)

  if (nargin == 0)
    ## Close current figure.
    figs = gcf ();
  elseif (nargin == 1)
    if (ischar (arg1) && strcmp (arg1, "all"))
      ## Close all figures.
      figs = __uiobject_figures__ ();
    elseif (isfigure (arg1))
      figs = arg1;
    else
      error ("close: expecting argument to be \"all\" or a figure handle");
    endif
  elseif (nargin == 2
	  && ischar (arg1) && strcmp (arg1, "all")
	  && ischar (arg2) && strcmp (arg2, "hidden"))
    figs = __uiobject_figures__ ();
  else
    print_usage ();
  endif

  for h = figs
    set (0, "currentfigure", h);
    feval (get (h, "closerequestfcn"));
  endfor

  if (nargout > 0)
    retval = 1;
  endif

endfunction
