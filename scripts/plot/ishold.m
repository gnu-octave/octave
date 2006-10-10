## Copyright (C) 2005 John W. Eaton
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
## @deftypefn {Built-in Function} {} ishold
## Return 1 if the next line will be added to the current plot, or 0 if
## the plot device will be cleared before drawing the next line.
## @end deftypefn

function retval = ishold ()

  global __current_figure__;
  global __hold_state__;

  if (isempty (__current_figure__))
    __current_figure__ = 1;
  endif

  if (isempty (__hold_state__))
    __hold_state__ = false;
  endif

  if (length (__hold_state__) < __current_figure__)
    __hold_state__(__current_figure__) = false;
  endif

  if (nargin == 0)
    retval = __hold_state__(__current_figure__);
  else
    print_usage ();
  endif

endfunction
