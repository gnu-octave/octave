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
## @deftypefn {Built-in Function} {} hold @var{args}
## Tell Octave to `hold' the current data on the plot when executing
## subsequent plotting commands.  This allows you to execute a series of
## plot commands and have all the lines end up on the same figure.  The
## default is for each new plot command to clear the plot device first.
## For example, the command
##
## @example
## hold on
## @end example
##
## @noindent
## turns the hold state on.  An argument of @code{"off"} turns the hold
## state off, and @code{hold} with no arguments toggles the current hold
## state.
## @end deftypefn

## PKG_ADD: mark_as_command hold

function hold (arg)

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
    __hold_state__(__current_figure__) = ! __hold_state__(__current_figure__);
  elseif (nargin == 1)
    if (strcmp (arg, "on"))
      __hold_state__(__current_figure__) = true;
    elseif (strcmp (arg, "off"))
      __hold_state__(__current_figure__) = false;
    else
      print_usage ();
    endif
  else
    print_usage ();
  endif

endfunction
