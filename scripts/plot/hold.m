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
## @deftypefn {Function File} {} hold @var{args}
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

function hold (varargin)

  [h, varargin] = __plt_get_axis_arg__ ("hold", varargin{:});

  hold_state = get (h, "nextplot");

  nargs = numel (varargin);

  if (nargs == 0)
    if (strcmp (hold_state, "add"))
      hold_state = "replace";
    else
      hold_state = "add";
    endif
  elseif (nargs == 1)
    state = varargin{1};
    if (ischar (state))
      if (strcmp ("off", state))
	hold_state = "replace";
      elseif (strcmp ("on", state))
	hold_state = "add";
      else
	print_usage ();
      endif
    endif
  else
    print_usage ();
  endif

  set (h, "nextplot", hold_state);

endfunction
