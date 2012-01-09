## Copyright (C) 2006-2012 John W. Eaton
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {Function File} {} box (@var{arg})
## @deftypefnx {Function File} {} box (@var{h}, @dots{})
## Control the display of a border around the plot.
## The argument may be either @code{"on"} or @code{"off"}.  If it is
## omitted, the current box state is toggled.
## @seealso{grid}
## @end deftypefn

## Author: jwe

function box (varargin)

  h = gca ();

  box_state = get (h, "box");

  nargs = numel (varargin);

  if (nargs == 0)
    if (strcmpi (box_state, "on"))
      box_state = "off";
    else
      box_state = "on";
    endif
  elseif (nargs == 1)
    state = varargin{1};
    if (ischar (state))
      if (strcmpi (state, "off"))
        box_state = "off";
      elseif (strcmpi (state, "on"))
        box_state = "on";
      else
        print_usage ();
      endif
    endif
  else
    print_usage ();
  endif

  set (h, "box", box_state);

endfunction
