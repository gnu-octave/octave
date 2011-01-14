## Copyright (C) 2005-2011 John W. Eaton
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
## @deftypefn {Function File} {} ishold
## Return true if the next plot will be added to the current plot, or
## false if the plot device will be cleared before drawing the next plot.
## @seealso{hold}
## @end deftypefn

function retval = ishold (h)

  if (nargin == 0)
    ax = gca ();
    fig = gcf ();
  elseif (nargin == 1)
    if (ishandle (h))
      if (isfigure (h))
        ax = get (h, "currentaxes");
        if (isempty (ax))
          ax = __go_axes__ (h);
          set (h, "currentaxes", ax);
        endif
        fig = h;
      elseif (strcmpi (get (h, "type"), "axes"))
        ax = h;
        fig = get (h, "parent");
      else
        error ("ishold: expecting argument to be axes or figure graphics handle");
      endif
    else
      error ("ishold: expecting argument to be axes or figure graphics handle");
    endif
  else
    print_usage ();
  endif

  retval = (strcmpi (get (fig, "nextplot"), "add")
            && strcmpi (get (ax, "nextplot"), "add"));

endfunction
