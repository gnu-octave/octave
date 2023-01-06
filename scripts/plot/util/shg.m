########################################################################
##
## Copyright (C) 1994-2023 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.
##
########################################################################

## -*- texinfo -*-
## @deftypefn {} {} shg
## Show the graph window.
##
## This function makes the current figure visible, and places it on top of
## of all other plot windows.
##
## Programming Note: @code{shg} is equivalent to @code{figure (gcf)} assuming
## that a current figure exists.
## @seealso{figure, drawnow, gcf}
## @end deftypefn

function shg ()

  if (nargin != 0)
    warning ("shg: ignoring extra arguments");
  endif

  hf = get (0, "currentfigure");
  if (! isempty (hf))
    set (hf, "visible", "on");
    __show_figure__ (hf);
  endif

endfunction
