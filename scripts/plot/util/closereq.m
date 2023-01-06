########################################################################
##
## Copyright (C) 2005-2023 The Octave Project Developers
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
## @deftypefn {} {} closereq ()
## Close the current figure and delete all graphics objects associated with it.
##
## By default, the @qcode{"closerequestfcn"} property of a new plot figure
## points to this function.
## @seealso{close, delete}
## @end deftypefn

function closereq ()

  ## Get current figure, but don't use gcf to avoid creating a new figure.
  cf = get (0, "currentfigure");
  if (isfigure (cf))
    delete (cf);
  endif

endfunction


## No BIST tests required.  Testing done in close.m.
%!assert (1)
