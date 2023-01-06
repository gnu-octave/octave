########################################################################
##
## Copyright (C) 2012-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{h} =} gco ()
## @deftypefnx {} {@var{h} =} gco (@var{hfig})
## Return a handle to the current object of the current figure, or a handle
## to the current object of the figure with handle @var{hfig}.
##
## The current object of a figure is the object that was last clicked on.  It
## is stored in the @qcode{"CurrentObject"} property of the target figure.
##
## If the last mouse click did not occur on any child object of the figure,
## then the current object is the figure itself.
##
## If no mouse click occurred in the target figure, this function returns an
## empty matrix.
##
## Programming Note: The value returned by this function is not necessarily the
## same as the one returned by @code{gcbo} during callback execution.  An
## executing callback can be interrupted by another callback and the current
## object may be changed.
##
## @seealso{gcbo, gca, gcf, gcbf, get, set}
## @end deftypefn

function h = gco (hfig)

  if (nargin == 1)
    if (! isfigure (hfig))
      error ("gco: HFIG must be a graphics handle to a figure object");
    endif
  else
    hfig = get (0, "currentfigure");
  endif

  h = get (hfig, "currentobject");

endfunction


## Test input invalidation
%!error <HFIG must be a graphics handle> gco (-1)
