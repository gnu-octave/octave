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
## @deftypefn {} {} uiresume (@var{h})
## Resume program execution suspended with @code{uiwait}.
##
## The handle @var{h} must be the same as the on specified in @code{uiwait}.
## If the handle is invalid or there is no @code{uiwait} call pending for the
## figure with handle @var{h}, this function does nothing.
## @seealso{uiwait}
## @end deftypefn

function uiresume (h)

  if (nargin < 1)
    h = gcf ();
  endif

  if (! isfigure (h))
    error ("uiresume: invalid figure handle H");
  endif

  try
    uiwait_state = get (h, "__uiwait_state__");
    if (strcmp (uiwait_state, "active"))
      set (h, "__uiwait_state__", "triggered");
    endif
  catch
    ## Ignore exception
  end_try_catch

endfunction
