########################################################################
##
## Copyright (C) 2013-2023 The Octave Project Developers
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
## @deftypefn {} {} preferences
## Display the GUI preferences dialog window for Octave.
## @end deftypefn

function preferences ()

  if (isguirunning ())
    __event_manager_show_preferences__ ();
  else
    warning ("preferences: GUI must be running to use preferences dialog");
  endif

endfunction


%!test
%! if (isguirunning ())
%!   return;
%! endif
%! fail ("preferences ()", "warning", "GUI must be running");
