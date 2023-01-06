########################################################################
##
## Copyright (C) 2003-2023 The Octave Project Developers
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
## @deftypefn {} {} beep ()
## Produce a beep from the speaker (or visual bell).
##
## This function sends the alarm character @qcode{"@backslashchar{}a"} to
## the terminal.  Depending on the user's configuration this may produce an
## audible beep, a visual bell, or nothing at all.
## @seealso{puts, fputs, printf, fprintf}
## @end deftypefn

function beep ()

  puts ("\a");

endfunction


%!error beep (1)
