########################################################################
##
## Copyright (C) 2004-2023 The Octave Project Developers
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
## @deftypefn {} {@var{b} =} waitforbuttonpress ()
## Wait for mouse click or key press over the current figure window.
##
## The return value of @var{b} is 0 if a mouse button was pressed or 1 if a
## key was pressed.
## @seealso{waitfor, ginput, kbhit}
## @end deftypefn

## The original version of this code bore the copyright
## Author: Petr Mikulik
## License: public domain

function b = waitforbuttonpress ()

  [x, y, k] = ginput (1);

  if (nargout == 1)
    if (k <= 5)
      b = 0;
    else
      b = 1;
    endif
  endif

endfunction
