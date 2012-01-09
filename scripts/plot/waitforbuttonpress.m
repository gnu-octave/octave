## Copyright (C) 2004-2012 Petr Mikulik
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
## @deftypefn {Function File} {@var{b} =} waitforbuttonpress ()
## Wait for button or mouse press.over a figure window.  The value of
## @var{b} returns 0 if a mouse button was pressed or 1 is a key was
## pressed.
## @seealso{ginput}
## @end deftypefn

## The original version of this code bore the copyright
## Author: Petr Mikulik
## License: public domain

function a = waitforbuttonpress ()

  if (nargin != 0 || nargout > 1)
    print_usage ();
  endif

  [x, y, k] = ginput (1);

  if (nargout == 1)
    if (k <= 5)
      a = 0;
    else
      a = 1;
    endif
  endif

endfunction
