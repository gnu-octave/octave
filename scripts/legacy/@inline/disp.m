########################################################################
##
## Copyright (C) 2017-2023 The Octave Project Developers
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
## @deftypefn {} {} disp (@var{fobj})
## Display an @code{inline} function object.
## @seealso{display}
## @end deftypefn

function disp (fobj)

  disp ("inline function object:");
  ## FIXME: inputname doesn't work with @class methods
  ## str = inputname (1);
  ## args = strjoin (fobj.args, ',');
  ## str = [str '(' args ')' " = " fobj.expr];
  ## disp (str);
  disp (formula (fobj));

endfunction
