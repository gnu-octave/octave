########################################################################
##
## Copyright (C) 2020-2023 The Octave Project Developers
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
## @deftypefn {} {@var{args} =} argnames (@var{fobj})
## Return a cell array of character strings containing the names of the
## arguments of the inline function object @var{fobj}.
##
## Programming Note: @code{symvar (@var{fobj})} is equivalent to
## @code{argnames (@var{fobj})}.
## @seealso{symvar, formula, vectorize, inline}
## @end deftypefn

function args = argnames (fobj)

  args = fobj.args;

endfunction
