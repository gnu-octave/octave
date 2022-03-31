########################################################################
##
## Copyright (C) 2020-2022 The Octave Project Developers
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
## @deftypefn {} {@var{fcnstr} =} char (@var{fcn})
## Return a character string representing the inline function @var{fcn}.
##
## Note that @code{char (@var{fcn})} is equivalent to
## @code{formula (@var{fcn})}.
## @seealso{char, argnames, inline, vectorize}
## @end deftypefn

function fcnstr = char (obj)

  if (nargin < 1)
    print_usage ();
  endif

  fcnstr = obj.expr;

endfunction
