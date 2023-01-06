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
## @deftypefn {} {@var{tf} =} isstring (@var{s})
## Return true if @var{s} is a string array.
##
## A string array is a data type that stores strings (row vectors of
## characters) at each element in the array.  It is distinct from character
## arrays which are N-dimensional arrays where each element is a single 1x1
## character.  It is also distinct from cell arrays of strings which store
## strings at each element, but use cell indexing @samp{@{@}} to access
## elements rather than string arrays which use ordinary array indexing
## @samp{()}.
##
## Programming Note: Octave does not yet implement string arrays so this
## function will always return false.
## @seealso{ischar, iscellstr, isfloat, isinteger, islogical, isnumeric, isa}
## @end deftypefn

function tf = isstring (s)

  if (nargin < 1)
    print_usage ();
  endif

  tf = false;

endfunction


%!assert (isstring ([]), false)
%!assert (isstring (1), false)
%!assert (isstring ('a'), false)
## FIXME: when string arrays are implemented, this should return true.
%!#assert (isstring ("b"), true)
%!assert (isstring ({'a'}), false)
%!assert (isstring ({"b"}), false)

%!error <Invalid call> isstring ()
%!error isstring ("a", "b")
