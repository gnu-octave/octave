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
## @deftypefn {} {@var{value} =} subsref (@var{fobj}, @var{idx})
## Perform subscripted function call on the inline function object @var{fobj}.
## @end deftypefn

function retval = subsref (fobj, idx)

  if (nargin != 2)
    print_usage ();
  endif

  if (isempty (idx))
    error ("@inline/subsref: missing index");
  endif

  if (strcmp (idx(1).type, "()"))
    args = idx.subs;
    if (numel (args) > 0)
      retval = feval (fobj, args{:});
    else
      retval = feval (fobj);
    endif
  else
    error ("@inline/subsref: invalid subscript type");
  endif

endfunction
