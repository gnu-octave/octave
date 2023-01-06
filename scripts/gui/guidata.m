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
## @deftypefn  {} {@var{data} =} guidata (@var{h})
## @deftypefnx {} {} guidata (@var{h}, @var{data})
## Query or set user-custom GUI data.
##
## The GUI data is stored in the figure handle @var{h}.  If @var{h} is not a
## figure handle then it's parent figure will be used for storage.
##
## @var{data} must be a single object which means it is usually preferable
## for it to be a data container such as a cell array or struct so that
## additional data items can be added easily.
##
## @seealso{getappdata, setappdata, get, set, getpref, setpref}
## @end deftypefn

function dataout = guidata (h, data)

  if (nargin < 1)
    print_usage ();
  endif

  if (! ishghandle (h))
    error ("guidata: H must be a valid object handle");
  endif
  h = ancestor (h, "figure");
  if (isempty (h))
    error ("guidata: no ancestor figure of H found");
  endif

  if (nargin == 1)
    dataout = get (h, "__guidata__");
  else
    set (h, "__guidata__", data);
    if (nargout == 1)
      dataout = data;
    endif
  endif

endfunction


## Test input validation
%!error <Invalid call> guidata ()
%!error <H must be a valid object handle> guidata ({1})
%!error <no ancestor figure of H found> guidata (0)
