## Copyright (C) 2012 Michael Goffioul
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
## @deftypefn  {Function File} {@var{data} =} guidata (@var{h})
## @deftypefnx {Function File} {} guidata (@var{h}, @var{data})
## Query or set user-custom GUI data.
##
## The GUI data is stored in the figure handle @var{h}.  If @var{h} is not a
## figure handle then it's parent figure will be used for storage.
##
## @var{data} must be a single object which means it is usually preferable
## for it to be a data container such as a cell array or struct.
##
## @seealso{getappdata, setappdata, get, set, getpref, setpref}
## @end deftypefn

## Author: goffioul

function varargout = guidata (varargin)

  if (nargin == 1 || nargin == 2)
    h = varargin{1};
    if (ishandle (h))
      h = ancestor (h, "figure");
      if (! isempty (h))
        if (nargin == 1)
          varargout{1} = get (h, "__guidata__");
        else
          data = varargin{2};
          set (h, "__guidata__", data);
          if (nargout == 1)
            varargout{1} = data;
          endif
        endif
      else
        error ("no ancestor figure found");
      endif
    else
      error ("invalid object handle");
    endif
  else
    print_usage ();
  endif

endfunction
