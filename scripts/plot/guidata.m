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
## @deftypefn  {Function File} {@var{data} =} guidata (@var{handle})
## @deftypefnx {Function File} {} guidata (@var{handle}, @var{data})
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
