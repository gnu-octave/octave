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
## @deftypefn  {Function File} {@var{hdata} =} guihandles (@var{handle})
## @deftypefnx {Function File} {@var{hdata} =} guihandles
## @end deftypefn

## Author: goffioul

function hdata = guihandles (varargin)

  hdata = [];

  if (nargin == 0 || nargin == 1)
    if (nargin == 1)
      h = varargin{1};
      if (ishandle (h))
        h = ancestor (h, "figure");
        if (isempty (h))
          error ("no ancestor figure found");
        endif
      else
        error ("invalid object handle");
      endif
    else
      h = gcf ();
    endif
    hdata = __make_guihandles_struct__ (h, hdata);
  else
    print_usage ();
  endif

endfunction

function hdata = __make_guihandles_struct__ (h, hdata)

  tag = get (h, "tag");
  if (! isempty (tag))
    if (isfield (hdata, tag))
      hdata.(tag) = [hdata.(tag), h];
    else
      try
        hdata.(tag) = h;
      catch
      end_try_catch
    endif
  endif

  kids = allchild (h);
  for i = 1 : length (kids)
    hdata = __make_guihandles_struct__ (kids(i), hdata);
  endfor

endfunction
