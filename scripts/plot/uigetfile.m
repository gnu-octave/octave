## Copyright (C) 2010 Kai Habel
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
## @deftypefn {Function File} {[@var{fname}, @var{fpath}, @var{fltidx}] =} uigetfile (@var{flt})
## @deftypefnx  {Function File} {[@dots{}] =} uigetfile (@var{flt}, @var{dialog_name}, @var{default_file})
## @deftypefnx {Function File} {[@dots{}] =} uigetfile (@var{flt}, @var{dialog_name})
## @deftypefnx {Function File} {[@dots{}] =} uigetfile (@dots{},"Position",[@var{px}, @var{py}])
## @deftypefnx {Function File} {[@dots{}] =} uigetfile (@dots{},"Multiselect",@var{mode})
##
## Open a GUI dialog to select a file. It returns the filename @var{fname},
## the path to this file @var{fpath} and the filter index @var{fltidx}.
## @var{flt} contains a (list of) file filter string(s) in one of the following
## formats:
##
## @table @code
## @item "/path/to/filename.ext"
## If a filename is given the file extension is extracted and used as filter.
## In addtion the path is selected as current path and the filname is selected
## as default file.
## Example: uigetfile("myfun.m");
##
## @item A single file extension "*.ext". 
## Example: uigetfile("*.ext");
##
## @item A 2-column cell array,
## containing the file extension in the 1st column and a brief description
## in the 2nd column.
## Example: uigetfile(@{"*.ext","My Description";"*.xyz","XYZ-Format"@});
## 
## The filter string can also contain a semicolon separated list of filter
## extensions.
## Example: uigetfile(@{"*.gif;*.png;*.jpg", "Supported Picture Formats"@});
## @end table
##
## @var{dialog_name} can be used to customize the dialog title.
## If @var{default_file} is given it is selected in the GUI dialog. 
## If in addtion a path is given it is also used as current path.
## 
## The screen position of the GUI dialog can be set by using the "Position" key
## and a 2-valued vector containing the pixel coordinates.
## Two or more files can be selected when setting the "Multiselect" key to "on".
## In that case @var{fname} is a cell array containing the files. 
## @end deftypefn

## Author: Kai Habel

function [retfile, retpath, retindex] = uigetfile (varargin)

  if (nargin <= 7)

    defaultvals = {"All Files(*)", #FLTK File Filter
                   "Open File?",   #Dialog Title
                   "",             #FLTK default file name
                   [240, 120],     #Dialog Position (pixel x/y)
                   "off"};         #Multiselect on/off

    outargs = cell (5, 1);
    for i = 1 : 5
      outargs{i} = defaultvals{i};
    endfor

    idx1 = idx2 = [];
    if (length (varargin) > 0)
      for i = 1 : length (varargin)
        val = varargin{i};
        if (ischar (val))
          if (strncmp (tolower (val), "multiselect", 11))
            idx1 = i;
          elseif (strncmp(tolower (val), "position", 8))
            idx2 = i;
          endif
        endif
      endfor
    endif

    stridx = [idx1, idx2, 0];
    if (length (stridx) > 1)
      stridx = min (stridx(1 : end - 1));
    endif

    args = varargin;
    if (stridx)
      args = varargin(1 : stridx - 1);
    endif

    len = length (args);
    if (len > 0)
      file_filter = args{1};
      outargs{1} = __fltk_file_filter__ (file_filter);
      if (ischar (file_filter))
        outargs{3} = file_filter;
      endif
    endif
    
    if (len > 1)
      outargs{2} = args{2};
    endif

    if (len > 2)
      outargs{3} = args{3};
    endif

    if (stridx)
      ## we have string arguments ("position" or "multiselect")

      ## check for even number of remaining arguments, prop/value pair(s)
      if (rem (nargin - stridx + 1, 2))
        error ("expecting property/value pairs");
      endif

      for i = stridx : 2 : nargin
        prop = varargin{i};
        val = varargin{i + 1};
        if (strncmp (tolower (prop), "position", 8)) 
          if (ismatrix (val) && length(val) == 2)
            outargs{4} = val;
          else
            error ("expecting 2-element vector for position argument")
          endif
        elseif (strncmp (tolower (prop), "multiselect", 11))
          if (ischar (val))
            outargs{5} = tolower (val);
          else
            error ("expecting string argument (on/off) for multiselect")
          endif
        else
          error ("unknown argument");
        endif
      endfor
    endif
  else
    error ("number of input arguments must be less than eight");
  endif

  if (any (cellfun(@(x)strcmp (x, "fltk"), available_backends)))
    [retfile, retpath, retindex] = __fltk_uigetfile__ (outargs{:});  
  else
    error ("uigetfile requires fltk backend.");
  endif

         
endfunction

%!demo 
%! uigetfile({"*.gif;*.png;*.jpg", "Supported Picture Formats"})
