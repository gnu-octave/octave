## Copyright (C) 2010-2011 Kai Habel
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
## @deftypefn  {Function File} {[@var{fname}, @var{fpath}, @var{fltidx}] =} uiputfile ()
## @deftypefnx {Function File} {[@var{fname}, @var{fpath}, @var{fltidx}] =} uiputfile (@var{flt})
## @deftypefnx {Function File} {[@var{fname}, @var{fpath}, @var{fltidx}] =} uiputfile (@var{flt}, @var{dialog_name})
## @deftypefnx {Function File} {[@var{fname}, @var{fpath}, @var{fltidx}] =} uiputfile (@var{flt}, @var{dialog_name}, @var{default_file})
## Open a GUI dialog for selecting a file.  @var{flt} contains a (list of) file
## filter string(s) in one of the following formats:
##
## @table @code
## @item "/path/to/filename.ext"
## If a filename is given the file extension is
## extracted and used as filter.
## In addition the path is selected as current path and the filename is selected
## as default file.
## Example: uiputfile("myfun.m");
##
## @item "*.ext"
## A single file extension.
## Example: uiputfile("*.ext");
##
## @item @{"*.ext","My Description"@}
## A 2-column cell array containing the file extension in the 1st column and
## a brief description in the 2nd column.
## Example: uiputfile(@{"*.ext","My Description";"*.xyz","XYZ-Format"@});
## @end table
##
## The filter string can also contain a semicolon separated list of filter
## extensions.
## Example: uiputfile(@{"*.gif;*.png;*.jpg", "Supported Picture Formats"@});
##
## @var{dialog_name} can be used to customize the dialog title.
## If @var{default_file} is given it is preselected in the GUI dialog.
## If, in addition, a path is given it is also used as current path.
## @end deftypefn

## Author: Kai Habel

function [retfile, retpath, retindex] = uiputfile (varargin)

  if (exist("__fltk_uigetfile__") != 3)
    error ("uiputfile: fltk graphics toolkit required");
  endif

  if (nargin > 3)
    print_usage ();
  endif

  defaultvals = {"All Files(*)", #FLTK File Filter
                 "Save File?",   #Dialog Title
                 pwd,            #FLTK default file name
                 [240, 120],     #Dialog Position (pixel x/y)
                 "create"};

  outargs = cell(5, 1);
  for i = 1 : 5
    outargs{i} = defaultvals{i};
  endfor

  if (nargin > 0)
    file_filter = varargin{1};
    outargs{1} = __fltk_file_filter__ (file_filter);
    if (ischar (file_filter))
      outargs{3} = file_filter;
    endif
  endif

  if (nargin > 1)
    outargs{2} = varargin{2};
  endif

  if (nargin > 2)
    outargs{3} = varargin{3};
  endif

  [retfile, retpath, retindex] = __fltk_uigetfile__ (outargs{:});

endfunction

%!demo
%! uiputfile({"*.gif;*.png;*.jpg", "Supported Picture Formats"})

## Remove from test statistics.  No real tests possible.
%!test
%! assert (1);
