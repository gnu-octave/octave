## Copyright (C) 2010-2012 Kai Habel
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

  defaulttoolkit = get (0, "defaultfigure__graphics_toolkit__");
  funcname = ["__uiputfile_", defaulttoolkit, "__"];
  functype = exist (funcname);
  if (! __is_function__ (funcname))
    funcname = "__uiputfile_fltk__";
    if (! __is_function__ (funcname))
      error ("uiputfile: fltk graphics toolkit required");
    elseif (! strcmp (defaulttoolkit, "gnuplot"))
      warning ("uiputfile: no implementation for toolkit '%s', using 'fltk' instead",
               defaulttoolkit);
    endif
  endif

  if (nargin > 3)
    print_usage ();
  endif

  defaultvals = {cell(0, 2),     # File Filter
                 "Save File",    # Dialog Title
                 "",             # Default file name
                 [240, 120],     # Dialog Position (pixel x/y)
                 "create",
                 pwd};           # Default directory

  outargs = cell(6, 1);
  for i = 1 : 6
    outargs{i} = defaultvals{i};
  endfor

  if (nargin > 0)
    file_filter = varargin{1};
    [outargs{1}, outargs{3}, defdir] = __file_filter__ (file_filter);
    if (length (defdir) > 0)
      outargs{6} = defdir;
    endif
  else
    outargs{1} = __file_filter__ (outargs{1});
  endif

  if (nargin > 1)
    if (ischar (varargin{2}))
      outargs{2} = varargin{2};
    elseif (! isempty (varargin{2}))
      print_usage ();
    endif
  endif

  if (nargin > 2)
    if (ischar (varargin{3}))
      [fdir, fname, fext] = fileparts (varargin{3});
      if (! isempty (fdir))
        outargs{6} = fdir;
      endif
      if (! isempty (fname) || ! isempty (fext))
        outargs{3} = strcat (fname, fext);
      endif
    elseif (! isempty (varargin{3}))
      print_usage ();
    endif
  endif

  [retfile, retpath, retindex] = feval (funcname, outargs{:});

endfunction

%!demo
%! uiputfile({"*.gif;*.png;*.jpg", "Supported Picture Formats"})

## Remove from test statistics.  No real tests possible.
%!test
%! assert (1);
