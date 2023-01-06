########################################################################
##
## Copyright (C) 2010-2023 The Octave Project Developers
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
## @deftypefn  {} {[@var{fname}, @var{fpath}, @var{fltidx}] =} uigetfile ()
## @deftypefnx {} {[@dots{}] =} uigetfile (@var{flt})
## @deftypefnx {} {[@dots{}] =} uigetfile (@var{flt}, @var{dialog_name})
## @deftypefnx {} {[@dots{}] =} uigetfile (@var{flt}, @var{dialog_name}, @var{default_file})
## @deftypefnx {} {[@dots{}] =} uigetfile (@dots{}, "MultiSelect", @var{mode})
##
## Open a GUI dialog for selecting a file and return the filename @var{fname},
## the path to this file @var{fpath}, and the filter index @var{fltidx}.
##
## @var{flt} contains a (list of) file filter string(s) in one of the following
## formats:
##
## @table @asis
## @item @qcode{"/path/to/filename.ext"}
## If a filename is given then the file extension is extracted and used as
## filter.  In addition, the path is selected as current path in the dialog and
## the filename is selected as default file.
## Example: @code{uigetfile ("myfcn.m")}
##
## @item A single file extension @qcode{"*.ext"}
## Example: @code{uigetfile ("*.ext")}
##
## @item A 2-column cell array
## containing a file extension in the first column and a brief description in
## the second column.  Example:
## @code{uigetfile (@{"*.ext", "My Description";"*.xyz", "XYZ-Format"@})}
##
## The filter string can also contain a semicolon separated list of filter
## extensions.  Example:
## @code{uigetfile (@{"*.gif;*.png;*.jpg", "Supported Picture Formats"@})}
##
## @item A directory name or path name
## If the folder name of path name contains a trailing file separator, the
## contents of that folder will be displayed.  If no trailing file separator
## is present the parent directory is listed.  The substring to the right of
## the rightmost file separator (if any) will be interpreted as a file or
## directory name and if that file or directory exists it will be highlighted.
## If the path name or directory name is entirely or partly nonexistent, the
## current working directory will be displayed.
## No filter will be active.
## @end table
##
## @var{dialog_name} can be used to customize the dialog title.
##
## If @var{default_file} is given then it will be selected in the GUI dialog.
## If, in addition, a path is given it is also used as current path.
##
## Two or more files can be selected when setting the @qcode{"MultiSelect"} key
## to @qcode{"on"}.  In that case, @var{fname} is a cell array containing the
## files.
##
## The outputs @var{fname} and @var{fpath} are strings returning the chosen
## name and path, respectively.  However, if the @samp{Cancel} button is
## clicked the outputs are of type double with a value of @code{0}.
## @var{fltidx} is the index in the list of filter extensions @var{flt} that
## was selected.
##
## @seealso{uiputfile, uigetdir}
## @end deftypefn

function [retfile, retpath, retindex] = uigetfile (varargin)

  if (nargin > 7)
    error ("uigetfile: number of input arguments must be less than eight");
  endif

  ## Preset default values
  outargs = {cell(0, 2),         # File Filter
             "Open File",        # Dialog Title
             "",                 # Default filename
             "off",              # MultiSelect on/off
             pwd};               # Default directory

  idx1 = idx2 = [];
  has_opts = false;
  if (nargin > 0)
    idx1 = find (strcmpi (varargin, "multiselect"), 1);
    idx2 = find (strcmpi (varargin, "position"), 1);
    if (idx1 || idx2)
      has_opts = true;
    endif
  endif

  if (! isempty (idx2))
    ## FIXME: Remove handling the "position" property completely in Octave 9
    warning (['uigetfile: The "position" argument is deprecated and will ', ...
              "be ignored. Consider removing it from the function call."]);
  endif

  optidx = min ([idx1, nargin+1]);

  args = varargin(1:optidx-1);

  len = numel (args);
  if (len > 0)
    [outargs{1}, outargs{3}, defdir] = __file_filter__ ("uigetfile", args{1});
    if (! isempty (defdir))
      outargs{5} = defdir;
    endif
  else
    outargs{1} = __file_filter__ ("uigetfile", outargs{1});
  endif

  if (len > 1)
    if (ischar (args{2}))
      if (! isempty (args{2}))
        outargs{2} = args{2};
      endif
    elseif (! isempty (args{2}))
      print_usage ();
    endif
  endif

  if (len > 2)
    if (ischar (args{3}))
      if (isfolder (args{3}))
        fdir = args{3};
        fname = fext = "";
      else
        [fdir, fname, fext] = fileparts (varargin{3});
      endif
      if (! isempty (fdir))
        outargs{5} = fdir;
      endif
      if (! isempty (fname) || ! isempty (fext))
        outargs{3} = [fname fext];
      endif
    elseif (! isempty (args{3}))
      print_usage ();
    endif
  endif

  if (has_opts)
    ## string arguments ("position" or "multiselect")

    ## check for even number of remaining arguments, prop/value pair(s)
    if (rem (nargin - optidx + 1, 2))
      error ("uigetfile: PROPERTY/VALUE arguments must occur in pairs");
    endif

    for i = optidx : 2 : nargin
      prop = varargin{i};
      val = varargin{i + 1};
      if (strcmpi (prop, "position"))
        ## FIXME: Remove handling this option in Octave 9.
      elseif (strcmpi (prop, "multiselect"))
        if (! ischar (val))
          error ('uigetfile: MultiSelect value must be a string ("on"/"off")');
        endif
        outargs{4} = tolower (val);
      else
        error ("uigetfile: unknown argument '%s'", prop);
      endif
    endfor
  endif

  if (__event_manager_have_dialogs__ ())
    [retfile, retpath, retindex] = __event_manager_file_dialog__ (outargs{:});
  else
    funcname = __get_funcname__ (mfilename ());
    [retfile, retpath, retindex] = feval (funcname, outargs{:});
  endif

endfunction


%!demo
%! uigetfile ({'*.gif;*.png;*.jpg', 'Supported Picture Formats'});

## Remove from test statistics.  No real tests possible.
%!assert (1)
