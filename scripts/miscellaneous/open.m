########################################################################
##
## Copyright (C) 2014-2023 The Octave Project Developers
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
## @deftypefn  {} {} open @var{file}
## @deftypefnx {} {@var{output} =} open (@var{file})
## Open the file @var{file} in Octave or in an external application based on
## the file type as determined by the filename extension.
##
## By default, recognized file types are
##
## @table @code
## @item .m
## Open file in the editor.  No @var{output} value is returned.
##
## @item  .mat
## @itemx octave-workspace
## Open the data file with @code{load}.  If no return value @var{output}
## is requested, variables are loaded in the base workspace.  Otherwise
## @var{output} will be a structure containing loaded data.
## @xref{XREFload,,load function}.
##
## @item .ofig
## Open the figure with @code{hgload}.
## @xref{XREFhgload,,hgload function}.
##
## @item .fig, .ofig
## Load the figure
##
## @item .exe
## Execute the program (on Windows systems only).  No @var{output} value
## is returned.
## @end table
##
## Custom file extensions may also be handled if a function @code{openxxx},
## where @code{xxx} is the extension, is found in the load path.  The function
## must accept the file name as input.  For example, in order to load
## @nospell{@qcode{".dat"}} data files in the base workspace, as is done by
## default for @qcode{".mat"} files, one may define
## @nospell{@qcode{"opendat.m"}} with the following contents:
##
## @example
## @group
## function retval = opendat (fname)
##   evalin ("base", sprintf ("load ('%s');", fname));
## endfunction
## @end group
## @end example
##
## Other file types are opened in the appropriate external application.
## @end deftypefn

function output = open (file)

  if (nargin < 1)
    print_usage ();
  endif

  if (! ischar (file))
    error ("open: FILE must be a string");
  endif

  if (! exist (file, "file"))
    error ("open: unable to find file %s", file);
  endif

  file = make_absolute_filename (tilde_expand (file));

  [~, fname, ext] = fileparts (file);

  if (! isempty (ext)
      && any (exist (["open" tolower(ext(2:end))]) == [2 3 5 103]))
    try
      openfcn = ["open" tolower(ext(2:end))];
      if (nargout > 0)
        output = feval (openfcn, file);
      else
        feval (openfcn, file);
      endif
    catch
      error ("open: %s", lasterr);
    end_try_catch
  elseif (strcmpi (ext, ".m"))
    edit (file);
  elseif (strcmpi (ext, ".mat") || strcmp (fname, "octave-workspace"))
    if (nargout > 0)
      output = load (file);
    else
      evalin ("base", sprintf ("load ('%s');", file));
    endif
  elseif (strcmpi (ext, ".ofig"))
    if (nargout > 0)
      output = openfig (file);
    else
      openfig (file);
    endif
  elseif (any (strcmpi (ext, {".mdl", ".slx", ".prj"})))
    error ("open: opening file type '%s' is not supported", ext);
  elseif (strcmpi (ext, ".exe"))
    if (ispc ())
      dos (file);
    else
      error ("open: executing .exe files is only supported on Windows systems");
    endif
  else
    __open_with_system_app__ (file);
  endif

endfunction


## Test input validation
%!error <Invalid call> open ()
%!error open ("abc", "def")
%!error <FILE must be a string> open (1)
