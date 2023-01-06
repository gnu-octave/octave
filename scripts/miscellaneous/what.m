########################################################################
##
## Copyright (C) 2007-2023 The Octave Project Developers
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
## @deftypefn  {} {} what
## @deftypefnx {} {} what @var{dir}
## @deftypefnx {} {w =} what (@var{dir})
## List the Octave specific files in directory @var{dir}.
##
## If @var{dir} is not specified then the current directory is used.
##
## If a return argument is requested, the files found are returned in the
## structure @var{w}.  The structure contains the following fields:
##
## @table @asis
## @item path
## Full path to directory @var{dir}
##
## @item m
## Cell array of m-files
##
## @item mat
## Cell array of mat files
##
## @item mex
## Cell array of mex files
##
## @item oct
## Cell array of oct files
##
## @item mdl
## Cell array of mdl files
##
## @item slx
## Cell array of slx files
##
## @item p
## Cell array of p-files
##
## @item classes
## Cell array of class directories (@file{@@@var{classname}/})
##
## @item packages
## Cell array of package directories (@file{+@var{pkgname}/})
## @end table
##
## Compatibility Note: Octave does not support mdl, slx, and p files.
## @code{what} will always return an empty list for these categories.
## @seealso{which, ls, exist}
## @end deftypefn

function w = what (dir)

  if (nargin == 0)
    dir = { pwd() };
  else
    dtmp = canonicalize_file_name (dir);
    if (isempty (dtmp))
      dtmp = {};
    else
      dtmp = {dtmp};
    endif
    ## Search for directory name in path
    if (dir(end) == '/' || dir(end) == '\')
      dir(end) = [];
    endif
    dtmp = unique ([dtmp; dir_in_loadpath(dir, "all")]);

    if (isempty (dtmp) && nargout == 0)
      printf ("%s not found\n", dir);
      return;
    endif

    dir = dtmp;
  endif

   ## Lookup info for each directory
   for i = 1 : numel (dir)
     ws(i) = __what__ (dir{i});
   endfor

   ## If none was found, return an empty struct
   if (numel (dir) == 0)
     ws = __what__ ("");
     ws = resize (ws, [0, 1]);  # Matlab compatibility, return 0x1 empty array
   endif

  if (nargout == 0)
    for i = 1 : numel (ws)
      __print_fnames__ ("M-files in directory", ws(i).path, ws(i).m);
      __print_fnames__ ("\nMAT-files in directory", ws(i).path, ws(i).mat);
      __print_fnames__ ("\nMEX-files in directory", ws(i).path, ws(i).mex);
      __print_fnames__ ("\nOCT-files in directory", ws(i).path, ws(i).oct);
      __print_fnames__ ("\nClasses in directory", ws(i).path, ws(i).classes);
      __print_fnames__ ("\nPackages in directory", ws(i).path, ws(i).packages);
    endfor
  else
    w = ws;
  endif

endfunction


## what() functionality for a single directory
function w = __what__ (dir)

  files = readdir (dir);
  w.path = dir;
  w.m = cell (0, 1);
  w.mat = cell (0, 1);
  w.mex = cell (0, 1);
  w.oct = cell (0, 1);
  w.mdl = cell (0, 1);
  w.slx = cell (0, 1);
  w.p = cell (0, 1);
  w.classes = cell (0, 1);
  w.packages = cell (0, 1);

  for i = 1 : numel (files)
    nm = files{i};

    if (strcmp (nm, ".") || strcmp (nm, ".."))
      continue;   # Ignore . and ..
    endif

    ## mdl, slx, and p are ignored (no if test) since they are not implemented
    [~, f, e] = fileparts (nm);
    if (strcmp (e, ".m"))
      if (isfile (fullfile (dir, nm)))
        w.m{end+1} = nm;
      endif
    elseif (strcmp (e, ".mat"))
      if (isfile (fullfile (dir, nm)))
        w.mat{end+1} = nm;
      endif
    elseif (strcmp (e, ".oct"))
      if (isfile (fullfile (dir, nm)))
        w.oct{end+1} = nm;
      endif
    elseif (strcmp (e, ['.' mexext]))
      if (isfile (fullfile (dir, nm)))
        w.mex{end+1} = nm;
      endif
    elseif (nm(1) == "@")
      if (isfolder (fullfile (dir, nm)))
        w.classes{end+1} = nm;
      endif
    elseif (nm(1) == "+")
      if (isfolder (fullfile (dir, nm)))
        w.packages{end+1} = nm;
      endif
    endif

  endfor

endfunction


## Pretty print filenames to terminal
function __print_fnames__ (msg, p, f)

  if (! isempty (f))
    printf ("%s %s:\n\n", msg, p);
    printf ("%s", list_in_columns (f, 0, "   "));
  endif

endfunction


%!test
%! w = what ();
%! assert (w.path, pwd);
%! assert (fieldnames (w), {"path"; "m"; "mat"; "mex"; "oct"; "mdl"; "slx";
%!                          "p"; "classes"; "packages"});

## FIXME: Should have additional tests.  Possibly create a temporary directory
## within TMPDIR, create files and folders, and call what() on that dir.
%!error what (1, 2)
