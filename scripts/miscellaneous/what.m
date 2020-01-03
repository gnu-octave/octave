## Copyright (C) 2007-2019 David Bateman
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
## Compatibility Note: Octave does not support mdl, slx, and p files; nor does
## it support package directories.  @code{what} will always return an empty
## list for these categories.
## @seealso{which, ls, exist}
## @end deftypefn

function retval = what (dir)

  if (nargin > 1)
    print_usage ();
  endif

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
     w(i) = __what__ (dir{i});
   endfor

   ## If none was found, return an empty struct
   if (numel (dir) == 0)
     w = __what__ ("");
     w = resize (w, [0, 1]);  # Matlab compatibility, return 0x1 empty array
   end

  if (nargout == 0)
    for i = 1 : numel (w)
      __display_filenames__ ("M-files in directory", w(i).path, w(i).m);
      __display_filenames__ ("\nMAT-files in directory", w(i).path, w(i).mat);
      __display_filenames__ ("\nMEX-files in directory", w(i).path, w(i).mex);
      __display_filenames__ ("\nOCT-files in directory", w(i).path, w(i).oct);
      __display_filenames__ ("\nClasses in directory", w(i).path, w(i).classes);
      __display_filenames__ ("\nPackages in directory", w(i).path, w(i).packages);
    endfor
  else
    retval = w;
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

  for i = 1 : length (files)
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
    elseif (strcmp (e, mexext ()))
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
function __display_filenames__ (msg, p, f)

  if (length (f) > 0)
    printf ("%s %s:\n\n", msg, p);

    maxlen = max (cellfun ("length", f));
    ncols = max (1, floor (terminal_size ()(2) / (maxlen + 3)));
    fmt = sprintf ("   %%-%ds", maxlen);
    fmt = repmat (fmt, [1, ncols]);
    fmt = [fmt "\n"];

    nrows = ceil (length (f) / ncols);
    for i = 1 : nrows
      args = f(i:nrows:end);
      if (length (args) < ncols)
        args(end+1 : ncols) = {""};
      endif
      printf (fmt, args{:});
    endfor
  endif

endfunction


%!test
%! w = what ();
%! assert (w.path, pwd);
%! assert (fieldnames (w), {"path"; "m"; "mat"; "mex"; "oct"; "mdl"; "slx";
%!                          "p"; "classes"; "packages"});

%!error what (1, 2)
