## Copyright (C) 2007-2012 David Bateman
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
## @deftypefn  {Command} {} what
## @deftypefnx {Command} {} what @var{dir}
## @deftypefnx {Function File} {w =} what (@var{dir})
## List the Octave specific files in directory @var{dir}.  If @var{dir} is
## not specified then the current directory is used.  If a return argument is
## requested, the files found are returned in the structure @var{w}.
## @seealso{which}
## @end deftypefn

function ret = what (d)

  if (nargin == 0)
    d = pwd ();
  elseif (isempty (strfind (d, filesep ())))
    ## Find the appropriate directory on the path.
    p = strtrim (strsplit (path (), pathsep()));
    d = p{find (cellfun (@(x) ! isempty (strfind (x, d)), p))(end)};
  else
    [status, msg, msgid] = fileattrib (d);
    if (status != 1)
      error ("what: could not find the file or path %s", d);
    else
      d = msg.Name;
    endif
  endif

  files = dir (d);
  w.path = d;
  w.m = cell (0, 1);
  w.mex = cell (0, 1);
  w.oct = cell (0, 1);
  w.mat = cell (0, 1);
  w.mdl = cell (0, 1);
  w.p = cell (0, 1);
  w.classes = cell (0, 1);

  for i = 1 : length (files)
    n = files(i).name;
    ## Ignore . and ..
    if (strcmp (n, ".") || strcmp (n, ".."))
      continue;
    else
      ## Ignore mdl and p files
      [dummy, f, e] = fileparts (n);
      if (strcmp (e, ".m"))
        w.m{end+1} = n;
      elseif (strcmp (e, mexext ()))
        w.mex{end+1} = n;
      elseif (strcmp (e, ".oct"))
        w.oct{end+1} = n;
      elseif (strcmp (e, ".mat"))
        w.mat{end+1} = n;
      elseif(strcmp (n(1), "@"))
        w.classes{end+1} = n;
      endif
    endif
  endfor

  if (nargout == 0)
    __display_filenames__ ("M-files in directory", w.path, w.m);
    __display_filenames__ ("\nMEX-files in directory", w.path, w.mex);
    __display_filenames__ ("\nOCT-files in directory", w.path, w.oct);
    __display_filenames__ ("\nMAT-files in directory", w.path, w.mat);
    __display_filenames__ ("\nClasses in directory", w.path, w.classes);
  else
    ret = w;
  endif
endfunction

function __display_filenames__ (msg, p, f)
  if (length (f) > 0)
    printf ("%s %s:\n\n", msg, p);

    maxlen = max (cellfun ("length", f));
    ncols = max (1, floor (terminal_size()(2) / (maxlen + 3)));
    fmt = "";
    for i = 1: ncols
      fmt = sprintf ("%s   %%-%ds", fmt, maxlen);
    endfor
    fmt = [fmt, "\n"];

    nrows = ceil (length (f) / ncols);
    for i = 1 : nrows
      args  = f(i:nrows:end);
      if (length (args) < ncols)
        args(end + 1 : ncols) = {""};
      endif
      printf (fmt, args{:});
    endfor
  endif
endfunction
