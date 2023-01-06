########################################################################
##
## Copyright (C) 2008-2023 The Octave Project Developers
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
## @deftypefn  {} {} rundemos ()
## @deftypefnx {} {} rundemos (@var{directory})
## Execute built-in demos for all m-files in the specified @var{directory}.
##
## Demo blocks in any C++ source files (@file{*.cc}) will also be executed
## for use with dynamically linked oct-file functions.
##
## If no directory is specified, operate on all directories in Octave's search
## path for functions.
## @seealso{demo, oruntests, path}
## @end deftypefn

function rundemos (directory)

  if (nargin == 0)
    dirs = ostrsplit (path (), pathsep ());
    do_class_dirs = true;
  elseif (nargin == 1)
    dirs = {canonicalize_file_name(directory)};
    if (isempty (dirs{1}) || ! isfolder (dirs{1}))
      ## Search for directory name in path
      if (directory(end) == '/' || directory(end) == '\')
        directory(end) = [];
      endif
      fullname = dir_in_loadpath (directory);
      if (isempty (fullname))
        error ("rundemos: DIRECTORY argument must be a valid pathname");
      endif
      dirs = {fullname};
    endif
    do_class_dirs = false;
  else
    print_usage ();
  endif

  for i = 1:numel (dirs)
    d = dirs{i};
    run_all_demos (d, do_class_dirs);
  endfor

endfunction

function run_all_demos (directory, do_class_dirs)

  flist = readdir (directory);
  dirs = {};
  for i = 1:numel (flist)
    f = flist{i};
    if ((length (f) > 2 && strcmpi (f((end-1):end), ".m"))
        || (length (f) > 3 && strcmpi (f((end-2):end), ".cc")))
      f = fullfile (directory, f);
      if (has_demos (f))
        try
          demo (f);
        catch
          printf ("error: %s\n\n", lasterror ().message);
        end_try_catch
        if (i != numel (flist))
          input ("Press <enter> to continue: ", "s");
        endif
      endif
    elseif (f(1) == "@")
      f = fullfile (directory, f);
      if (isfolder (f))
        dirs(end+1) = f;
      endif
    endif
  endfor

  ## Recurse into class directories since they are implied in the path
  if (do_class_dirs)
    for i = 1:numel (dirs)
      d = dirs{i};
      run_all_demos (d, false);
    endfor
  endif

endfunction

function retval = has_demos (f)

  fid = fopen (f);
  if (f < 0)
    error ("rundemos: fopen failed: %s", f);
  endif

  str = fread (fid, "*char").';
  fclose (fid);
  retval = ! isempty (regexp (str, '^%!demo', 'lineanchors', 'once'));

endfunction


%!error rundemos ("foo", 1)
%!error <DIRECTORY argument> rundemos ("#_TOTALLY_/_INVALID_/_PATHNAME_#")
