## Copyright (C) 2008-2013 John W. Eaton
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
## @deftypefn  {Function File} {} rundemos ()
## @deftypefnx {Function File} {} rundemos (@var{directory})
## Execute built-in demos for all function files in the specified directory.
## Also executes demos in any C++ source files found in the directory, for
## use with dynamically linked functions.
##
## If no directory is specified, operate on all directories in Octave's
## search path for functions.
## @seealso{runtests, path}
## @end deftypefn

## Author: jwe

function rundemos (directory)

  if (nargin == 0)
    dirs = ostrsplit (path (), pathsep ());
    do_class_dirs = true;
  elseif (nargin == 1)
    if (is_absolute_filename (directory))
      dirs = {directory};
    elseif (is_rooted_relative_filename (directory))
      dirs = {canonicalize_file_name(directory)};
    else
      if (directory(end) == filesep ())
        directory = directory(1:end-1);
      endif
      fullname = find_dir_in_path (directory);
      if (! isempty (fullname))
        dirs = {fullname};
      else
        error ("rundemos: DIRECTORY argument must be a valid pathname");
      endif
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
    if ((length (f) > 2 && strcmpi (f((end-1):end), ".m")) ||
        (length (f) > 3 && strcmpi (f((end-2):end), ".cc")))
      f = fullfile (directory, f);
      if (has_demos (f))
        try
          demo (f);
        catch
          printf ("error: %s\n\n", lasterror().message);
        end_try_catch
        if (i != numel (flist))
          input ("Press <enter> to continue: ", "s");
        endif
      endif
    elseif (f(1) == "@")
      f = fullfile (directory, f);
      if (isdir (f))
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
  else
    str = fread (fid, "*char").';
    fclose (fid);
    retval = ! isempty (regexp (str, '^%!demo', 'lineanchors', 'once'));
  endif
endfunction


%!error rundemos ("foo", 1)
%!error <DIRECTORY argument> rundemos ("#_TOTALLY_/_INVALID_/_PATHNAME_#")

