## Copyright (C) 2008-2012 John W. Eaton
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
## If no directory is specified, operate on all directories in Octave's
## search path for functions.
## @seealso{runtests, path}
## @end deftypefn

## Author: jwe

function rundemos (directory)

  if (nargin == 0)
    dirs = strsplit (path (), pathsep ());
  elseif (nargin == 1)
    if (is_absolute_filename (directory))
      dirs = {directory};
    else
      directory = regexprep (directory, ['\',filesep(),'$'], "");
      fullname = find_dir_in_path (directory);
      if (! isempty (fullname))
        dirs = {fullname};
      else
        error ("rundemos: DIRECTORY argument must be a valid pathname");
      endif
    endif
  else
    print_usage ();
  endif

  for i = 1:numel (dirs)
    d = dirs{i};
    run_all_demos (d);
  endfor

endfunction

function run_all_demos (directory)
  dirinfo = dir (directory);
  flist = {dirinfo.name};
  for i = 1:numel (flist)
    f = flist{i};
    if (length (f) > 2 && strcmp (f((end-1):end), ".m"))
      f = fullfile (directory, f);
      if (has_demos (f))
        try
          demo (f);
        catch
          printf ("error: %s\n\n", lasterror().message)
        end_try_catch
        if (i != numel (flist))
          input ("Press <enter> to continue: ", "s");
        endif
      endif
    endif
  endfor
endfunction

function retval = has_demos (f)
  fid = fopen (f);
  if (f < 0)
    error ("rundemos: fopen failed: %s", f);
  else
    str = fscanf (fid, "%s");
    fclose (fid);
    retval = findstr (str, "%!demo");
  endif
endfunction

%!error rundemos ("foo", 1);
