## Copyright (C) 2010-2012 John W. Eaton
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
## @deftypefn  {Function File} {} runtests ()
## @deftypefnx {Function File} {} runtests (@var{directory})
## Execute built-in tests for all function files in the specified directory.
## If no directory is specified, operate on all directories in Octave's
## search path for functions.
## @seealso{rundemos, path}
## @end deftypefn

## Author: jwe

function runtests (directory)

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
        error ("runtests: DIRECTORY argument must be a valid pathname");
      endif
    endif
  else
    print_usage ();
  endif

  for i = 1:numel (dirs)
    d = dirs{i};
    run_all_tests (d);
  endfor

endfunction

function run_all_tests (directory)
  dirinfo = dir (directory);
  flist = {dirinfo.name};
  no_tests = {};
  printf ("Processing files in %s:\n\n", directory);
  fflush (stdout);
  for i = 1:numel (flist)
    f = flist{i};
    if (length (f) > 2 && strcmp (f((end-1):end), ".m"))
      ff = fullfile (directory, f);
      if (has_tests (ff))
        print_test_file_name (f);
        [p, n, xf, sk] = test (ff, "quiet");
        print_pass_fail (n, p);
        fflush (stdout);
      else
        no_tests{end+1} = f;
      endif
    endif
  endfor
  if (! isempty (no_tests))
    printf ("\nThe following files in %s have no tests:\n\n", directory);
    printf ("%s", list_in_columns (no_tests));
  endif
endfunction

function retval = has_tests (f)
  fid = fopen (f);
  if (fid >= 0)
    str = fread (fid, "*char")';
    fclose (fid);
    retval = ! isempty (regexp (str, '^%!(test|assert|error|warning)', "lineanchors"));
  else
    error ("runtests: fopen failed: %s", f);
  endif
endfunction

function print_pass_fail (n, p)
  if (n > 0)
    printf (" PASS %4d/%-4d", p, n);
    nfail = n - p;
    if (nfail > 0)
      printf (" FAIL %d", nfail);
    endif
  endif
  puts ("\n");
endfunction

function print_test_file_name (nm)
  filler = repmat (".", 1, 55-length (nm));
  printf ("  %s %s", nm, filler);
endfunction
