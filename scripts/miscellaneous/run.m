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
## @deftypefn  {} {} run @var{script}
## @deftypefnx {} {} run ("@var{script}")
## Run @var{script} in the current workspace.
##
## Scripts which reside in directories specified in Octave's load path, and
## which end with the extension @file{.m}, can be run simply by typing
## their name.  For scripts not located on the load path, use @code{run}.
##
## The filename @var{script} can be a bare, fully qualified, or relative
## filename and with or without a file extension.  If no extension is
## specified, Octave will first search for a script with the @file{.m}
## extension before falling back to the script name without an extension.
##
## Implementation Note: If @var{script} includes a path component, then
## @code{run} first changes the working directory to the directory where
## @var{script} is found.  Next, the script is executed.  Finally, @code{run}
## returns to the original working directory @emph{unless} @var{script} has
## specifically changed directories.
## @seealso{path, addpath, source}
## @end deftypefn

function run (script)

  if (nargin < 1)
    print_usage ();
  endif

  [d, f, ext] = fileparts (script);
  if (! strcmp (ext, ".m"))
    ## prefer files with .m extension for compatibility with Matlab
    if (exist ([script ".m"], "file"))
      f = [f ext];
      ext = ".m";
      script = [script ".m"];
    endif
  endif

  if (! exist (script, "file"))
    error ("run: file SCRIPT must exist and be a valid Octave scriptfile");
  endif

  if (! isempty (d))
    if (! isfolder (d))
      error ("run: the path %s doesn't exist", d);
    endif

    startdir = pwd ();
    scriptdir = "";
    unwind_protect
      cd (d);
      scriptdir = pwd ();
      evalin ("caller", sprintf ("source ('%s%s');", f, ext),
              "rethrow (lasterror ())");
    unwind_protect_cleanup
      if (is_same_file (scriptdir, pwd ()))
        cd (startdir);
      endif
    end_unwind_protect

  else
    if (! isempty (ext))
      script = which (script);
    else
      ## Search PATH with null extension ('.' will be stripped and ext = "")
      script = which ([script "."]);
    endif
    evalin ("caller", sprintf ("source ('%s');", script),
            "rethrow (lasterror ())");
  endif

endfunction


## Test script file execution
## Use a variable name that is unlikely to be the name of a function.
%!test
%! clear _5yVNhWVJWJn47RKnzxPsyb_
%! assert (exist ("_5yVNhWVJWJn47RKnzxPsyb_"), 0);
%! tmp_dir = tempname ();
%! test_script = fullfile (tmp_dir, "test_script.m");
%! unwind_protect
%!   mkdir (tmp_dir);
%!   fid = fopen (test_script, "w");
%!   fprintf (fid, "_5yVNhWVJWJn47RKnzxPsyb_ = 1337;\n");
%!   fclose (fid);
%!   run (test_script);
%!   assert (exist ("_5yVNhWVJWJn47RKnzxPsyb_", "var"), 1);
%!   assert (_5yVNhWVJWJn47RKnzxPsyb_, 1337);
%! unwind_protect_cleanup
%!   unlink (test_script);
%!   sts = rmdir (tmp_dir);
%! end_unwind_protect

## Test function file execution
%!test
%! path_orig = path ();
%! tmp_dir = tempname ();
%! test_function = fullfile (tmp_dir, "tf.m");
%! unwind_protect
%!   mkdir (tmp_dir);
%!   fid = fopen (test_function, "w");
%!   fprintf (fid, "function tf ()\n");
%!   fprintf (fid, "  addpath ('%s');\n", tmp_dir);
%!   fprintf (fid, "endfunction\n");
%!   fclose (fid);
%!   ## Check if temporary directory is on the loadpath.
%!   ## Function 'dir_in_loadpath' is broken for this use case, so code a test.
%!   dirs = strsplit (path (), pathsep ());
%!   tstval1 = any (is_same_file (tmp_dir, dirs));
%!   run (test_function);
%!   dirs = strsplit (path (), pathsep ());
%!   tstval2 = any (is_same_file (tmp_dir, dirs));
%!   assert (tstval1, false);
%!   assert (tstval2, true);
%! unwind_protect_cleanup
%!   unlink (test_function);
%!   sts = rmdir (tmp_dir);
%!   path (path_orig);
%! end_unwind_protect

## Test input validation
%!error <Invalid call> run ()
%!error run ("a", "b")
%!error <SCRIPT must exist> run ("__A_very_#unlikely#_file_name__")
