########################################################################
##
## Copyright (C) 2005-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{filelist} =} tar (@var{tarfile}, @var{files})
## @deftypefnx {} {@var{filelist} =} tar (@var{tarfile}, @var{files}, @var{rootdir})
## Pack the list of files and directories specified in @var{files} into the
## TAR archive @var{tarfile}.
##
## @var{files} is a character array or cell array of strings.  Shell wildcards
## in the filename such as @samp{*} or @samp{?} are accepted and expanded.
## Directories are recursively traversed and all files are added to the
## archive.
##
## If @var{rootdir} is defined then any files without absolute pathnames are
## located relative to @var{rootdir} rather than the current directory.
##
## The optional output @var{filelist} is a list of the files that were included
## in the archive.
## @seealso{untar, unpack, bzip2, gzip, zip}
## @end deftypefn

function filelist = tar (tarfile, files, rootdir = ".")

  if (nargin < 2)
    print_usage ();
  endif

  if (! ischar (tarfile))
    error ("tar: TARFILE must be a string");
  elseif (ischar (files))
    files = cellstr (files);
  elseif (! iscellstr (files))
    error ("tar: FILES must be a character array or cellstr");
  endif

  rootdir = tilde_expand (rootdir);

  tarfile = make_absolute_filename (tarfile);

  if (ispc)
    ## Change tarfile into a mingw style acceptable for tar
    tarfile = __w2mpth__ (tarfile);
  endif

  ## BSD tar emits progress on stderr
  if (tar_is_bsd ())
    cmd = sprintf ("tar cvf %s -C %s %s 2>&1",
                            tarfile, rootdir, sprintf (" '%s'", files{:}));
  else
    cmd = sprintf ("tar cvf %s -C %s %s",
                            tarfile, rootdir, sprintf (" %s", files{:}));
  endif

  ## Save and restore the TAR_OPTIONS environment variable used by GNU tar.
  tar_options_env = getenv ("TAR_OPTIONS");
  unwind_protect
    unsetenv ("TAR_OPTIONS");
    [status, output] = system (cmd);
  unwind_protect_cleanup
    if (! isempty (tar_options_env))
      setenv ("TAR_OPTIONS", tar_options_env);
    endif
  end_unwind_protect

  if (status)
    error ("tar: tar exited with status = %d", status);
  endif

  if (nargout > 0)
    filelist = ostrsplit (output, "\r\n", true);
    filelist = filelist';

    ## BSD tar emits file actions in the first 2 columns
    if (tar_is_bsd ())
      filelist = cellfun (@(x) x(3:end), filelist, 'UniformOutput', false);
    endif
  endif

endfunction


## FIXME: This test may fail if the tar command is not installed.  If this
##        test fails, it might be better to change it into a testif with a
##        runtime condition on the tar program.
%!test
%! ## test tar together with untar
%! orig_dir = pwd ();
%! unwind_protect
%!   dirname = tarname = outdir = "";
%!   dirname = tempname ();
%!   assert (mkdir (dirname));
%!   chdir (dirname);
%!   dirname2 = "dir2";
%!   assert (mkdir (dirname2));
%!   fname1 = "file1";
%!   fname2 = fullfile (dirname2, "file2");
%!   fid = fopen (fname1, "wt");
%!   assert (fid >= 0);
%!   fdisp (fid, "Hello World");
%!   fclose (fid);
%!   fid = fopen (fname2, "wt");
%!   assert (fid >= 0);
%!   fdisp (fid, "Goodbye World");
%!   fclose (fid);
%!   tarname = [tempname() ".tar"];
%!   filelist = tar (tarname, {dirname2, fname1});
%!   if (! strcmp (filelist{3}, fname1))
%!     error ("tar file contents does not match expected file");
%!   endif
%!   if (! exist (tarname, "file"))
%!     error ("tar archive file cannot be found!");
%!   endif
%!   outdir = tempname ();
%!   untar (tarname, outdir);
%!   fid = fopen (fullfile (outdir, fname1), "rt");
%!   assert (fid >= 0);
%!   str = fgetl (fid);
%!   fclose (fid);
%!   assert (str, "Hello World");
%!   fid = fopen (fullfile (outdir, fname2), "rt");
%!   assert (fid >= 0);
%!   str = fgetl (fid);
%!   fclose (fid);
%!   assert (str, "Goodbye World");
%! unwind_protect_cleanup
%!   chdir (orig_dir);
%!   unlink (tarname);
%!   confirm_recursive_rmdir (false, "local");
%!   sts = rmdir (dirname, "s");
%!   sts = rmdir (outdir, "s");
%! end_unwind_protect

## Test input validation
%!error <Invalid call> tar ()
%!error <Invalid call> tar (1)
%!error <TARFILE must be a string> tar (1, "foobar")
%!error <FILES must be a character array or cellstr> tar ("foobar", 1)
