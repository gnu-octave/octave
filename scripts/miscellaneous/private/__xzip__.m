## Copyright (C) 2008, 2009 Thorsten Meyer
## based on the original gzip function by David Bateman
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
## @deftypefn {Function File} {@var{entries} =} __xzip__ (@var{commandname}, @var{extension}, @var{commandtemplate}, @var{files}, @var{outdir})
## Undocumented internal function.
## @end deftypefn

## Compress the list of files and/or directories specified in @var{files} 
## with the external compression command @var{commandname}. The template 
## @var{commandtemplate} is used to actually start the command. Each file
## is compressed separately and a new file with the extension @var{extension} 
## is created and placed into the directory @var{outdir}. The original files 
## are not touched. Existing compressed files are silently overwritten. 
## This is an internal function. Do not use directly.

function entries = __xzip__ (commandname, extension, 
                             commandtemplate, files, outdir)

  if (nargin == 4 || nargin == 5)
    if (! ischar (extension) || length (extension) == 0)
      error ("__xzip__: EXTENSION has to be a string with finite length");
    endif
    
    if (nargin == 5 && ! exist (outdir, "dir"))
      error ("__xzip__: output directory does not exist");
    endif

    if (ischar (files))
      files = cellstr (files);
    else
      error ("__xzip__: expecting FILES to be a character array");
    endif

    if (nargin == 4)
      outdir = tmpnam ();
      mkdir (outdir);
    endif

    cwd = pwd();
    unwind_protect
      files = glob (files);

      ## Ignore any file with the compress extension
      files (cellfun (@(x) length(x) > length(extension) 
        && strcmp (x((end - length(extension) + 1):end), extension), 
        files)) = [];

      copyfile (files, outdir);

      [d, f] = myfileparts(files);

      cd (outdir);

      cmd = sprintf (commandtemplate, sprintf (" %s", f{:}));

      [status, output] = system (cmd);
      if (status == 0)

        if (nargin == 5)
          compressed_files = cellfun(
              @(x) fullfile (outdir, sprintf ("%s.%s", x, extension)), 
              f, "uniformoutput", false);
        else
          movefile (cellfun(@(x) sprintf ("%s.%s", x, extension), f, 
                            "uniformoutput", false), cwd);
          ## FIXME this does not work when you try to compress directories

          compressed_files  = cellfun(@(x) sprintf ("%s.%s", x, extension), 
                                      files, "uniformoutput", false);
        endif

        if (nargout > 0)
          entries = compressed_files;
        endif
      else
        error ("__xzip__: %s command failed with exit status = %d",
               commandname, status);
      endif
    unwind_protect_cleanup
      cd(cwd);
      if (nargin == 4)
        crr = confirm_recursive_rmdir ();
        unwind_protect
          confirm_recursive_rmdir (false);
          rmdir (outdir, "s");
        unwind_protect_cleanup
          confirm_recursive_rmdir (crr);
        end_unwind_protect
      endif
    end_unwind_protect
  else
    print_usage ();
  endif

endfunction

function [d, f] = myfileparts (files)
  [d, f, ext] = cellfun (@(x) fileparts (x), files, "uniformoutput", false);
  f = cellfun (@(x, y) sprintf ("%s%s", x, y), f, ext,
               "uniformoutput", false); 
  idx = cellfun (@isdir, files);
  d(idx) = "";
  f(idx) = files(idx);
endfunction

## FIXME -- reinstate these tests if we invent a way to test private
## functions directly.
##
## %!error <extension has to be a string with finite length> 
## %!  __xzip__("gzip", "", "gzip -r %s", "bla");
## %!error <no files to move>
## %!  __xzip__("gzip", ".gz", "gzip -r %s", tmpnam);
## %!error <command failed with exit status>
## %!  # test __xzip__ with invalid compression command
## %!  unwind_protect
## %!    filename = tmpnam;
## %!    dummy    = 1;
## %!    save(filename, "dummy");
## %!    dirname  = tmpnam;
## %!    mkdir(dirname);
## %!    entry = __xzip__("gzip", ".gz", "xxxzipxxx -r %s 2>/dev/null", 
## %!                     filename, dirname);
## %!  unwind_protect_cleanup
## %!    delete(filename);
## %!    rmdir(dirname);
## %!  end_unwind_protect
