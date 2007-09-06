## Copyright (C) 2007  David Bateman
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {@var{entries} =} gzip (@var{files})
## @deftypefnx {Function File} {@var{entries} =} gzip (@var{files}, @var{outdir})
## Compress the list of files and/or directories specified in @var{files}.
## Each file is compressed separately and a new file with a '.gz' extension
## is create. The original file is not touch. If @var{rootdir} is defined 
## the compressed versions of the files are placed in this directory.
## @seealso{gunzip, zip, tar}
## @end deftypefn

function entries = gzip (files, outdir)

  if (nargin == 1 || nargin == 2)

    if (nargin == 2 && ! exist (outdir, "dir"))
      error ("gzip: output directory does not exist");
    endif

    if (ischar (files))
      files = cellstr (files);
    endif

    if (nargin == 1)
      outdir = tmpnam ();
      mkdir (outdir);
    endif

    cwd = pwd();
    unwind_protect
      if (iscellstr (files))
	files = glob (files);

	## Ignore any file with a .gz extension
	files (cellfun (@(x) strcmp (x(end-2:end), ".gz"), files)) = [];
    
	copyfile (files, outdir);
	[d, f] = myfileparts(files);
	cd (outdir);

	cmd = sprintf ("gzip -r %s", sprintf (" %s", f{:}));

	[status, output] = system (cmd);

	if (status == 0)

	  if (nargin == 2)
	    gzfiles = cellfun(@(x) fullfile (outdir, sprintf ("%s.gz", x)), ...
			      f, "UniformOutput", false);
	  else
	    movefile (cellfun(@(x) sprintf ("%s.gz", x), f, ...
			      "UniformOutput", false), cwd);
	    gzfiles = cellfun(@(x) sprintf ("%s.gz", x), ...
			      files, "UniformOutput", false);
	  endif

	  if (nargout > 0)
            entries = gzfiles;
	  endif
	else
	  error ("gzip: failed with exit status = %d", status);
	endif
    
      else
	error ("gzip: expecting all arguments to be character strings");
      endif
    unwind_protect_cleanup
      cd(cwd);
      if (nargin == 1)
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

function [d, f] = myfileparts (x)
  [d, f, ext] = cellfun (@(x) fileparts (x), x, "UniformOutput", false);
  f = cellfun (@(x, y) sprintf ("%s%s", x, y), f, ext, ...
	       "UniformOutput", false); 
  idx = cellfun (@(x) isdir (x), x);
  d(idx) = "";
  f(idx) = x(idx);
endfunction
