## Copyright (C) 2005 Søren Hauberg
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
## @deftypefn {Function File} untar (@var{tarfile}, @var{dir})
## Unpack the TAR archive @var{tarfile} to the directory @var{dir}.
## If @var{dir} is not specified, it defaults to the current directory.
## @seealso{tar, gzip, gunzip, zip, unzip}
## @end deftypefn

## Author: Søren Hauberg <hauberg@gmail.com>
## Adapted-By: jwe

function files = untar (tarfile, directory)

  if (nargin == 1 || nargin == 2)

    if (nargin == 1)
      directory = ".";
    endif

    ## The file must exist (and be a file) and the directory must be a
    ## string.
    if (exist (tarfile, "file") && ischar (directory))

      orig_dir = pwd ();

      tarfile = canonicalize_file_name (tarfile);

      s = stat (directory);
      if (isempty (s))
	[status, msg] = mkdir (directory);
	if (! status)
	  error ("untar: mkdir failed to create %s: %s", directory, msg);
	endif
      elseif (! S_ISDIR (s.mode))
	error ("untar: %s: not a directory", directory);
      endif

      unwind_protect
	chdir (directory);
	[status, output] = system (sprintf ("tar -x -v -f %s", tarfile));
      unwind_protect_cleanup
	chdir (orig_dir);
      end_unwind_protect

      if (status == 0)
	if (nargout > 0)
	  fs = filesep ();
	  if (directory(end) != fs)
	    directory = strcat (directory, fs);
	  endif
	  ## Sadly not reliable if a filename contains a newline
	  ## character!
	  if (output(end) == "\n")
	    output(end) = [];
	  endif
	  files = cellstr (split (output, "\n"));
	  if (! strcmp (directory, "."))
	    nf = length (files);
	    for i = 1:nf
	      files{i} = strcat (directory, files{i});
	    endfor
	  endif
	  files = files';
	endif
      else
	error ("tar: tar exited with status = %s", status);
      endif

    else
      error ("untar: expecting arguments to be character strings");
    endif

  else
    print_usage ("untar");
  endif

endfunction
