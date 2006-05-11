## Copyright (C) 2005 Søren Hauberg
## 
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.
## 
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
## 
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

## -*- texinfo -*-
## @deftypefn {Function File} untar (@var{tarfile}, @var{dir})
## Unpack the TAR archive @var{tarfile} to the directory @var{dir}.
## If @var{dir} is not specified, it defaults to the current directory.
## @seealso{tar, gzip, gunzip, zip, unzip}
## @end deftypefn

## Author: Søren Hauberg <hauberg@gmail.com>
## Adapted-By: jwe

function files = untar (tarfile, dir)

  if (nargin == 1 || nargin == 2)

    if (nargin == 1)
      dir = ".";
    endif

    if (ischar (tarfile) && ischar (dir))

      orig_dir = pwd ();

      tarfile = canonicalize_file_name (tarfile);

      s = stat (dir);
      if (isempty (s))
	[status, msg] = mkdir (dir);
	if (! status)
	  error ("untar: mkdir failed to create %s: %s", dir, msg);
	endif
      elseif (! S_ISDIR (s.mode))
	error ("untar: %s: not a directory", dir);
      endif

      unwind_protect
	chdir (dir);
	[status, output] = system (sprintf ("tar -x -v -f %s", tarfile));
      unwind_protect_cleanup
	chdir (orig_dir);
      end_unwind_protect

      if (status == 0)
	if (nargout > 0)
	  fs = filesep ();
	  if (dir(end) != fs)
	    dir = strcat (dir, fs);
	  endif
	  ## Sadly not reliable if a filename contains a newline
	  ## character!
	  if (output(end) == "\n")
	    output(end) = [];
	  endif
	  files = cellstr (split (output, "\n"));
	  if (! strcmp (dir, "."))
	    nf = length (files);
	    for i = 1:nf
	      files{i} = strcat (dir, files{i});
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
