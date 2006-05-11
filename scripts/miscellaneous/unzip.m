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
## @deftypefn {Function File} unzip (@var{zipfile}, @var{dir})
## Unpack the ZIP archive @var{zipfile} to the directory @var{dir}.
## If @var{dir} is not specified, it defaults to the current directory.
## @seealso{tar, untar, gzip, gunzip, zip}
## @end deftypefn

## Author: Søren Hauberg <hauberg@gmail.com>
## Adapted-By: jwe

function files = unzip (zipfile, dir)

  if (nargin == 1 || nargin == 2)

    if (nargin == 1)
      dir = ".";
    endif

    if (ischar (zipfile) && ischar (dir))

      [status, output] = system (sprintf ("unzip %s -d %s", zipfile, dir));

      if (status == 0)
	if (nargout > 0)
	  ## Create list of extracted files.  It blows that there seems
	  ## to be no way to get unzip to print a simple list of file
	  ## names.
	  files = strrep (output, "  inflating: ", "");
	  files = cellstr (split (files, "\n"));
	  files = files(2:end-1,:);
	  files = files';
	endif
      else
	error ("unzip: unzip exited with status = %d", status);
      endif
    endif

  else
    print_usage ("unzip");
  endif

endfunction

