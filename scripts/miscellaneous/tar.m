## Copyright (C) 2005-2012 Søren Hauberg
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
## @deftypefn  {Function File} {@var{entries} =} tar (@var{tarfile}, @var{files})
## @deftypefnx {Function File} {@var{entries} =} tar (@var{tarfile}, @var{files}, @var{root})
## Pack @var{files} @var{files} into the TAR archive @var{tarfile}.  The
## list of files must be a string or a cell array of strings.
##
## The optional argument @var{root} changes the relative path of @var{files}
## from the current directory.
##
## If an output argument is requested the entries in the archive are
## returned in a cell array.
## @seealso{untar, bzip2, gzip, zip}
## @end deftypefn

## Author: Søren Hauberg <hauberg@gmail.com>

function entries = tar (tarfile, files, root = ".")

  if (nargin < 2 || nargin > 3)
    print_usage ();
  endif

  if (ischar (files))
    files = cellstr (files);
  endif

  if (! (ischar (tarfile) && iscellstr (files) && ischar (root)))
    error ("tar: all arguments must be character strings");
  endif

  cmd = sprintf ("tar cvf %s -C %s %s", tarfile, root,
                 sprintf (" %s", files{:}));

  [status, output] = system (cmd);

  if (status)
    error ("tar: tar exited with status = %d", status);
  endif

  if (nargout > 0)
    if (output(end) == "\n")
      output(end) = [];
    endif
    entries = strsplit (output, "\n");
    entries = entries';
  endif

endfunction
