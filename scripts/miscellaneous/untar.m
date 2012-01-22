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
## @deftypefn  {Function File} {} untar (@var{tarfile})
## @deftypefnx {Function File} {} untar (@var{tarfile}, @var{dir})
## Unpack the TAR archive @var{tarfile} to the directory @var{dir}.
## If @var{dir} is not specified, it defaults to the current directory.
## @seealso{tar, unpack, bunzip2, gunzip, unzip}
## @end deftypefn

## Author: Søren Hauberg <hauberg@gmail.com>
## Adapted-By: jwe, Bill Denney

function varargout = untar (tarfile, dir = ".")

  if (nargin != 1 && nargin != 2)
    print_usage ();
  endif

  if (nargout > 0)
    varargout = cell (1, nargout);
    [varargout{:}] = unpack (tarfile, dir, mfilename ());
  else
    unpack (tarfile, dir, mfilename ());
  endif

endfunction
