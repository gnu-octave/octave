## Copyright (C) 2006-2012 Bill Denney
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
## @deftypefn {Function File} {} gunzip (@var{gzfile}, @var{dir})
## Unpack the gzip archive @var{gzfile} to the directory @var{dir}.  If
## @var{dir} is not specified, it defaults to the current directory.  If
## @var{gzfile} is a directory, all gzfiles in the directory will be
## recursively gunzipped.
## @seealso{gzip, unpack, bunzip2, unzip, untar}
## @end deftypefn

## Author: Bill Denney <denney@seas.upenn.edu>

function varargout = gunzip (gzfile, dir = ".")

  if (nargin != 1 && nargin != 2)
    print_usage ();
  endif

  if (nargout > 0)
    varargout = cell (1, nargout);
    [varargout{:}] = unpack (gzfile, dir, mfilename ());
  else
    unpack (gzfile, dir, mfilename ());
  endif

endfunction
