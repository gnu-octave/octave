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
## @deftypefn {Function File} {} unzip (@var{zipfile}, @var{dir})
## Unpack the ZIP archive @var{zipfile} to the directory @var{dir}.
## If @var{dir} is not specified, it defaults to the current directory.
## @seealso{unpack, bzip2, bunzip2, tar, untar, gzip, gunzip, zip}
## @end deftypefn

## Author: Søren Hauberg <hauberg@gmail.com>
## Adapted-By: jwe, Bill Denney

function varargout = unzip (files, outputdir)

  if (! (nargin == 1 || nargin == 2))
    print_usage ();
  endif

  if (nargin == 1)
    outputdir = ".";
  endif

  if (nargout > 0)
    varargout = cell (1, nargout);
    [varargout{:}] = unpack (files, outputdir, mfilename ());
  else
    unpack (files, outputdir, mfilename ());
  endif

endfunction
