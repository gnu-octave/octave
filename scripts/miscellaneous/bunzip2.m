## Copyright (C) 2006 Bill Denney
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
## @deftypefn {Function File} {} bunzip2 (@var{bzfile}, @var{dir})
## Unpack the bzip2 archive @var{bzfile} to the directory @var{dir}. If
## @var{dir} is not specified, it defaults to the current directory.
## @seealso{unpack, bzip2, tar, untar, gzip, gunzip, zip, unzip}
## @end deftypefn

## Author: Bill Denney <denney@seas.upenn.edu>

function varargout = bunzip2 (files, outputdir)

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
