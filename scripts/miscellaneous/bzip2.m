## Copyright (C) 2008 Thorsten Meyer
## (based on gzip.m by David Bateman)
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
## @deftypefn {Function File} {@var{entries} =} bzip2 (@var{files})
## @deftypefnx {Function File} {@var{entries} =} bzip2 (@var{files}, @var{outdir})
## Compress the list of files specified in @var{files}.
## Each file is compressed separately and a new file with a '.bz2' extension
## is created. The original files are not touched.  Existing compressed files 
## are silently overwritten.If @var{outdir} is defined the compressed versions 
## of the files are placed in this directory.
## @seealso{bunzip2, gzip, zip, tar, __xzip__}
## @end deftypefn

function entries = bzip2 (varargin)

  if (nargin == 1 || nargin == 2)
    __xzip__ ("bzip2", "bz2", "bzip2 %s", varargin{:});
  else
    print_usage ();
  endif

endfunction
