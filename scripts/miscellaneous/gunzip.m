########################################################################
##
## Copyright (C) 2006-2023 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.
##
########################################################################

## -*- texinfo -*-
## @deftypefn  {} {} gunzip (@var{gzfile})
## @deftypefnx {} {} gunzip (@var{gzfile}, @var{outdir})
## @deftypefnx {} {@var{filelist} =} gunzip (@dots{})
## Unpack the gzip archive @var{gzfile}.
##
## If @var{gzfile} is a directory, all gzfiles in the directory will be
## recursively unpacked.
##
## If @var{outdir} is specified the files are unpacked in this directory rather
## than the one where @var{gzfile} is located.
##
## The optional output @var{filelist} is a list of the uncompressed files.
## @seealso{gzip, unpack, bunzip2, unzip, untar}
## @end deftypefn

function filelist = gunzip (gzfile, outdir = [])

  if (nargin < 1)
    print_usage ();
  endif

  if (isempty (outdir) && ischar (gzfile))
    outdir = fileparts (gzfile);
  endif

  if (nargout > 0)
    filelist = unpack (gzfile, outdir, "gz");
  else
    unpack (gzfile, outdir, "gz");
  endif

endfunction


## Tests for this m-file are located in gzip.m
## Remove from test statistics
%!assert (1)
