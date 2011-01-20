## Copyright (C) 2009-2011 Eric Chassande-Mottin, CNRS (France)
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
## @deftypefn  {Function File} {[@var{a}, @dots{}] =} textread (@var{filename})
## @deftypefnx {Function File} {[@var{a}, @dots{}] =} textread (@var{filename}, @var{format})
## @deftypefnx {Function File} {[@var{a}, @dots{}] =} textread (@var{filename}, @var{format}, @var{prop1}, @var{value1}, @dots{})
## Read data from a text file.
##
## The file @var{filename} is read and parsed according to @var{format}.  The
## function behaves like @code{strread} except it works by parsing a file
## instead
## of a string.  See the documentation of @code{strread} for details.
## In addition to the options supported by @code{strread}, this function
## supports one more:
## @itemize
## @item "headerlines":
## @end itemize
## The first @var{value} number of lines of @var{str} are skipped.
## @seealso{strread, load, dlmread, fscanf}
## @end deftypefn

function varargout = textread (filename, format = "%f", varargin)
  ## Check input
  if (nargin < 1)
    print_usage ();
  endif

  if (!ischar (filename) || !ischar (format))
    error ("textread: first and second input arguments must be strings");
  endif

  ## Read file
  fid = fopen (filename, "r");
  if (fid == -1)
    error ("textread: could not open '%s' for reading", filename);
  endif

  ## Maybe skip header lines
  headerlines = find (strcmpi (varargin, "headerlines"), 1);
  if (! isempty (headerlines))
    fskipl (fid, headerlines);
    varargin(headerlines:headerlines+1) = [];
  endif

  str = fread (fid, "char=>char").';
  fclose (fid);

  ## Call strread to make it do the real work
  [varargout{1:max (nargout, 1)}] = strread (str, format, varargin {:});

endfunction
