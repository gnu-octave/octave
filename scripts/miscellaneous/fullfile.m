## Copyright (C) 2003 John W. Eaton
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
## @deftypefn {Function File} {@var{filename} =} fullfile (@var{dir1}, @var{dir2}, @dots{}, @var{file})
## Return a complete filename constructed from the given components.
## @seealso{fileparts}
## @end deftypefn

function filename = fullfile (varargin)

  if (nargin > 0)
    filename = varargin{1};
    if (length (filename) < 1)
      filename = ".";
    endif
    if (strcmp (filename(end), filesep))
      filename(end) = "";
    endif
    for i = 2:nargin
      tmp = varargin{i};
      if (strcmp (tmp(1), filesep))
	tmp(1) = "";
      endif
      if (i < nargin && strcmp (tmp(end), filesep))
	tmp(end) = "";
      endif
      filename = strcat (filename, filesep, tmp);
    endfor
  else
    usage ("fullfile (dir1, dir2, ..., file)");
  endif

endfunction
