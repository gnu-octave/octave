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
## Software Foundation, 59 Temple Place - Suite 330, Boston, MA
## 02111-1307, USA.

## -*- texinfo -*-
## @deftypefn {Built-in Function} {[@var{dir}, @var{name}, @var{ext}, @var{ver}]} fnmatch (@var{filename})
## Return the directory, name, extension, and version components of
## @var{filename}.
## @end deftypefn

function filename = fullfile (varargin)

  if (nargin > 0)
    filename = varargin{1};
    if (strcmp (filename(end), "/"))
      filename(end) = "";
    endif
    for i = 2:nargin
      tmp = varargin{i};
      if (strcmp (tmp(1), "/"))
	tmp(1) = "";
      endif
      if (i < nargin && strcmp (tmp(end), "/"))
	tmp(end) = "";
      endif
      filename = strcat (filename, filesep, tmp);
    endfor
  else
    usage ("fullfile (dir1, dir2, ..., file)");
  endif

endfunction
