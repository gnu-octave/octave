## Copyright (C) 1996, 1997 John W. Eaton
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

## Undocumented internal function.

## Author: jwe

function [h, varargin] = __plt_get_axis_arg__ (caller, varargin)

  if (nargin > 1 && length (varargin) > 0 && ishandle (varargin{1}))
    tmp = varargin{1};
    obj = get (tmp);
    if (strcmp (obj.type, "axes"))
      h = tmp;
      varargin(1) = [];
      if (isempty (varargin))
	varargin = {};
      endif
    else
      error ("%s: expecting first argument to be axes handle", caller);
    endif
  else
    h = gca ();
    varargin = {};
  endif

endfunction
