## Copyright (C) 2005 John W. Eaton
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
## @deftypefn {Function File} {} axes ()
## @deftypefnx {Function File} {} axes (@var{property}, @var{value}, @dots{})
## @deftypefnx {Function File} {} axes (@var{h})
## Create an axes object and return a handle to it.
## @end deftypefn

## Author: jwe

function h = axes (varargin)

  if (nargin == 0 || nargin > 1)
    ## make default axes object, and make it the current axes for the
    ## current figure.
    cf = gcf ();
    tmp = __go_axes__ (cf, varargin{:});
    set (cf, "currentaxes", tmp);
  elseif (nargin == 1)
    ## arg is axes handle, make it the current axes for the current
    ## figure.
    tmp = varargin{1};
    if (ishandle (tmp) && strcmp (get (tmp, "type"), "axes"))
      parent = get (tmp, "parent");
      set (0, "currentfigure", parent);
      set (parent, "currentaxes", tmp);
    else
      error ("axes: expecting argument to be axes handle");
    endif
  else
    print_usage ();
  endif

  if (nargout > 0)
    h = tmp;
  endif

endfunction
