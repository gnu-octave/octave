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
## @deftypefn {Function File} {} patch ()
## @deftypefnx {Function File} {} patch (@var{x}, @var{y}, @var{c})
## @deftypefnx {Function File} {} patch (@var{x}, @var{y}, @var{c}, @var{opts})
## @deftypefnx {Function File} {} patch (@var{h}, @dots{})
## Create patch object from @var{x} and @var{y} with color @var{c} and
## insert in the current axes object.  Return handle to patch object.
##
## For a uniform colored patch, @var{c} can be given as an RGB vector,
## scalar value referring to the current colormap, or string value (for
## example, "r" or "red"). 
## @end deftypefn

## Author: jwe

function h = patch (varargin)

  if (isscalar (varargin{1}) && ishandle (varargin{1}))
    h = varargin {1};
    if (! strcmp (get (h, "type"), "axes"))
      error ("patch: expecting first argument to be an axes object");
    endif
    oldh = gca ();
    unwind_protect
      axes (h);
      tmp = __patch__ (h, varargin{:});
    unwind_protect_cleanup
      axes (oldh);
    end_unwind_protect
  else
    tmp = __patch__ (gca (), varargin{:});
  endif

  if (nargout > 0)
    h = tmp;
  endif

endfunction
