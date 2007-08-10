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
## Create patch object from @var{x} and @var{y} with color @var{c} and insert in current
## axes object.  Return handle to patch object.
## For an uniform colored patch, @var{c} can be given as [r, g, b]-vector, scalar value refering
## to the current colormap, or string value (e.g. "r" or "red").
## @end deftypefn

## Author: jwe

function h = patch (varargin)

  ## make a default patch object, and make it the current axes for
  ## the current figure.
  tmp = __patch__ (gca (), varargin{:});

  if (nargout > 0)
    h = tmp;
  endif

endfunction
