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

## -*- texinfo -*-
## @deftypefn {Function File} {} meshdom (@var{x}, @var{y})
## Given vectors of @var{x} and @var{y} coordinates, return two matrices
## corresponding to the @var{x} and @var{y} coordinates of the mesh.
##
## Note: this function is provided for compatibility with older versions
## of @sc{Matlab}.  You should use @code{meshgrid} instead.
## @end deftypefn

## Author: jwe

function [xx, yy] = meshdom (x, y)

  if (nargin == 2)
    if (isvector (x) && isvector (y))
      xx = ones (length (y), 1) * x(:).';
      yy = flipud (y(:)) * ones (1, length (x));
    else
      error ("meshdom: arguments must be vectors");
    endif
  else
    usage ("[xx, yy] = meshdom (x, y)");
  endif

endfunction
