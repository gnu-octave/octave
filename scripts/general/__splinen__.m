## Copyright (C) 2007 David Bateman
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

## -*- texinfo -*-
## @deftypefn {Function File} {@var{yi} = } __splinen__ (@var{x}, @var{y}, @var{xi})
## Internal support function for multi-dimensional splines.
## @end deftypefn

## FIXME: Allow arbitrary grids..

function yi = __splinen__ (x, y, xi, extrapval, f)
  if (nargin != 5)
    error ("Incorrect number of arguments");
  endif
  isvec = @(x) numel (x) == length (x);   # ND isvector function
  if (!iscell (x) || length(x) < ndims(y) || any (! cellfun (isvec, x)) ||
      !iscell (xi) || length(xi) < ndims(y) || any (! cellfun (isvec, xi)))
    error ("%s: non gridded data or dimensions inconsistent", f);
  endif
  yi = y;
  for i = length(x):-1:1
    yi = permute (spline (x{i}, yi, xi{i}), [length(x),1:length(x)-1]);
  endfor

  [xi{:}] = ndgrid (xi{:});
  idx = zeros (size(xi{1}));
  for i = 1 : length(x)
    idx |= xi{i} < min (x{i}(:)) | xi{i} > max (x{i}(:));
  endfor
  yi(idx) = extrapval;
endfunction
