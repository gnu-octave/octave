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
## @deftypefn {Function File} {[@var{xx}, @var{yy}, @var{zz}] =} meshgrid (@var{x}, @var{y}, @var{z})
## @deftypefnx {Function File} {[@var{xx}, @var{yy}] =} meshgrid (@var{x}, @var{y})
## @deftypefnx {Function File} {[@var{xx}, @var{yy}] =} meshgrid (@var{x})
## Given vectors of @var{x}, @var{y}, and @var{z} coordinates, return
## three dimensional arrays corresponding to the @var{x}, @var{y}, and
## @var{z} coordinates of a mesh.  Given only @var{x} and @var{y},
## return matrices corresponding to the @var{x} and @var{y} coordinates
## of a mesh.  The rows of @var{xx} are copies of @var{x}, and the
## columns of @var{yy} are copies of @var{y}.  If @var{y} is omitted,
## then it is assumed to be the same as @var{x}.
## @end deftypefn
##
## @seealso{mesh, contour}

## Author: jwe

function [xx, yy, zz] = meshgrid (x, y, z)

  if (nargin == 1)
    y = x;
  endif
  if (nargin > 0 && nargin < 3)
    if (isvector (x) && isvector (y))
      xx = ones (length (y), 1) * x(:).';
      yy = y(:) * ones (1, length (x));
    else
      error ("meshgrid: arguments must be vectors");
    endif
  elseif (nargin == 3)
   if (isvector (x) && isvector (y) && isvector (z))
      lenx = length (x);
      leny = length (y);
      lenz = length (z);
      xx = repmat (ones (leny, 1) * x(:).', [1, 1, lenz]);
      yy = repmat (y(:) * ones (1, lenx), [1, 1, lenz]);
      zz = reshape (repmat (z(:).', lenx*leny, 1)(:), leny, lenx, lenz);
   else
      error ("meshgrid: arguments must be vectors");
   endif
  else
    usage ("[xx, yy, zz] = meshgrid (x, y, z)");
  endif

endfunction
