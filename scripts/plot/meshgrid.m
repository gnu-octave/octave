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
## Software Foundation, 59 Temple Place - Suite 330, Boston, MA
## 02111-1307, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {[@var{xx}, @var{yy}] =} meshgrid (@var{x}, @var{y})
## @deftypefnx {Function File} {[@var{xx}, @var{yy}] =} meshgrid (@var{x})
## Given vectors of @var{x} and @var{y} coordinates, return two matrices corresponding
## to the @var{x} and @var{y} coordinates of a mesh.  The rows of @var{xx} are copies of @var{x},
## and the columns of @var{yy} are copies of @var{y}.
## @end deftypefn
##
## @seealso{sombrero, plot, semilogx, semilogy, loglog, polar, mesh,
## meshdom, contour, bar, stairs, replot, xlabel, ylabel, and title}

## Author: jwe

function [xx, yy] = meshgrid (x, y)

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
  else
    usage ("[xx, yy] = meshgrid (x, y)");
  endif

endfunction
