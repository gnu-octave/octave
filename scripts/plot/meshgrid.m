## Copyright (C) 1996, 1997, 2000, 2001, 2002, 2004, 2005, 2006, 2007, 2009
##               John W. Eaton
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {Function File} {[@var{xx}, @var{yy}, @var{zz}] =} meshgrid (@var{x}, @var{y}, @var{z})
## @deftypefnx {Function File} {[@var{xx}, @var{yy}] =} meshgrid (@var{x}, @var{y})
## @deftypefnx {Function File} {[@var{xx}, @var{yy}] =} meshgrid (@var{x})
## Given vectors of @var{x} and @var{y} and @var{z} coordinates, and
## returning 3 arguments, return three-dimensional arrays corresponding
## to the @var{x}, @var{y}, and @var{z} coordinates of a mesh.  When
## returning only 2 arguments, return matrices corresponding to the
## @var{x} and @var{y} coordinates of a mesh.  The rows of @var{xx} are
## copies of @var{x}, and the columns of @var{yy} are copies of @var{y}.
## If @var{y} is omitted, then it is assumed to be the same as @var{x},
## and @var{z} is assumed the same as @var{y}.
## @seealso{mesh, contour}
## @end deftypefn

## Author: jwe

function [xx, yy, zz] = meshgrid (x, y, z)

  if (nargin == 0 || nargin > 3)
    print_usage ();
  endif

  if (nargin < 2)
    y = x;
  endif

  ## Use repmat to ensure that the result values have the same type as
  ## the arguments.

  if (nargout < 3)
    if (isvector (x) && isvector (y))
      xx = repmat (x(:).', length (y), 1);
      yy = repmat (y(:), 1, length (x));
    else
      error ("meshgrid: arguments must be vectors");
    endif
  else
    if (nargin < 3)
      z = y;
    endif
    if (isvector (x) && isvector (y) && isvector (z))
       lenx = length (x);
       leny = length (y);
       lenz = length (z);
       xx = repmat (repmat (x(:).', leny, 1), [1, 1, lenz]);
       yy = repmat (repmat (y(:), 1, lenx), [1, 1, lenz]);
       zz = reshape (repmat (z(:).', lenx*leny, 1)(:), leny, lenx, lenz);
    else
      error ("meshgrid: arguments must be vectors");
    endif
  endif

endfunction
