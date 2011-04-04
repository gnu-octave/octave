## Copyright (C) 1999-2011 Kai Habel
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
## @deftypefn  {Function File} {@var{T} =} delaunay3 (@var{x}, @var{y}, @var{z})
## @deftypefnx {Function File} {@var{T} =} delaunay3 (@var{x}, @var{y}, @var{z}, @var{opt})
## A matrix of size [n, 4] is returned.  Each row contains a
## set of tetrahedron which are
## described by the indices to the data point vectors (x,y,z).
##
## A fourth optional argument, which must be a string or cell array of strings,
## contains extra options passed to the underlying qhull command.  See the
## documentation for the Qhull library for details.
## @seealso{delaunay, delaunayn}
## @end deftypefn

## Author: Kai Habel <kai.habel@gmx.de>

function tetr = delaunay3 (x, y, z, opt)

  if (nargin != 3 && nargin != 4)
    print_usage ();
  endif

  if (isvector (x) && isvector (y) &&isvector (z)
      && length (x) == length (y) && length(x) == length (z))
    if (nargin == 3)
      tetr = delaunayn ([x(:), y(:), z(:)]);
    elseif (ischar (opt) || iscell (opt))
      tetr = delaunayn ([x(:), y(:), z(:)], opt);
    else
      error ("delaunay3: fourth argument must be a string or cell array of strings");
    endif
  else
    error ("delaunay3: first three input arguments must be vectors of same size");
  endif

endfunction

%!testif HAVE_QHULL
%! x = [-1, -1, 1, 0, -1]; y = [-1, 1, 1, 0, -1]; z = [0, 0, 0, 1, 1];
%! assert (sortrows (sort (delaunay3 (x, y, z), 2)), [1,2,3,4;1,2,4,5])

