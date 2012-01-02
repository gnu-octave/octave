## Copyright (C) 2004-2012 Paul Kienzle
## Copyright (C) 2012 Jordi Gutiérrez Hermoso
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
##
## Original version by Paul Kienzle distributed as free software in the
## public domain.

## -*- texinfo -*-
## @deftypefn  {Function File} {} __sprand_impl__ (@var{s}, @var{randfun})
## @deftypefnx {Function File} {} __sprand_impl__ (@var{m}, @var{n}, @var{d}, @var{funname}, @var{randfun})
## Undocumented internal function.
## @end deftypefn

## Actual implementation of sprand and sprandn happens here.

function S = __sprand_impl__ (varargin)

  if (nargin == 2)
    m = varargin{1};
    randfun = varargin{2};
    [i, j] = find (m);
    [nr, nc] = size (m);
    S = sparse (i, j, randfun (size (i)), nr, nc);
    return;
  endif

  [m, n, d, funname, randfun] = deal(varargin{:});

  if (!(isscalar (m) && m == fix (m) && m > 0))
    error ("%s: M must be an integer greater than 0", funname);
  endif

  if (!(isscalar (n) && n == fix (n) && n > 0))
    error ("%s: N must be an integer greater than 0", funname);
  endif

  if (d < 0 || d > 1)
    error ("%s: density D must be between 0 and 1", funname);
  endif

  mn = m*n;
  k = round (d*mn);
  idx = randperm (mn, k);

  [i, j] = ind2sub ([m, n], idx);
  S = sparse (i, j, randfun (k, 1), m, n);

endfunction