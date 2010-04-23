## Copyright (C) 2004, 2005, 2006, 2007, 2008, 2009 Paul Kienzle
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
## @deftypefn {Function File} {} sprand (@var{m}, @var{n}, @var{d})
## @deftypefnx {Function File} {} sprand (@var{s})
## Generate a random sparse matrix.  The size of the matrix will be
## @var{m} by @var{n}, with a density of values given by @var{d}.
## @var{d} should be between 0 and 1. Values will be uniformly
## distributed between 0 and 1.
##
## Note: sometimes the actual density may be a bit smaller than @var{d}. 
## This is unlikely to happen for large really sparse matrices.
##
## If called with a single matrix argument, a random sparse matrix is
## generated wherever the matrix @var{S} is non-zero.
## @seealso{sprandn}
## @end deftypefn

## Author: Paul Kienzle <pkienzle@users.sf.net>
##
## Changelog:
##
## Piotr Krzyzanowski <przykry2004@users.sf.net>
##      2004-09-27      use Paul's hint to allow larger random matrices
##                      at the price of sometimes lower density than desired
## David Bateman 
##      2004-10-20      Texinfo help and copyright message 

function S = sprand (m, n, d)
  if (nargin == 1)
    [i, j, v] = find (m);
    [nr, nc] = size (m);
    S = sparse (i, j, rand (size (v)), nr, nc);
  elseif (nargin == 3)
    mn = n*m;
    ## how many entries in S would be satisfactory?
    k = round (d*mn);
    idx = unique (fix (rand (min (k*1.01, k+10), 1) * mn)) + 1; 
    ## idx contains random numbers in [1,mn]
    ## generate 1% or 10 more random values than necessary in order to
    ## reduce the probability that there are less than k distinct
    ## values; maybe a better strategy could be used but I don't think
    ## it's worth the price

    ## actual number of entries in S
    k = min (length (idx), k);
    j = floor ((idx(1:k)-1)/m);
    i = idx(1:k) - j*m;
    if (isempty (i))
      S = sparse (m, n);
    else
      S = sparse (i, j+1, rand (k, 1), m, n);
    endif
  else
    print_usage ();
  endif
endfunction
