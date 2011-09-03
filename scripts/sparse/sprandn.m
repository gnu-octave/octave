## Copyright (C) 2004-2011 Paul Kienzle
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
## @deftypefn  {Function File} {} sprandn (@var{m}, @var{n}, @var{d})
## @deftypefnx {Function File} {} sprandn (@var{s})
## Generate a random sparse matrix.  The size of the matrix will be
## @var{m} by @var{n}, with a density of values given by @var{d}.
## @var{d} should be between 0 and 1. Values will be normally
## distributed with mean of zero and variance 1.
##
## Note: sometimes the actual density may be a bit smaller than @var{d}.
## This is unlikely to happen for large really sparse matrices.
##
## If called with a single matrix argument, a random sparse matrix is
## generated wherever the matrix @var{S} is non-zero.
## @seealso{sprand}
## @end deftypefn

## Author: Paul Kienzle <pkienzle@users.sf.net>

function S = sprandn (m, n, d)

  if (nargin != 1 && nargin != 3)
    print_usage ();
  endif

  if (nargin == 1)
    [i, j] = find (m);
    [nr, nc] = size (m);
    S = sparse (i, j, randn (size (i)), nr, nc);
    return;
  endif

  if (!(isscalar (m) && m == fix (m) && m > 0))
    error ("sprand: M must be an integer greater than 0");
  endif

  if (!(isscalar (n) && n == fix (n) && n > 0))
    error ("sprand: N must be an integer greater than 0");
  endif

  if (d < 0 || d > 1)
    error ("sprand: density D must be between 0 and 1");
  endif

  mn = m*n;
  k = round (d*mn);
  idx = unique (fix (rand (min (k*1.01, k+10), 1) * mn)) + 1;
  ## idx contains random numbers in [1,mn]
  ## generate 1% or 10 more random values than necessary in order to
  ## reduce the probability that there are less than k distinct
  ## values; maybe a better strategy could be used but I don't think
  ## it's worth the price.

  ## actual number of entries in S
  k = min (length (idx), k);
  j = floor ((idx(1:k)-1)/m);
  i = idx(1:k) - j*m;
  if (isempty (i))
    S = sparse (m, n);
  else
    S = sparse (i, j+1, randn (k, 1), m, n);
  endif

endfunction


## FIXME: Test for density can't happen until code of sprandn is improved
%!test
%! s = sprandn (4, 10, 0.1);
%! assert (size (s), [4, 10]);
##%! assert (nnz (s) / numel (s), 0.1, .01);

%% Test 1-input calling form
%!test
%! s = sprandn (sparse ([1 2 3], [3 2 3], [2 2 2]));
%! [i, j, v] = find (s);
%! assert (sort (i), [1 2 3]');
%! assert (sort (j), [2 3 3]');

%% Test input validation
%!error sprandn ()
%!error sprandn (1, 2)
%!error sprandn (1, 2, 3, 4)
%!error sprandn (ones(3), 3, 0.5)
%!error sprandn (3.5, 3, 0.5)
%!error sprandn (0, 3, 0.5)
%!error sprandn (3, ones(3), 0.5)
%!error sprandn (3, 3.5, 0.5)
%!error sprandn (3, 0, 0.5)
%!error sprandn (3, 3, -1)
%!error sprandn (3, 3, 2)

