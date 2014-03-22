## Copyright (C) 2004-2013 Paul Kienzle
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
## @deftypefn  {Function File} {} sprand (@var{m}, @var{n}, @var{d})
## @deftypefnx  {Function File} {} sprand (@var{m}, @var{n}, @var{d}, @var{rc})
## @deftypefnx {Function File} {} sprand (@var{s})
## Generate a random sparse matrix.  The size of the matrix will be
## @var{m} by @var{n}, with a density of values given by @var{d}.
## @var{d} should be between 0 and 1.  Values will be uniformly
## distributed between 0 and 1.
##
## If called with a single matrix argument, a random sparse matrix is
## generated wherever the matrix @var{S} is non-zero.
##
## If called with the rc parameter as a scalar, a random sparse matrix with a
## reciprocal condition number rc is generated. If rc is a vector, then it 
## contains the first singular values of the matrix generated (length(rc) <= min(m, n)).
##
## @seealso{sprandn, sprandsym}
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

function S = sprand (m, n, d, rc)

  if (nargin == 1 )
    S = __sprand_impl__ (m, @rand);
  elseif ( nargin == 3)
    S = __sprand_impl__ (m, n, d, "sprand", @rand);
  elseif (nargin == 4)
    S = __sprand_impl__ (m, n, d, rc, "sprand", @rand);
  else
    print_usage ();
  endif

endfunction


%!test
%! s = sprand (4, 10, 0.1);
%! assert (size (s), [4, 10]);
%! assert (nnz (s) / numel (s), 0.1);

%% Test 1-input calling form
%!test
%! s = sprand (sparse ([1 2 3], [3 2 3], [2 2 2]));
%! [i, j, v] = find (s);
%! assert (sort (i), [1 2 3]');
%! assert (sort (j), [2 3 3]');
%! assert (all (v > 0 & v < 1));

%% Test 4-input calling form
%!test
%! d = rand();
%! s1 = sprand (100, 100, d, 0.4);
%! rc = [5, 4, 3, 2, 1, 0.1];
%! s2 = sprand (100, 100, d, rc);
%! s3 = sprand (6, 4, d, rc);
%! assert (svd (s2)'(1:length (rc)), rc, sqrt (eps)); 
%! assert (1/cond (s1), 0.4, sqrt (eps));
%! assert (nnz (s1) / (100*100), d, 0.02);
%! assert (nnz (s2) / (100*100), d, 0.02); 
%! assert (svd (s3)', [5 4 3 2], sqrt (eps));

%% Test input validation
%!error sprand ()
%!error sprand (1, 2)
%!error sprand (1, 2, 3, 4)
%!error sprand (ones (3), 3, 0.5)
%!error sprand (3.5, 3, 0.5)
%!error sprand (0, 3, 0.5)
%!error sprand (3, ones (3), 0.5)
%!error sprand (3, 3.5, 0.5)
%!error sprand (3, 0, 0.5)
%!error sprand (3, 3, -1)
%!error sprand (3, 3, 2)
%!error sprand (2, 2, 0.2, -1)
%!error sprand (2, 2, 0.2, 2)

%% Test very large, very low density matrix doesn't fail 
%!test
%! s = sprand(1e6,1e6,1e-7);

