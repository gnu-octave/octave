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
## @deftypefn  {Function File} {} sprandn (@var{m}, @var{n}, @var{d})
## @deftypefnx {Function File} {} sprandn (@var{m}, @var{n}, @var{d}, @var{rc})
## @deftypefnx {Function File} {} sprandn (@var{s})
## Generate a random sparse matrix.  The size of the matrix will be
## @var{m} by @var{n}, with a density of values given by @var{d}.
## @var{d} should be between 0 and 1.  Values will be normally
## distributed with mean of zero and variance 1.
##
## If called with a single matrix argument, a random sparse matrix is
## generated wherever the matrix @var{S} is non-zero.
## 
## If called with the rc parameter, then a random sparse matrix with a
## reciprocal condition number rc is generated if rc is scalar. If rc 
## is a vector, then it contains the first singular values of the matrix
## generated (length(rc) <= min(m, n)).
## 
## @seealso{sprand, sprandsym}
## @end deftypefn

## Author: Paul Kienzle <pkienzle@users.sf.net>

function S = sprandn (m, n, d, rc)

  if (nargin == 1 )
    S = __sprand_impl__ (m, @randn);
  elseif ( nargin == 3)
    S = __sprand_impl__ (m, n, d, "sprandn", @randn);
  elseif (nargin == 4)
    S = __sprand_impl__ (m, n, d, rc, "sprandn", @randn);
  else
    print_usage ();
  endif

endfunction


%!test
%! s = sprandn (4, 10, 0.1);
%! assert (size (s), [4, 10]);
%! assert (nnz (s) / numel (s), 0.1);

%% Test 1-input calling form
%!test
%! s = sprandn (sparse ([1 2 3], [3 2 3], [2 2 2]));
%! [i, j] = find (s);
%! assert (sort (i), [1 2 3]');
%! assert (sort (j), [2 3 3]');

%% Test 4-input calling form
%!test
%! d = rand();
%! s1 = sprandn (100, 100, d, 0.4);
%! rc = [5, 4, 3, 2, 1, 0.1];
%! s2 = sprandn (100, 100, d, rc);
%! s3 = sprandn (6, 4, d, rc);
%! assert (svd (s2)'(1:length (rc)), rc, sqrt (eps)); 
%! assert (1/cond (s1), 0.4, sqrt (eps));
%! assert (nnz (s1) / (100*100), d, 0.02); 
%! assert (nnz (s2) / (100*100), d, 0.02); 
%! assert (svd (s3)', [5 4 3 2], sqrt (eps));

%% Test input validation
%!error sprandn ()
%!error sprandn (1, 2)
%!error sprandn (1, 2, 3, 4)
%!error sprandn (ones (3), 3, 0.5)
%!error sprandn (3.5, 3, 0.5)
%!error sprandn (0, 3, 0.5)
%!error sprandn (3, ones (3), 0.5)
%!error sprandn (3, 3.5, 0.5)
%!error sprandn (3, 0, 0.5)
%!error sprandn (3, 3, -1)
%!error sprandn (3, 3, 2)
%!error sprandn (2, 2, 0.2, -1)
%!error sprandn (2, 2, 0.2, 2)

%% Test very large, very low density matrix doesn't fail 
%!test
%! s = sprandn(1e6,1e6,1e-7);

