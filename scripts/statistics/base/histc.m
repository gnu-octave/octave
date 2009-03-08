## Copyright (C) 2009, Søren Hauberg
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
## @deftypefn {Function File} {@var{n} =} histc (@var{y}, @var{edges})
## @deftypefnx {Function File} {@var{n} =} histc (@var{y}, @var{edges}, @var{dim})
## @deftypefnx {Function File} {[@var{n}, @var{idx}] =} histc (...)
## Produce histogram counts.
##
## When @var{y} is a vector, the function counts the number of elements of
## @var{y} that fall in the histogram bins defined by @var{edges}. This must be
## a vector of monotonically non-decreasing values that define the edges of the
## histogram bins. So, @code{@var{n} (k)} contains the number of elements in
## @var{y} for which @code{@var{edges} (k) <= @var{y} < @var{edges} (k+1)}.
## The final element of @var{n} contains the number of elements of @var{y}
## that was equal to the last element of @var{edges}.
##
## When @var{y} is a @math{N}-dimensional array, the same operation as above is
## repeated along dimension @var{dim}. If this argument is given, the operation
## is performed along the first non-singleton dimension.
##
## If a second output argument is requested an index matrix is also returned.
## The @var{idx} matrix has same size as @var{y}. Each element of @var{idx}
## contains the index of the histogram bin in which the corresponding element
## of @var{y} was counted.
##
## @seealso{hist}
## @end deftypefn

function [n, idx] = histc (data, edges, dim)
  ## Check input
  if (nargin < 2)
    print_usage ();
  endif

  sz = size (data);
  if (nargin < 3)
    dim = find (sz > 1, 1);
  endif

  if (!isreal (data))
    error ("histc: first argument must be real a vector");
  endif
  
  ## Make sure 'edges' is sorted
  num_edges = numel (edges);
  if (isreal (edges))
    edges = edges (:);
    tmp = sort (edges);
    if (any (tmp != edges))
      warning ("histc: edge values not sorted on input");
      edges = tmp;
    endif
  else
    error ("histc: second argument must be a vector");
  endif

  ## Allocate the histogram
  nsz = sz;
  nsz (dim) = num_edges;
  n = zeros (nsz);
  
  ## Allocate 'idx'
  if (nargout > 1)
    idx = zeros (sz);
  endif
  
  ## Prepare indices
  idx1 = cell (1, dim-1);
  for k = 1:length (idx1)
    idx1 {k} = 1:sz (k);
  endfor
  idx2 = cell (length (sz) - dim);
  for k = 1:length (idx2)
    idx2 {k} = 1:sz (k+dim);
  endfor
  
  ## Compute the histograms
  for k = 1:num_edges-1
    b = (edges (k) <= data & data < edges (k+1));
    n (idx1 {:}, k, idx2 {:}) = sum (b, dim);
    if (nargout > 1)
      idx (b) = k;
    endif
  endfor
  b = (data == edges (end));
  n (idx1 {:}, num_edges, idx2 {:}) = sum (b, dim);
  if (nargout > 1)
    idx (b) = num_edges;
  endif

endfunction

%!test
%! data = linspace (0, 10, 1001);
%! n = histc (data, 0:10);
%! assert (n, [repmat(100, 1, 10), 1]);

%!test
%! data = repmat (linspace (0, 10, 1001), [2, 1, 3]);
%! n = histc (data, 0:10, 2);
%! assert (n, repmat ([repmat(100, 1, 10), 1], [2, 1, 3]));

