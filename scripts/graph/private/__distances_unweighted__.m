########################################################################
##
## Copyright (C) 2026 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.
##
########################################################################

## -*- texinfo -*-
## @deftypefn  {} {@var{D} =} __distances_unweighted__ (@var{A})
## @deftypefnx {} {@var{D} =} __distances_unweighted__ (@var{A}, @var{sources})
## Private helper: run breadth-first search (BFS) from each row of
## @var{sources} on the graph whose adjacency pattern is given by the
## square matrix @var{A} and return the resulting shortest-path
## distance matrix @var{D}.  Every non-zero entry in @var{A} is treated
## as a single edge of length @code{1}; weights are ignored entirely.
##
## @var{A} is a square matrix (sparse or dense); the pattern of
## non-zero entries defines the edges.  Self-loops (diagonal entries)
## are ignored: @var{D}(k, @var{sources}(k)) is always @code{0}.
##
## @var{sources} is an optional column vector of positive integer node
## indices.  When omitted or empty, BFS runs from every node and the
## result is the all-pairs @code{N}-by-@code{N} hop-count matrix.
## Otherwise @var{D} is a @code{numel (@var{sources})}-by-@code{N}
## dense double matrix with one row per source.
##
## This helper is used when @code{distances} is called with
## @code{Method = 'unweighted'} (and for @code{Method = 'auto'} on an
## unweighted graph); BFS is @math{O (N + E)} per source, which is
## strictly better than Dijkstra when we do not need to respect
## weights.
##
## This helper is internal to the graph/digraph classes and is not
## intended to be called directly by user code.
## @seealso{distances, graph, digraph}
## @end deftypefn

function D = __distances_unweighted__ (A, sources)

  if (nargin < 1)
    print_usage ();
  endif

  N = size (A, 1);

  if (nargin < 2 || isempty (sources))
    src_list = (1:N).';
  else
    src_list = double (sources(:));
  endif

  K = numel (src_list);

  if (N == 0)
    D = zeros (K, 0);
    return;
  endif

  D = inf (K, N);

  if (K == 0)
    return;
  endif

  ## Precompute neighbor lists once.  Use spones() to collapse
  ## weights to a binary edge indicator; this also handles dense
  ## input uniformly.
  P = spones (A);
  neighbors = cell (N, 1);
  for u = 1:N
    neighbors{u} = find (P(u, :));
  endfor

  for k = 1:K
    src = src_list(k);
    dist = inf (1, N);
    dist(src) = 0;

    ## Simple FIFO queue using an index into a growing list.
    queue = zeros (1, N);
    queue(1) = src;
    head = 1;
    tail = 1;

    while (head <= tail)
      u = queue(head);
      head = head + 1;
      nb = neighbors{u};
      for jj = 1:numel (nb)
        v = nb(jj);
        if (! isfinite (dist(v)))
          dist(v) = dist(u) + 1;
          tail = tail + 1;
          queue(tail) = v;
        endif
      endfor
    endwhile

    D(k, :) = dist;
  endfor

endfunction


## ------------------------------------------------------------------
## BIST blocks
## ------------------------------------------------------------------

## Empty input yields 0x0.
%!test
%! D = __distances_unweighted__ (sparse (0, 0));
%! assert (size (D), [0, 0]);

## 1x1 graph: D = 0.
%!test
%! D = __distances_unweighted__ (sparse (1, 1));
%! assert (D, 0);

## Single directed edge 1->2.
%!test
%! A = sparse ([1], [2], [1], 2, 2);
%! D = __distances_unweighted__ (A);
%! assert (D, [0, 1; Inf, 0]);

## Symmetric edge 1-2.
%!test
%! A = sparse ([1, 2], [2, 1], [1, 1], 2, 2);
%! D = __distances_unweighted__ (A);
%! assert (D, [0, 1; 1, 0]);

## Weights are ignored (same result as binary pattern).
%!test
%! A1 = sparse ([1, 2, 3], [2, 3, 1], [5, 10, 15], 3, 3);
%! A2 = sparse ([1, 2, 3], [2, 3, 1], [1, 1, 1], 3, 3);
%! assert (__distances_unweighted__ (A1), __distances_unweighted__ (A2));

## BFS on a directed 3-cycle: 1->2->3->1.
%!test
%! A = sparse ([1, 2, 3], [2, 3, 1], 1, 3, 3);
%! D = __distances_unweighted__ (A);
%! assert (D, [0 1 2; 2 0 1; 1 2 0]);

## Self-loop is ignored.
%!test
%! A = sparse ([1, 1], [1, 2], 1, 2, 2);
%! D = __distances_unweighted__ (A);
%! assert (D, [0, 1; Inf, 0]);

## Isolated node yields Inf.
%!test
%! A = sparse ([1], [2], 1, 3, 3);
%! D = __distances_unweighted__ (A);
%! assert (D(1, 3), Inf);
%! assert (D(2, 3), Inf);
%! assert (D(3, 1), Inf);

## Negative weights are ignored (unweighted means pattern only).
%!test
%! A = sparse ([1, 2], [2, 3], [-1, -1], 3, 3);
%! D = __distances_unweighted__ (A);
%! assert (D, [0 1 2; Inf 0 1; Inf Inf 0]);

## Dense input accepted.
%!test
%! A = [0 1 0; 0 0 1; 0 0 0];
%! D = __distances_unweighted__ (A);
%! assert (D, [0 1 2; Inf 0 1; Inf Inf 0]);

## sources arg: single source yields 1xN.
%!test
%! A = sparse ([1, 2, 3], [2, 3, 1], 1, 3, 3);
%! D = __distances_unweighted__ (A, 1);
%! assert (size (D), [1, 3]);
%! assert (D, [0, 1, 2]);

## sources arg: multiple sources preserves order.
%!test
%! A = sparse ([1, 2, 3], [2, 3, 1], 1, 3, 3);
%! D = __distances_unweighted__ (A, [3; 1]);
%! assert (D(1, :), [1, 2, 0]);
%! assert (D(2, :), [0, 1, 2]);

## Empty sources falls through to all-pairs.
%!test
%! A = sparse ([1, 2], [2, 3], 1, 3, 3);
%! D1 = __distances_unweighted__ (A, []);
%! D2 = __distances_unweighted__ (A);
%! assert (D1, D2);

## No-arg call errors via print_usage.
%!error __distances_unweighted__ ()

## Chain graph.
%!test
%! N = 5;
%! A = sparse (1:N-1, 2:N, 1, N, N);
%! D = __distances_unweighted__ (A);
%! expected = zeros (N, N);
%! for i = 1:N
%!   for j = 1:N
%!     if (j >= i)
%!       expected(i, j) = j - i;
%!     else
%!       expected(i, j) = Inf;
%!     endif
%!   endfor
%! endfor
%! assert (D, expected);
