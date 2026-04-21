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
## @deftypefn {} {@var{v} =} __bfsearch_impl__ (@var{A}, @var{s})
## Private helper implementing a breadth-first search on a binary or
## count-valued sparse adjacency matrix.
##
## @var{A} is a @math{N}-by-@math{N} sparse matrix.  Any nonzero entry
## @code{@var{A}(i,j)} is treated as an edge from row-node @math{i} to
## column-node @math{j}; zero entries are non-edges.  For an undirected
## graph, pass the symmetric adjacency.  For a directed graph, pass the
## directed adjacency (row = source, column = destination).
##
## @var{s} is a validated 1-based node index (scalar positive integer
## in @code{1:N}).
##
## Returns a numeric column vector @var{v} of node indices in the order
## they are discovered by BFS starting at @var{s}.  Unreachable nodes
## are omitted, so @code{length (@var{v})} is the number of nodes
## reachable from @var{s} (at least 1).
##
## When multiple unvisited neighbours of the current node are
## available, they are visited in ascending order of node index.  This
## matches MATLAB's documented tie-breaking rule for @code{bfsearch}
## and makes the result deterministic.  Parallel-edge multiplicity is
## ignored: a neighbour with any positive edge count is enqueued at
## most once.
##
## @seealso{bfsearch, graph, digraph}
## @end deftypefn

function v = __bfsearch_impl__ (A, s)

  if (nargin != 2)
    print_usage ();
  endif

  N = size (A, 1);

  ## Edgeless / singleton fast path: only the source is visited.
  if (N == 0)
    v = zeros (0, 1);
    return;
  endif

  visited = false (N, 1);
  visited(s) = true;

  ## Discovery order; preallocate to N (the reachable set cannot be
  ## larger than N) and trim at the end.
  v = zeros (N, 1);
  count = 1;
  v(1) = s;

  ## FIFO queue implemented on a preallocated column vector.  head is
  ## the index of the next element to dequeue; tail is one past the
  ## last enqueued element.  Using integer indices avoids the O(n)
  ## copy that shrinking a queue with "queue(1) = []" would cost on
  ## each dequeue.
  queue_buf = zeros (N, 1);
  queue_buf(1) = s;
  head = 1;
  tail = 2;

  while (head < tail)
    u = queue_buf(head);
    head = head + 1;

    ## Out-neighbours (or all neighbours for a symmetric A): find
    ## returns column indices of nonzero entries in ascending order,
    ## which is exactly the tie-break rule we want.
    cols = find (A(u, :));
    if (isempty (cols))
      continue;
    endif
    cols = cols(:).';  # row vector for scalar loop

    for j = 1:numel (cols)
      n = cols(j);
      if (! visited(n))
        visited(n) = true;
        count = count + 1;
        v(count) = n;
        queue_buf(tail) = n;
        tail = tail + 1;
      endif
    endfor
  endwhile

  v = v(1:count);

endfunction


## Private-helper smoke tests.  (Private helpers are not loaded from a
## plain script context by default, so these tests only run when the
## private directory is on the load path -- i.e. inside the BIST runs
## of scripts/graph/bfsearch.m or scripts/graph/digraph.m.)

## Singleton: BFS from node 1 visits only node 1.
%!test
%! v = __bfsearch_impl__ (sparse (1, 1), 1);
%! assert (v, 1);

## Edgeless 5-node: only the source is visited.
%!test
%! v = __bfsearch_impl__ (sparse (5, 5), 3);
%! assert (v, 3);

## Directed 3-cycle 1->2->3->1 from node 1.
%!test
%! A = sparse ([1 2 3], [2 3 1], 1, 3, 3);
%! v = __bfsearch_impl__ (A, 1);
%! assert (v, [1; 2; 3]);

## Directed 3-cycle from node 2: result rotates.
%!test
%! A = sparse ([1 2 3], [2 3 1], 1, 3, 3);
%! v = __bfsearch_impl__ (A, 2);
%! assert (v, [2; 3; 1]);

## Tie-break on ascending-index order for multi-fan-out root.
%!test
%! A = sparse ([1 1 1], [4 2 3], 1, 4, 4);
%! v = __bfsearch_impl__ (A, 1);
%! assert (v, [1; 2; 3; 4]);

## Reachability: nodes in a disjoint component are omitted.
%!test
%! A = sparse ([1 2 4], [2 3 5], 1, 5, 5);
%! v = __bfsearch_impl__ (A, 1);
%! assert (v, [1; 2; 3]);

## Undirected cycle (symmetric adjacency): BFS visits ascending
## neighbours first, so from node 1 we get 1, 2, 4, 3.
%!test
%! s = [1 2 3 4];
%! t = [2 3 4 1];
%! A = sparse ([s t], [t s], 1, 4, 4);
%! v = __bfsearch_impl__ (A, 1);
%! assert (v, [1; 2; 4; 3]);

## BFS result is always a column vector.
%!test
%! A = sparse ([1 2], [2 3], 1, 3, 3);
%! v = __bfsearch_impl__ (A, 1);
%! assert (size (v), [3, 1]);

## Self-loop at source: result unchanged.
%!test
%! A = sparse ([1 1 2], [1 2 3], 1, 3, 3);
%! v = __bfsearch_impl__ (A, 1);
%! assert (v, [1; 2; 3]);
