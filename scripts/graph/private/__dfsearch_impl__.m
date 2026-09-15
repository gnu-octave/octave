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
## @deftypefn {} {@var{v} =} __dfsearch_impl__ (@var{A}, @var{s})
## Private helper implementing a depth-first search on a binary or
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
## they are discovered by DFS starting at @var{s}.  Unreachable nodes
## are omitted, so @code{length (@var{v})} is the number of nodes
## reachable from @var{s} (at least 1).
##
## When multiple unvisited neighbours of the current node are
## available, they are visited in ascending order of node index.  This
## matches MATLAB's documented tie-breaking rule for @code{dfsearch}
## and makes the result deterministic.  Parallel-edge multiplicity is
## ignored: a neighbour with any positive edge count is visited at most
## once.
##
## @seealso{dfsearch, graph, digraph}
## @end deftypefn

function v = __dfsearch_impl__ (A, s)

  if (nargin != 2)
    print_usage ();
  endif

  N = size (A, 1);

  ## Edgeless / singleton fast path: only the source is visited.
  if (N == 0)
    v = zeros (0, 1);
    return;
  endif

  ## State: 0 = undiscovered, 1 = on stack (discovered), 2 = finished.
  state = zeros (N, 1);
  state(s) = 1;

  ## Discovery order (preallocate; reachable set is at most N).
  v = zeros (N, 1);
  count = 1;
  v(1) = s;

  ## Explicit DFS stack.  Each element is the node id.  We store a
  ## parallel vector of child lists and a per-frame cursor so we can
  ## resume iterating children after a recursive descent.
  stack_nodes = zeros (N, 1);
  stack_children = cell (N, 1);
  stack_cursor = zeros (N, 1);
  sp = 1;  # stack pointer (1-based; sp is the index of the top-of-stack)

  stack_nodes(sp) = s;
  cols = find (A(s, :));
  stack_children{sp} = cols(:).';  # row vector, ascending index order
  stack_cursor(sp) = 1;

  while (sp >= 1)
    u = stack_nodes(sp);
    children = stack_children{sp};
    idx = stack_cursor(sp);

    advanced = false;
    while (idx <= numel (children))
      n = children(idx);
      idx = idx + 1;
      if (state(n) == 0)
        ## Tree edge -- recurse.
        stack_cursor(sp) = idx;    # save resume position on parent
        state(n) = 1;
        count = count + 1;
        v(count) = n;
        sp = sp + 1;
        stack_nodes(sp) = n;
        cols = find (A(n, :));
        stack_children{sp} = cols(:).';
        stack_cursor(sp) = 1;
        advanced = true;
        break;
      endif
      ## Already discovered or finished -- just advance the cursor.
    endwhile

    if (! advanced)
      ## All children of u processed; pop.
      state(u) = 2;
      sp = sp - 1;
    endif
  endwhile

  v = v(1:count);

endfunction


## Private-helper smoke tests.  (Private helpers are not loaded from a
## plain script context by default, so these tests only run when the
## private directory is on the load path -- i.e. inside the BIST runs
## of scripts/graph/dfsearch.m or scripts/graph/digraph.m.)

## Singleton: DFS from node 1 visits only node 1.
%!test
%! v = __dfsearch_impl__ (sparse (1, 1), 1);
%! assert (v, 1);

## Edgeless 5-node: only the source is visited.
%!test
%! v = __dfsearch_impl__ (sparse (5, 5), 3);
%! assert (v, 3);

## Directed 3-cycle 1->2->3->1 from node 1.
%!test
%! A = sparse ([1 2 3], [2 3 1], 1, 3, 3);
%! v = __dfsearch_impl__ (A, 1);
%! assert (v, [1; 2; 3]);

## Directed 3-cycle from node 2: result rotates.
%!test
%! A = sparse ([1 2 3], [2 3 1], 1, 3, 3);
%! v = __dfsearch_impl__ (A, 2);
%! assert (v, [2; 3; 1]);

## Tie-break on ascending-index order for multi-fan-out root.
%!test
%! A = sparse ([1 1 1], [4 2 3], 1, 4, 4);
%! v = __dfsearch_impl__ (A, 1);
%! assert (v, [1; 2; 3; 4]);

## Reachability: nodes in a disjoint component are omitted.
%!test
%! A = sparse ([1 2 4], [2 3 5], 1, 5, 5);
%! v = __dfsearch_impl__ (A, 1);
%! assert (v, [1; 2; 3]);

## Undirected path (symmetric adjacency) 1-2-3 from node 1.
%!test
%! s = [1 2];
%! t = [2 3];
%! A = sparse ([s t], [t s], 1, 3, 3);
%! v = __dfsearch_impl__ (A, 1);
%! assert (v, [1; 2; 3]);

## DFS result is always a column vector.
%!test
%! A = sparse ([1 2], [2 3], 1, 3, 3);
%! v = __dfsearch_impl__ (A, 1);
%! assert (size (v), [3, 1]);

## Self-loop at source: result unchanged.
%!test
%! A = sparse ([1 1 2], [1 2 3], 1, 3, 3);
%! v = __dfsearch_impl__ (A, 1);
%! assert (v, [1; 2; 3]);

## DFS preorder on a small binary tree (directed edges).
%!test
%! A = sparse ([1 1 2 2 3 3], [2 3 4 5 6 7], 1, 7, 7);
%! v = __dfsearch_impl__ (A, 1);
%! assert (v, [1; 2; 4; 5; 3; 6; 7]);
