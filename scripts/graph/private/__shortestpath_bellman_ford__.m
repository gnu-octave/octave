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
## @deftypefn {} {[@var{path}, @var{d}] =} __shortestpath_bellman_ford__ (@var{W}, @var{s}, @var{t})
## Private helper: compute a single shortest path from node @var{s} to
## node @var{t} using the Bellman-Ford algorithm with predecessor
## tracking.  Unlike Dijkstra, Bellman-Ford supports negative edge
## weights (but not negative cycles).
##
## @var{W} is the square sparse or dense weight matrix: @var{W}(i, j)
## is the weight of the edge from node @math{i} to node @math{j}; a
## zero entry means no edge.  For an undirected graph the caller
## supplies a symmetric @var{W}; for a directed graph @var{W} is
## typically asymmetric.
##
## @var{s} and @var{t} are scalar positive integer node indices in
## @code{1:size (W, 1)}.
##
## The first return value @var{path} is a column vector of node
## indices giving the shortest path from @var{s} to @var{t} under
## @var{W}, in forward order.  When @var{t} is not reachable from
## @var{s}, @var{path} is a @code{0}-by-@code{1} empty column vector
## and @var{d} is @code{Inf}.  When @code{s == t}, @var{path} is the
## 1-element column vector @code{[@var{s}]} and @var{d} is @code{0}.
## Self-loops with non-negative weight in @var{W} are ignored.
##
## The second return value @var{d} is the scalar sum of edge weights
## along @var{path}, or @code{Inf} when unreachable.
##
## Raises @code{Octave:invalid-input-arg} with a message matching
## @code{negative cycle} whenever a negative cycle is reachable from
## @var{s} (which makes the shortest-path problem ill-defined).
##
## This helper is internal to the graph/digraph classes and is not
## intended to be called directly by user code.
## @seealso{shortestpath, graph, digraph, __shortestpathtree_bellman_ford__}
## @end deftypefn

function [path, d] = __shortestpath_bellman_ford__ (W, s, t)

  if (nargin != 3)
    print_usage ();
  endif

  s = double (s);
  t = double (t);

  ## Trivial case: source == target.
  if (s == t)
    path = s;
    d = 0;
    return;
  endif

  ## Delegate the shortest-path-tree computation (and the negative
  ## cycle detection) to the tree helper, then reconstruct the path
  ## by walking predecessors backward from t to s.
  [pred, dist] = __shortestpathtree_bellman_ford__ (W, s);

  d = dist(t);
  if (! isfinite (d))
    path = zeros (0, 1);
    return;
  endif

  ## Reconstruct path by walking predecessors backwards from t to s.
  N = numel (pred);
  buf = zeros (N, 1);
  k = 1;
  buf(k) = t;
  while (buf(k) != s)
    p = pred(buf(k));
    if (p == 0)
      ## Defensive: should be unreachable given dist(t) < Inf above.
      path = zeros (0, 1);
      return;
    endif
    k = k + 1;
    buf(k) = p;
  endwhile
  path = flipud (buf(1:k));

endfunction


## ------------------------------------------------------------------
## BIST blocks
## ------------------------------------------------------------------

## Source == target returns [s] with d=0.
%!test
%! W = sparse ([1 2], [2 3], [1 1], 3, 3);
%! [P, d] = __shortestpath_bellman_ford__ (W, 2, 2);
%! assert (P, 2);
%! assert (d, 0);

## Single edge 1->2, unweighted: path is [1; 2].
%!test
%! W = sparse (1, 2, 1, 2, 2);
%! [P, d] = __shortestpath_bellman_ford__ (W, 1, 2);
%! assert (P, [1; 2]);
%! assert (d, 1);

## Chain 1->2->3: path 1 to 3 is [1; 2; 3], d=2.
%!test
%! W = sparse ([1 2], [2 3], [1 1], 3, 3);
%! [P, d] = __shortestpath_bellman_ford__ (W, 1, 3);
%! assert (P, [1; 2; 3]);
%! assert (d, 2);

## Single directed negative edge 1->2 weight -5: d = -5.
%!test
%! W = sparse (1, 2, -5, 2, 2);
%! [P, d] = __shortestpath_bellman_ford__ (W, 1, 2);
%! assert (P, [1; 2]);
%! assert (d, -5);

## Negative edge in a chain: 1->2(5), 2->3(-3). Path [1;2;3], d=2.
%!test
%! W = sparse ([1 2], [2 3], [5, -3], 3, 3);
%! [P, d] = __shortestpath_bellman_ford__ (W, 1, 3);
%! assert (P, [1; 2; 3]);
%! assert (d, 2);

## Negative edge beats positive alternative: 1->2(5), 2->3(-3),
## 1->3(10).  Path [1;2;3] with d=2 beats direct d=10.
%!test
%! W = sparse ([1 2 1], [2 3 3], [5, -3, 10], 3, 3);
%! [P, d] = __shortestpath_bellman_ford__ (W, 1, 3);
%! assert (P, [1; 2; 3]);
%! assert (d, 2);

## Unreachable: returns empty 0x1 path, d=Inf.
%!test
%! W = sparse (3, 3);
%! [P, d] = __shortestpath_bellman_ford__ (W, 1, 2);
%! assert (size (P), [0, 1]);
%! assert (d, Inf);

## Disjoint components: unreachable.
%!test
%! W = sparse ([1 3], [2 4], [1 1], 4, 4);
%! [P, d] = __shortestpath_bellman_ford__ (W, 1, 3);
%! assert (size (P), [0, 1]);
%! assert (d, Inf);

## Direction respected: reverse of 1->2 is unreachable.
%!test
%! W = sparse (1, 2, 1, 2, 2);
%! [P, d] = __shortestpath_bellman_ford__ (W, 2, 1);
%! assert (size (P), [0, 1]);
%! assert (d, Inf);

## Self-loop on source node (non-negative) is ignored.
%!test
%! W = sparse ([1 1], [1 2], [99 1], 2, 2);
%! [P, d] = __shortestpath_bellman_ford__ (W, 1, 2);
%! assert (P, [1; 2]);
%! assert (d, 1);

## Negative cycle in a directed 3-cycle errors.
%!error <negative cycle>
%! W = sparse ([1 2 3], [2 3 1], [1 1 -10], 3, 3);
%! __shortestpath_bellman_ford__ (W, 1, 2);

## Negative self-loop errors.
%!error <negative cycle>
%! W = sparse ([1 1], [1 2], [-1 1], 2, 2);
%! __shortestpath_bellman_ford__ (W, 1, 2);

## Undirected negative edge (symmetric) is a neg cycle.
%!error <negative cycle>
%! W = sparse ([1 2], [2 1], [-1 -1], 2, 2);
%! __shortestpath_bellman_ford__ (W, 1, 2);

## Dense input accepted.
%!test
%! W = [0 1 0; 0 0 -1; 0 0 0];
%! [P, d] = __shortestpath_bellman_ford__ (W, 1, 3);
%! assert (P, [1; 2; 3]);
%! assert (d, 0);

## No-arg call errors.
%!error __shortestpath_bellman_ford__ ()

## Wrong argcount errors.
%!error __shortestpath_bellman_ford__ (sparse (1, 1), 1)

## Symmetric input (undirected-style): 1-2-3 chain, 1 to 3 is [1;2;3].
%!test
%! W = sparse ([1 2 2 3], [2 1 3 2], [1 1 1 1], 3, 3);
%! [P, d] = __shortestpath_bellman_ford__ (W, 1, 3);
%! assert (P, [1; 2; 3]);
%! assert (d, 2);

## CLRS Figure 24.4 example: 1->5 path [1 3 4 2 5] with d = -2.
%!test
%! s = [1 1 2 2 2 3 3 4 5 5];
%! t = [2 3 3 4 5 4 5 2 1 4];
%! w = [6 7 8 5 -4 -3 9 -2 2 7];
%! W = sparse (s, t, w, 5, 5);
%! [P, d] = __shortestpath_bellman_ford__ (W, 1, 5);
%! assert (P, [1; 3; 4; 2; 5]);
%! assert (d, -2);
