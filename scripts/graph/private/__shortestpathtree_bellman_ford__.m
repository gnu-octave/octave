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
## @deftypefn {} {[@var{pred}, @var{dist}] =} __shortestpathtree_bellman_ford__ (@var{W}, @var{s})
## Private helper: compute the single-source shortest path tree from
## node @var{s} using the Bellman-Ford algorithm with predecessor
## tracking.  Unlike Dijkstra, Bellman-Ford supports negative edge
## weights (but not negative cycles).
##
## @var{W} is the square sparse or dense weight matrix: @var{W}(i, j)
## is the weight of the edge from node @math{i} to node @math{j}; a
## zero entry means no edge.  For an undirected graph the caller
## supplies a symmetric @var{W}; for a directed graph @var{W} is
## typically asymmetric.  Negative weights are permitted provided no
## negative cycle is reachable from @var{s}.
##
## @var{s} is a scalar positive integer node index in
## @code{1:size (W, 1)}.
##
## The first return value @var{pred} is an @code{N}-by-@code{1} column
## vector of predecessor indices: @code{pred(i)} is the index of the
## predecessor of node @math{i} in the shortest path tree rooted at
## @var{s}, or @code{0} if node @math{i} is not reachable from
## @var{s}.  @code{pred(@var{s})} is @code{0} (the source has no
## predecessor).  Self-loops in @var{W} are ignored for non-negative
## weights; a negative self-loop reachable from @var{s} constitutes a
## negative cycle and raises an error.
##
## The second return value @var{dist} is an @code{N}-by-@code{1} column
## vector of distances: @code{dist(i)} is the length of the shortest
## path from @var{s} to node @math{i} under @var{W}, @code{Inf} when
## @math{i} is not reachable, and @code{0} for @math{i == @var{s}}.
##
## Raises @code{Octave:invalid-input-arg} with a message matching
## @code{negative cycle} whenever any reachable edge still offers an
## improving relaxation after @math{N - 1} passes.
##
## This helper is internal to the graph/digraph classes and is not
## intended to be called directly by user code.
## @seealso{shortestpathtree, graph, digraph, __distances_bellman_ford__}
## @end deftypefn

function [pred, dist] = __shortestpathtree_bellman_ford__ (W, s)

  if (nargin != 2)
    print_usage ();
  endif

  N = size (W, 1);
  s = double (s);

  dist = inf (N, 1);
  pred = zeros (N, 1);

  if (N == 0)
    return;
  endif

  dist(s) = 0;

  ## Extract the edge list once via column-major find().
  [ii, jj, ww] = find (W);
  ii = double (ii(:));
  jj = double (jj(:));
  ww = double (ww(:));
  E = numel (ii);

  if (E == 0)
    return;
  endif

  ## Any negative self-loop is a one-node negative cycle.  Firing the
  ## error here is consistent with __distances_bellman_ford__.
  self_neg = (ii == jj) & (ww < 0);
  if (any (self_neg))
    error ("Octave:invalid-input-arg", ...
           "shortestpathtree: graph contains a negative cycle");
  endif

  ## N - 1 relaxation passes with predecessor tracking.  We need the
  ## argmin over all edges ending at each target to update pred, so
  ## use a scalar loop over edges inside each pass.  Early exit when
  ## no update happens in a pass.
  for iter = 1:(N - 1)
    changed = false;
    for e = 1:E
      u = ii(e);
      v = jj(e);
      w = ww(e);
      du = dist(u);
      if (isfinite (du))
        alt = du + w;
        if (alt < dist(v))
          dist(v) = alt;
          pred(v) = u;
          changed = true;
        endif
      endif
    endfor
    if (! changed)
      break;
    endif
  endfor

  ## Negative-cycle detection: one more pass, no relaxation permitted.
  for e = 1:E
    u = ii(e);
    v = jj(e);
    w = ww(e);
    du = dist(u);
    if (isfinite (du) && (du + w < dist(v)))
      error ("Octave:invalid-input-arg", ...
             "shortestpathtree: graph contains a negative cycle");
    endif
  endfor

endfunction


## ------------------------------------------------------------------
## BIST blocks
## ------------------------------------------------------------------

## Empty graph: N=0 returns zero-length pred and dist.
%!test
%! W = sparse (0, 0);
%! [p, d] = __shortestpathtree_bellman_ford__ (W, 1);
%! assert (size (p), [0, 1]);
%! assert (size (d), [0, 1]);

## Single node: pred is 0, dist is 0.
%!test
%! W = sparse (1, 1);
%! [p, d] = __shortestpathtree_bellman_ford__ (W, 1);
%! assert (p, 0);
%! assert (d, 0);

## Chain 1->2->3: from 1, pred = [0; 1; 2], dist = [0; 1; 2].
%!test
%! W = sparse ([1 2], [2 3], [1 1], 3, 3);
%! [p, d] = __shortestpathtree_bellman_ford__ (W, 1);
%! assert (p, [0; 1; 2]);
%! assert (d, [0; 1; 2]);

## Unreachable: from 1 on an edgeless 3-node W, only dist(1)==0.
%!test
%! W = sparse (3, 3);
%! [p, d] = __shortestpathtree_bellman_ford__ (W, 1);
%! assert (p, [0; 0; 0]);
%! assert (d, [0; Inf; Inf]);

## Disjoint components: from 1, only component {1,2} reachable.
%!test
%! W = sparse ([1 3], [2 4], [1 1], 4, 4);
%! [p, d] = __shortestpathtree_bellman_ford__ (W, 1);
%! assert (p, [0; 1; 0; 0]);
%! assert (d, [0; 1; Inf; Inf]);

## Negative edge but no negative cycle: 1->2 weight 5, 2->3 weight -3.
## From 1: pred = [0; 1; 2], dist = [0; 5; 2].
%!test
%! W = sparse ([1 2], [2 3], [5, -3], 3, 3);
%! [p, d] = __shortestpathtree_bellman_ford__ (W, 1);
%! assert (p, [0; 1; 2]);
%! assert (d, [0; 5; 2]);

## Negative edge preferred over positive alternative.  1->2 (5),
## 2->3 (-3), 1->3 (10).  From 1, dist(3) = 2 via 1->2->3, not 10.
%!test
%! W = sparse ([1 2 1], [2 3 3], [5 -3 10], 3, 3);
%! [p, d] = __shortestpathtree_bellman_ford__ (W, 1);
%! assert (d, [0; 5; 2]);
%! assert (p(3), 2);

## Negative cycle (directed 3-cycle with sum < 0) errors.
%!error <negative cycle>
%! W = sparse ([1 2 3], [2 3 1], [1 1 -10], 3, 3);
%! __shortestpathtree_bellman_ford__ (W, 1);

## Negative self-loop errors.
%!error <negative cycle>
%! W = sparse ([1 1], [1 2], [-1, 1], 2, 2);
%! __shortestpathtree_bellman_ford__ (W, 1);

## Undirected negative edge encoded symmetrically is a neg cycle.
%!error <negative cycle>
%! W = sparse ([1, 2], [2, 1], [-1, -1], 2, 2);
%! __shortestpathtree_bellman_ford__ (W, 1);

## Self-loop with zero/positive weight is fine.
%!test
%! W = sparse ([1 1], [1 2], [0.5 1], 2, 2);
%! [p, d] = __shortestpathtree_bellman_ford__ (W, 1);
%! assert (d, [0; 1]);

## Dense input accepted.
%!test
%! W = [0 1 0; 0 0 1; 0 0 0];
%! [p, d] = __shortestpathtree_bellman_ford__ (W, 1);
%! assert (p, [0; 1; 2]);
%! assert (d, [0; 1; 2]);

## No-args call errors.
%!error __shortestpathtree_bellman_ford__ ()

## Wrong argcount errors.
%!error __shortestpathtree_bellman_ford__ (sparse (1, 1))

## Classic CLRS Figure 24.4 Bellman-Ford example.  Node mapping is
## s=1, t=2, y=3, x=4, z=5.  Edges: 1->2(6), 1->3(7), 2->3(8),
## 2->4(5), 2->5(-4), 3->4(-3), 3->5(9), 4->2(-2), 5->1(2), 5->4(7).
## Expected from s=1: dist = [0, 2, 7, 4, -2]; pred = [0, 4, 1, 3, 2].
%!test
%! s = [1 1 2 2 2 3 3 4 5 5];
%! t = [2 3 3 4 5 4 5 2 1 4];
%! w = [6 7 8 5 -4 -3 9 -2 2 7];
%! W = sparse (s, t, w, 5, 5);
%! [p, d] = __shortestpathtree_bellman_ford__ (W, 1);
%! assert (d, [0; 2; 7; 4; -2]);
%! assert (p, [0; 4; 1; 3; 2]);
