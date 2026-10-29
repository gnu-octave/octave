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
## @deftypefn {} {@var{D} =} __distances_johnson__ (@var{W})
## Private helper: run Johnson's all-pairs shortest-path algorithm on
## the directed graph encoded by the square matrix @var{W} of edge
## weights, and return the resulting @math{N}-by-@math{N} dense double
## distance matrix @var{D}.  Johnson's algorithm handles graphs with
## negative edge weights (but not negative cycles) in
## @math{O (V^2 \log V + V E)} time, which is asymptotically better
## than the @math{O (V^2 E)} cost of running Bellman-Ford from every
## source, especially for sparse graphs.
##
## @var{W} is a square matrix (sparse or dense); @var{W}(i, j) is the
## weight of the edge from node @math{i} to node @math{j}, and a zero
## entry means no edge (consistent with Octave sparse storage).  The
## caller is responsible for encoding undirected edges symmetrically
## when that is the desired semantics; note however that a single
## undirected negative edge encoded symmetrically is a negative cycle
## (u-v-u) and will be detected.
##
## The algorithm proceeds in four steps:
## @enumerate
## @item Attach a virtual source node @math{q} with weight-0 edges to
## every real node.
## @item Run Bellman-Ford from @math{q} to compute
## @code{h(v) = dist (q, v)} for every @math{v}.  A negative cycle
## reachable from @math{q} (equivalently, anywhere in the graph)
## raises an error.
## @item Reweight every edge @math{(u, v)} to
## @code{w'(u, v) = w (u, v) + h (u) - h (v)}, guaranteed non-negative.
## @item Run Dijkstra from every real source on the reweighted graph,
## then recover the original distances by the inverse shift
## @code{d(u, v) = d'(u, v) - h(u) + h(v)}.
## @end enumerate
##
## The returned matrix @var{D} is a dense double @math{N}-by-@math{N}
## matrix; @var{D}(i, j) is the length of a shortest directed path
## from node @math{i} to node @math{j} under the weights in @var{W},
## or @code{Inf} when @math{j} is not reachable from @math{i}.  The
## diagonal @var{D}(i, i) is always @code{0}; self-loops in @var{W}
## do not affect the result.  Empty input @code{@var{W} = sparse (0, 0)}
## returns the @code{0}-by-@code{0} double matrix.
##
## Raises @code{Octave:invalid-input-arg} with a message matching
## @code{negative cycle} whenever any reachable edge still offers an
## improving relaxation after @math{N - 1} passes, or when a negative
## self-loop is present.
##
## This helper is internal to the graph/digraph classes and is not
## intended to be called directly by user code.
## @seealso{distances, graph, digraph, __distances_bellman_ford__,
## __distances_dijkstra__}
## @end deftypefn

function D = __distances_johnson__ (W)

  if (nargin < 1)
    print_usage ();
  endif

  N = size (W, 1);

  if (N == 0)
    D = zeros (0, 0);
    return;
  endif

  ## Extract the edge list once.  Column-major find() traverses the
  ## non-zero entries as directed edges in (src, dst) column order.
  [ii, jj, ww] = find (W);
  ii = double (ii(:));
  jj = double (jj(:));
  ww = double (ww(:));
  E = numel (ii);

  ## Any negative self-loop is a one-node negative cycle.
  if (E > 0)
    self_neg = (ii == jj) & (ww < 0);
    if (any (self_neg))
      error ("Octave:invalid-input-arg", ...
             "distances: graph contains a negative cycle");
    endif
  endif

  ## -------------------------------------------------------------
  ## Step 1: Compute vertex potentials h(v) via Bellman-Ford from a
  ## virtual source with weight-0 edges to every real node.  This is
  ## equivalent to running Bellman-Ford with h initialised to zero on
  ## every node -- the virtual edge @code{q -> v} of weight 0 makes
  ## @code{dist(v) <= 0} from the outset, and subsequent relaxations
  ## pull down further only where a negative-weighted path exists.
  ## -------------------------------------------------------------
  h = zeros (1, N);
  for iter = 1:(N - 1)
    changed = false;
    if (E > 0)
      prop = h(ii) + ww.';
      new_min = accumarray (jj, prop, [N, 1], @min, Inf).';
      new_h = min (h, new_min);
      if (any (new_h != h))
        changed = true;
        h = new_h;
      endif
    endif
    if (! changed)
      break;
    endif
  endfor

  ## Step 2: One extra pass detects a negative cycle reachable in the
  ## augmented graph (equivalently, anywhere in the real graph).
  if (E > 0)
    prop = h(ii) + ww.';
    if (any (prop < h(jj)))
      error ("Octave:invalid-input-arg", ...
             "distances: graph contains a negative cycle");
    endif
  endif

  ## -------------------------------------------------------------
  ## Step 3: Reweight each edge so that w'(u, v) >= 0.  The closed
  ## form is w'(u, v) = w(u, v) + h(u) - h(v).  Theoretically every
  ## reweighted value is non-negative; any tiny negative we see here
  ## is floating-point drift from summing potentials, so we clamp to
  ## zero with @code{max}.
  ## -------------------------------------------------------------
  if (E > 0)
    reweight = ww + h(ii).' - h(jj).';
    reweight = max (reweight, 0);
  else
    reweight = zeros (0, 1);
  endif

  ## -------------------------------------------------------------
  ## Step 4: Build adjacency lists for Dijkstra directly from the
  ## edge list (rather than going through @code{sparse (ii, jj,
  ## reweight, N, N)}) because Octave's sparse storage drops exact
  ## zero entries.  Johnson's reweighting routinely produces 0-weight
  ## edges exactly on the tight edges of the shortest-path tree from
  ## the virtual source, and those edges must be preserved for
  ## Dijkstra to find the correct paths.  Self-loops do not affect
  ## shortest paths and are skipped.
  ## -------------------------------------------------------------
  neighbors = cell (N, 1);
  weights_cell = cell (N, 1);
  for u = 1:N
    neighbors{u} = zeros (0, 1);
    weights_cell{u} = zeros (0, 1);
  endfor
  if (E > 0)
    ## Bucket edges by source; keep self-loops out.
    keep = (ii != jj);
    if (any (keep))
      src_k = ii(keep);
      dst_k = jj(keep);
      w_k = reweight(keep);
      ## Count edges per source to preallocate.
      src_count = accumarray (src_k, 1, [N, 1]);
      ## Sort by source so we can slice contiguous runs.
      [src_sorted, perm] = sort (src_k);
      dst_sorted = dst_k(perm);
      w_sorted = w_k(perm);
      offset = 1;
      for u = 1:N
        cnt = src_count(u);
        if (cnt > 0)
          neighbors{u} = dst_sorted(offset:offset + cnt - 1);
          weights_cell{u} = w_sorted(offset:offset + cnt - 1);
          offset = offset + cnt;
        endif
      endfor
    endif
  endif

  ## -------------------------------------------------------------
  ## Step 5: Run Dijkstra's algorithm from every real source on the
  ## reweighted graph, producing Dp(src, j) = reweighted shortest
  ## path distance from src to j.  The inner loop mirrors the
  ## implementation in @file{__distances_dijkstra__.m} but reads from
  ## the neighbor lists we just built so the 0-weight reweighted
  ## edges are preserved.
  ## -------------------------------------------------------------
  Dp = inf (N, N);
  for src = 1:N
    dist = inf (N, 1);
    dist(src) = 0;
    visited = false (N, 1);
    for iter = 1:N
      cand = dist;
      cand(visited) = Inf;
      [min_d, u] = min (cand);
      if (! isfinite (min_d))
        break;
      endif
      visited(u) = true;
      nb = neighbors{u};
      w = weights_cell{u};
      for j = 1:numel (nb)
        v = nb(j);
        if (! visited(v))
          alt = dist(u) + w(j);
          if (alt < dist(v))
            dist(v) = alt;
          endif
        endif
      endfor
    endfor
    Dp(src, :) = dist.';
  endfor

  ## -------------------------------------------------------------
  ## Step 6: Undo the reweighting for the final distance matrix.
  ## The closed form is d(u, v) = d'(u, v) - h(u) + h(v); leave
  ## unreachable entries as Inf.  The diagonal is then zeroed
  ## explicitly to absorb any floating-point drift from summing
  ## potentials.
  ## -------------------------------------------------------------
  D = Dp;
  reachable = isfinite (Dp);
  ## correction(u, v) = -h(u) + h(v); broadcast an N-by-1 with a
  ## 1-by-N via implicit expansion.
  correction = (-h).' + h;
  D(reachable) = Dp(reachable) + correction(reachable);
  D(1:N + 1:N * N) = 0;

endfunction


## ------------------------------------------------------------------
## BIST blocks
## ------------------------------------------------------------------

## Empty input yields 0x0.
%!test
%! D = __distances_johnson__ (sparse (0, 0));
%! assert (size (D), [0, 0]);

## 1x1 graph with no edges: D = 0.
%!test
%! D = __distances_johnson__ (sparse (1, 1));
%! assert (D, 0);

## Edgeless N-node graph: diagonal 0, off-diagonal Inf.
%!test
%! D = __distances_johnson__ (sparse (3, 3));
%! expected = [0 Inf Inf; Inf 0 Inf; Inf Inf 0];
%! assert (D, expected);

## Single directed positive-weight edge.
%!test
%! W = sparse (1, 2, 7, 2, 2);
%! D = __distances_johnson__ (W);
%! assert (D, [0, 7; Inf, 0]);

## Weighted 3-cycle all positive -- matches Bellman-Ford / Dijkstra.
%!test
%! W = sparse ([1 2 3], [2 3 1], [5 10 15], 3, 3);
%! D = __distances_johnson__ (W);
%! assert (D, [0 5 15; 25 0 10; 15 20 0]);

## Diagonal is zero even with self-loop.
%!test
%! W = sparse ([1 1 2], [1 2 3], [7 1 1], 3, 3);
%! D = __distances_johnson__ (W);
%! assert (diag (D), zeros (3, 1));
%! assert (D(1, 3), 2);

## Directed graph with a negative edge (no negative cycle).
%!test
%! W = sparse ([1 1 2], [2 3 3], [5 10 -3], 3, 3);
%! D = __distances_johnson__ (W);
%! assert (D(1, 2), 5);
%! assert (D(1, 3), 2);
%! assert (D(2, 3), -3);
%! assert (isinf (D(3, 1)));

## Negative-weight DAG: distance matches direct path sums.
%!test
%! W = sparse ([1 2], [2 3], [-2, -3], 3, 3);
%! D = __distances_johnson__ (W);
%! assert (D(1, 2), -2);
%! assert (D(1, 3), -5);
%! assert (D(2, 3), -3);
%! assert (isinf (D(3, 1)));
%! assert (isinf (D(3, 2)));

## Diamond with a negative shortcut edge that makes 0 reweighted:
## 1->2(1), 2->3(-1), 1->3(2).  d(1,2)=1, d(1,3)=0 via 1->2->3,
## d(2,3)=-1.  The reweighting of 2->3 to 0 must not be dropped.
%!test
%! W = sparse ([1 2 1], [2 3 3], [1 -1 2], 3, 3);
%! D = __distances_johnson__ (W);
%! assert (D(1, 2), 1);
%! assert (D(1, 3), 0);
%! assert (D(2, 3), -1);
%! assert (isinf (D(3, 1)));

## Results match Bellman-Ford on a general neg-weight digraph.
%!test
%! W = sparse ([1 1 2], [2 3 3], [5 10 -3], 3, 3);
%! D_j = __distances_johnson__ (W);
%! D_bf = __distances_bellman_ford__ (W);
%! assert (D_j, D_bf);

## Results match Dijkstra on an all-nonneg digraph.
%!test
%! W = sparse ([1 1 2 2 3], [2 3 3 4 4], [1 4 2 5 1], 4, 4);
%! D_j = __distances_johnson__ (W);
%! D_dij = __distances_dijkstra__ (W);
%! assert (D_j, D_dij);

## Negative cycle in a directed 3-cycle: errors.
%!error <negative cycle>
%! W = sparse ([1 2 3], [2 3 1], [1 1 -10], 3, 3);
%! __distances_johnson__ (W);

## Negative self-loop: errors.
%!error <negative cycle>
%! W = sparse ([1], [1], [-1], 2, 2);
%! __distances_johnson__ (W);

## Undirected negative edge encoded symmetrically is a neg cycle.
%!error <negative cycle>
%! W = sparse ([1 2], [2 1], [-1 -1], 2, 2);
%! __distances_johnson__ (W);

## Self-loop with zero/positive weight is OK; diagonal stays 0.
%!test
%! W = sparse ([1 1], [1 2], [0.5, 3], 2, 2);
%! D = __distances_johnson__ (W);
%! assert (D(1, 1), 0);
%! assert (D(1, 2), 3);

## Two disjoint components produce Inf across components.
%!test
%! W = sparse ([1 3], [2 4], [1 1], 4, 4);
%! D = __distances_johnson__ (W);
%! assert (D(1, 2), 1);
%! assert (D(3, 4), 1);
%! assert (isinf (D(1, 3)));
%! assert (isinf (D(1, 4)));
%! assert (isinf (D(3, 1)));
%! assert (isinf (D(3, 2)));

## Dense input accepted.
%!test
%! W = [0 5 0; 0 0 -3; 0 0 0];
%! D = __distances_johnson__ (W);
%! assert (D(1, 3), 2);
%! assert (D(1, 2), 5);
%! assert (D(2, 3), -3);

## Chain 1->2->...->10 unweighted, integer distances.
%!test
%! s = 1:9;  t = 2:10;
%! W = sparse (s, t, ones (1, 9), 10, 10);
%! D = __distances_johnson__ (W);
%! for i = 1:10
%!   for j = 1:10
%!     if (j >= i)
%!       assert (D(i, j), j - i);
%!     else
%!       assert (D(i, j), Inf);
%!     endif
%!   endfor
%! endfor

## CLRS Figure 24.4 canonical example:
##   nodes s=1, t=2, y=3, x=4, z=5
##   edges 1->2(6), 1->3(7), 2->3(8), 2->4(5), 2->5(-4),
##         3->4(-3), 3->5(9), 4->2(-2), 5->1(2), 5->4(7)
## Row 1 of the all-pairs distance matrix is the textbook vector
## [0, 2, 7, 4, -2].
%!test
%! s = [1 1 2 2 2 3 3 4 5 5];
%! t = [2 3 3 4 5 4 5 2 1 4];
%! w = [6 7 8 5 -4 -3 9 -2 2 7];
%! W = sparse (s, t, w, 5, 5);
%! D = __distances_johnson__ (W);
%! assert (D(1, :), [0, 2, 7, 4, -2]);
%! ## Must match Bellman-Ford's all-pairs result bit-for-bit.
%! D_bf = __distances_bellman_ford__ (W);
%! assert (D, D_bf);

## Reweighted-to-zero edges are preserved: construct a graph where
## h(v) = v of the BF potentials cancels the edge weight exactly.
## Edges 1->2(-1), 2->3(-1), 1->3(5) -- after BF from virtual source,
## h = [0, -1, -2] so reweight(1->2) = -1 + 0 - (-1) = 0,
## reweight(2->3) = -1 + (-1) - (-2) = 0, reweight(1->3) = 5 + 0 -
## (-2) = 7.  Johnson's must keep the 0-weight edges in Dijkstra and
## return d(1,3) = -2 (via 1->2->3).
%!test
%! W = sparse ([1 2 1], [2 3 3], [-1 -1 5], 3, 3);
%! D = __distances_johnson__ (W);
%! assert (D(1, 2), -1);
%! assert (D(2, 3), -1);
%! assert (D(1, 3), -2);

## No-arg call errors via print_usage.
%!error __distances_johnson__ ()
