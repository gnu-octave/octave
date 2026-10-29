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
## @deftypefn  {} {@var{D} =} __distances_bellman_ford__ (@var{W})
## @deftypefnx {} {@var{D} =} __distances_bellman_ford__ (@var{W}, @var{sources})
## Private helper: run the Bellman-Ford single-source shortest-path
## algorithm from each row of @var{sources} on the directed graph
## encoded by the square matrix @var{W} of edge weights, and return
## the resulting distance matrix @var{D}.  Unlike Dijkstra,
## Bellman-Ford supports negative edge weights (but not negative
## cycles).
##
## @var{W} is a square matrix (sparse or dense); @var{W}(i, j) is the
## weight of the edge from node @math{i} to node @math{j}, and a zero
## entry means no edge (consistent with Octave sparse storage).  The
## caller is responsible for encoding undirected edges symmetrically
## when that is the desired semantics; however, a single undirected
## negative edge is a negative cycle by itself (u-v-u) and will be
## detected.
##
## @var{sources} is an optional column vector of positive integer node
## indices.  When omitted or empty, Bellman-Ford runs from every node
## and the result is the all-pairs distance matrix.
##
## Raises @code{Octave:invalid-input-arg} with a message matching
## @code{negative cycle} whenever any reachable edge still offers an
## improving relaxation after @math{N - 1} passes (the textbook
## negative-cycle detection).  Self-loops are not explicitly checked;
## a negative self-loop also fires this check because looping through
## it repeatedly keeps shrinking the node's distance.
##
## This helper is internal to the graph/digraph classes and is not
## intended to be called directly by user code.
## @seealso{distances, graph, digraph}
## @end deftypefn

function D = __distances_bellman_ford__ (W, sources)

  if (nargin < 1)
    print_usage ();
  endif

  N = size (W, 1);

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

  ## Extract the edge list once: column-major find() traverses all
  ## non-zero entries as directed edges.
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

  for k = 1:K
    src = src_list(k);
    dist = inf (1, N);
    dist(src) = 0;

    ## N - 1 relaxation passes.  Break early if a pass changes
    ## nothing.
    for iter = 1:(N - 1)
      changed = false;
      if (E > 0)
        ## Vectorized relaxation.  A proposed new distance through
        ## edge (u, v) is dist(u) + w; keep the minimum of the
        ## current dist(v) and all proposals.
        ##
        ## Because multiple edges can target the same v within a
        ## single pass, we use accumarray with @min to combine
        ## proposals before updating dist.
        prop = dist(ii) + ww.';
        ## accumarray groups proposals by target j.
        new_min = accumarray (jj, prop, [N, 1], @min, Inf).';
        new_dist = min (dist, new_min);
        if (any (new_dist != dist))
          changed = true;
          dist = new_dist;
        endif
      endif
      if (! changed)
        break;
      endif
    endfor

    ## One more pass: if any edge can still be relaxed, there is a
    ## negative cycle reachable from src.
    if (E > 0)
      prop = dist(ii) + ww.';
      if (any (prop < dist(jj)))
        error ("Octave:invalid-input-arg", ...
               "distances: graph contains a negative cycle");
      endif
    endif

    D(k, :) = dist;
  endfor

endfunction


## ------------------------------------------------------------------
## BIST blocks
## ------------------------------------------------------------------

## Empty input yields 0x0.
%!test
%! D = __distances_bellman_ford__ (sparse (0, 0));
%! assert (size (D), [0, 0]);

## 1x1 graph with no edges: D = 0.
%!test
%! D = __distances_bellman_ford__ (sparse (1, 1));
%! assert (D, 0);

## Single directed positive-weight edge.
%!test
%! W = sparse ([1], [2], [7], 2, 2);
%! D = __distances_bellman_ford__ (W);
%! assert (D, [0, 7; Inf, 0]);

## Weighted 3-cycle all positive.
%!test
%! W = sparse ([1, 2, 3], [2, 3, 1], [5, 10, 15], 3, 3);
%! D = __distances_bellman_ford__ (W);
%! assert (D, [0 5 15; 25 0 10; 15 20 0]);

## Directed graph with a negative edge (no negative cycle).
%!test
%! W = sparse ([1, 1, 2], [2, 3, 3], [5, 10, -3], 3, 3);
%! D = __distances_bellman_ford__ (W);
%! assert (D(1, 3), 2);

## Negative-weight DAG: distance matches direct path sums.
%!test
%! W = sparse ([1, 2], [2, 3], [-2, -3], 3, 3);
%! D = __distances_bellman_ford__ (W);
%! assert (D(1, 3), -5);
%! assert (D(2, 3), -3);
%! assert (isinf (D(3, 1)));

## Negative cycle in a directed 3-cycle: errors.
%!error <negative cycle>
%! W = sparse ([1, 2, 3], [2, 3, 1], [1, 1, -10], 3, 3);
%! __distances_bellman_ford__ (W);

## Negative self-loop: errors.
%!error <negative cycle>
%! W = sparse ([1], [1], [-1], 2, 2);
%! __distances_bellman_ford__ (W);

## Undirected negative edge encoded symmetrically is a neg cycle.
%!error <negative cycle>
%! W = sparse ([1, 2], [2, 1], [-1, -1], 2, 2);
%! __distances_bellman_ford__ (W);

## Self-loop with zero/positive weight is OK.
%!test
%! W = sparse ([1, 1], [1, 2], [0.5, 3], 2, 2);
%! D = __distances_bellman_ford__ (W);
%! assert (D(1, 2), 3);

## sources arg: single source.
%!test
%! W = sparse ([1, 2], [2, 3], [5, -3], 3, 3);
%! D = __distances_bellman_ford__ (W, 1);
%! assert (size (D), [1, 3]);
%! assert (D(1, 3), 2);

## sources arg: multiple sources preserved in order.
%!test
%! W = sparse ([1, 2], [2, 3], [5, -3], 3, 3);
%! D = __distances_bellman_ford__ (W, [3; 1]);
%! assert (size (D), [2, 3]);
%! assert (D(2, 3), 2);

## Empty sources falls through to all-pairs.
%!test
%! W = sparse ([1, 2], [2, 3], [1, 1], 3, 3);
%! D1 = __distances_bellman_ford__ (W, []);
%! D2 = __distances_bellman_ford__ (W);
%! assert (D1, D2);

## Dense input accepted.
%!test
%! W = [0 5 0; 0 0 -3; 0 0 0];
%! D = __distances_bellman_ford__ (W);
%! assert (D(1, 3), 2);
%! assert (D(1, 2), 5);

## No-arg call errors via print_usage.
%!error __distances_bellman_ford__ ()
