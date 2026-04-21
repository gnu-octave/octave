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
## @deftypefn {} {@var{D} =} __distances_dijkstra__ (@var{W})
## Private helper: run Dijkstra's single-source shortest-path algorithm
## from every node of a graph encoded by the square sparse weight
## matrix @var{W} and return the resulting all-pairs distance matrix
## @var{D}.
##
## @var{W}(i, j) is the weight of the edge from node @math{i} to node
## @math{j}; a zero entry means no edge (consistent with Octave's
## sparse storage).  @var{W} must have non-negative entries.  For an
## undirected graph the caller supplies a symmetric @var{W}; for a
## directed graph @var{W} is typically asymmetric.
##
## @var{D} is a dense @code{N}-by-@code{N} double matrix where
## @var{D}(i, j) is the length of a shortest directed path from
## @math{i} to @math{j} under the weights in @var{W}, or @code{Inf}
## when @math{j} is not reachable from @math{i}.  @var{D}(i, i) is
## always @code{0}; self-loops in @var{W} do not affect the result.
##
## Raises @code{Octave:invalid-input-arg} when @var{W} contains a
## negative entry: Dijkstra's algorithm is not valid with negative
## edge weights.  The caller is responsible for any preprocessing
## required to collapse parallel edges (for example, taking the
## minimum weight of parallel edges in a multigraph).
##
## This helper is internal to the graph/digraph classes and is not
## intended to be called directly by user code.
## @seealso{distances, graph, digraph}
## @end deftypefn

function D = __distances_dijkstra__ (W)

  if (nargin < 1)
    print_usage ();
  endif

  N = size (W, 1);
  if (N == 0)
    D = zeros (0, 0);
    return;
  endif

  ## Validate non-negative weights.  Use nonzeros() so zero entries in
  ## sparse storage (i.e. non-edges) are skipped automatically.
  if (any (nonzeros (W) < 0))
    error ("Octave:invalid-input-arg", ...
           "distances: negative edge weights are not supported by the default Dijkstra method");
  endif

  D = inf (N, N);
  ## Precompute row-wise neighbor lists so every source doesn't
  ## re-scan the whole sparse matrix.  For each node u we collect
  ## its outgoing (non-zero) neighbors and their weights once.
  neighbors = cell (N, 1);
  weights = cell (N, 1);
  for u = 1:N
    [~, nb, w] = find (W(u, :));
    neighbors{u} = nb(:);
    weights{u} = w(:);
  endfor

  for src = 1:N
    dist = inf (N, 1);
    dist(src) = 0;
    visited = false (N, 1);

    ## Classic Dijkstra with O(N) extract-min per iteration (good
    ## enough for the dense result we're building).  A binary-heap
    ## priority queue would improve the asymptotic cost but adds
    ## significant implementation complexity in pure .m; revisit if
    ## performance warrants it.
    for iter = 1:N
      cand = dist;
      cand(visited) = Inf;
      [min_d, u] = min (cand);
      if (! isfinite (min_d))
        break;
      endif
      visited(u) = true;
      nb = neighbors{u};
      w = weights{u};
      for k = 1:numel (nb)
        v = nb(k);
        if (! visited(v))
          alt = dist(u) + w(k);
          if (alt < dist(v))
            dist(v) = alt;
          endif
        endif
      endfor
    endfor

    D(src, :) = dist.';
  endfor

endfunction


## ------------------------------------------------------------------
## BIST blocks
## ------------------------------------------------------------------

## Empty input yields 0x0 result.
%!test
%! D = __distances_dijkstra__ (sparse (0, 0));
%! assert (size (D), [0, 0]);

## 1x1 input with no self-loop: D = 0.
%!test
%! D = __distances_dijkstra__ (sparse (1, 1));
%! assert (D, 0);

## Single directed edge 1->2, weight 1.
%!test
%! W = sparse ([1], [2], [1], 2, 2);
%! D = __distances_dijkstra__ (W);
%! assert (D, [0, 1; Inf, 0]);

## Symmetric edge 1-2, weight 1 (undirected-style input).
%!test
%! W = sparse ([1, 2], [2, 1], [1, 1], 2, 2);
%! D = __distances_dijkstra__ (W);
%! assert (D, [0, 1; 1, 0]);

## Weighted 3-node directed cycle.
%!test
%! W = sparse ([1, 2, 3], [2, 3, 1], [5, 10, 15], 3, 3);
%! D = __distances_dijkstra__ (W);
%! assert (D, [0 5 15; 25 0 10; 15 20 0]);

## Isolated node yields Inf.
%!test
%! W = sparse ([1], [2], [1], 3, 3);
%! D = __distances_dijkstra__ (W);
%! assert (D(1, 3), Inf);
%! assert (D(2, 3), Inf);
%! assert (D(3, 1), Inf);

## Self-loop weight is ignored.
%!test
%! W = sparse ([1, 1], [1, 2], [99, 1], 2, 2);
%! D = __distances_dijkstra__ (W);
%! assert (D(1, 1), 0);
%! assert (D(1, 2), 1);

## Chooses shorter two-hop path over direct heavy edge.
%!test
%! W = sparse ([1, 1, 2], [2, 3, 3], [1, 100, 1], 3, 3);
%! D = __distances_dijkstra__ (W);
%! assert (D(1, 3), 2);

## Negative weight errors.
%!error <negative edge weights>
%! W = sparse ([1, 2], [2, 3], [1, -1], 3, 3);
%! __distances_dijkstra__ (W);

## Dense double input accepted (not just sparse).
%!test
%! W = [0 1 0; 0 0 1; 0 0 0];
%! D = __distances_dijkstra__ (W);
%! assert (D, [0 1 2; Inf 0 1; Inf Inf 0]);

## No-arg call errors via print_usage.
%!error __distances_dijkstra__ ()
