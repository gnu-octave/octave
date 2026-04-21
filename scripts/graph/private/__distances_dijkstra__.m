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
## @deftypefn  {} {@var{D} =} __distances_dijkstra__ (@var{W})
## @deftypefnx {} {@var{D} =} __distances_dijkstra__ (@var{W}, @var{sources})
## Private helper: run Dijkstra's single-source shortest-path algorithm
## from the specified @var{sources} of a graph encoded by the square
## sparse weight matrix @var{W} and return the resulting distance
## matrix @var{D}.  Without @var{sources}, Dijkstra runs from every
## node and the result is the all-pairs distance matrix.
##
## @var{W}(i, j) is the weight of the edge from node @math{i} to node
## @math{j}; a zero entry means no edge (consistent with Octave's
## sparse storage).  @var{W} must have non-negative entries.  For an
## undirected graph the caller supplies a symmetric @var{W}; for a
## directed graph @var{W} is typically asymmetric.
##
## @var{sources} is an optional column vector of positive integer node
## indices (each in @code{1:size (@var{W}, 1)}) that selects which
## sources to run Dijkstra from.  When omitted or empty, Dijkstra runs
## from all @code{N = size (@var{W}, 1)} nodes.  The output
## @var{D} is a dense @code{numel (@var{sources})}-by-@code{N} double
## matrix in which @var{D}(k, j) is the length of a shortest directed
## path from @code{@var{sources}(k)} to node @math{j} under the
## weights in @var{W}, or @code{Inf} when @math{j} is not reachable
## from that source.  @var{D}(k, @var{sources}(k)) is always
## @code{0}; self-loops in @var{W} do not affect the result.
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

function D = __distances_dijkstra__ (W, sources)

  if (nargin < 1)
    print_usage ();
  endif

  N = size (W, 1);

  if (nargin < 2 || isempty (sources))
    all_sources = true;
    src_list = (1:N).';
  else
    all_sources = false;
    src_list = double (sources(:));
  endif

  K = numel (src_list);

  if (N == 0)
    D = zeros (K, 0);
    return;
  endif

  ## Validate non-negative weights.  Use nonzeros() so zero entries in
  ## sparse storage (i.e. non-edges) are skipped automatically.
  if (any (nonzeros (W) < 0))
    error ("Octave:invalid-input-arg", ...
           "distances: negative edge weights are not supported by the default Dijkstra method");
  endif

  D = inf (K, N);

  if (K == 0)
    return;
  endif

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

  for k = 1:K
    src = src_list(k);
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

    D(k, :) = dist.';
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

## sources arg: single-source Dijkstra yields a 1xN row.
%!test
%! W = sparse ([1, 2, 3], [2, 3, 1], [5, 10, 15], 3, 3);
%! D = __distances_dijkstra__ (W, 1);
%! assert (size (D), [1, 3]);
%! assert (D, [0, 5, 15]);

## sources arg: multiple sources yields a KxN matrix.
%!test
%! W = sparse ([1, 2, 3], [2, 3, 1], [5, 10, 15], 3, 3);
%! D = __distances_dijkstra__ (W, [1; 3]);
%! assert (size (D), [2, 3]);
%! assert (D(1, :), [0, 5, 15]);
%! assert (D(2, :), [15, 20, 0]);

## sources arg: empty sources yields 0xN empty.
%!test
%! W = sparse ([1, 2, 3], [2, 3, 1], [5, 10, 15], 3, 3);
%! D = __distances_dijkstra__ (W, []);
%! assert (size (D), [3, 3]);
%! ## Actually empty -> default (all sources) kicks in; documented
%! ## behaviour.  Confirm full all-pairs result.
%! assert (D, [0 5 15; 25 0 10; 15 20 0]);

## sources arg: source order preserved.
%!test
%! W = sparse ([1, 2, 3], [2, 3, 1], [5, 10, 15], 3, 3);
%! D = __distances_dijkstra__ (W, [3; 1]);
%! assert (D(1, :), [15, 20, 0]);
%! assert (D(2, :), [0, 5, 15]);
