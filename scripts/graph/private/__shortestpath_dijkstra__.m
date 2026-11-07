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
## @deftypefn {} {[@var{path}, @var{d}] =} __shortestpath_dijkstra__ (@var{W}, @var{s}, @var{t})
## Private helper: compute a single shortest path from node @var{s} to
## node @var{t} using Dijkstra's algorithm with predecessor tracking.
##
## @var{W} is the square sparse or dense weight matrix: @var{W}(i, j)
## is the weight of the edge from node @math{i} to node @math{j}; a
## zero entry means no edge.  For an undirected graph the caller
## supplies a symmetric @var{W}; for a directed graph @var{W} is
## typically asymmetric.  All stored weights must be non-negative.
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
## Self-loops in @var{W} are ignored.
##
## The second return value @var{d} is the scalar sum of edge weights
## along @var{path}, or @code{Inf} when unreachable.
##
## Raises @code{Octave:invalid-input-arg} when @var{W} contains a
## negative entry.
##
## This helper is internal to the graph/digraph classes and is not
## intended to be called directly by user code.
## @seealso{shortestpath, graph, digraph, __distances_dijkstra__}
## @end deftypefn

function [path, d] = __shortestpath_dijkstra__ (W, s, t)

  if (nargin != 3)
    print_usage ();
  endif

  N = size (W, 1);

  ## Validate non-negative weights.  Use nonzeros() so zero entries in
  ## sparse storage (i.e. non-edges) are skipped automatically.
  if (any (nonzeros (W) < 0))
    error ("Octave:invalid-input-arg", ...
           "shortestpath: negative edge weights are not supported by the default Dijkstra method");
  endif

  s = double (s);
  t = double (t);

  ## Trivial case: source == target.
  if (s == t)
    path = s;
    d = 0;
    return;
  endif

  ## Precompute neighbor lists for speed.  For each node u collect its
  ## outgoing non-zero neighbors and their weights once.
  neighbors = cell (N, 1);
  weights = cell (N, 1);
  for u = 1:N
    [~, nb, w] = find (W(u, :));
    neighbors{u} = nb(:);
    weights{u} = w(:);
  endfor

  dist = inf (N, 1);
  pred = zeros (N, 1);   # 0 means "no predecessor / source / unreached"
  visited = false (N, 1);
  dist(s) = 0;

  ## Classic Dijkstra with O(N) extract-min per iteration, with early
  ## termination once the target is finalised.  For dense BIST-sized
  ## inputs this is fine; revisit if performance warrants a proper
  ## priority-queue implementation.
  for iter = 1:N
    cand = dist;
    cand(visited) = Inf;
    [min_d, u] = min (cand);
    if (! isfinite (min_d))
      break;
    endif
    if (u == t)
      ## Standard Dijkstra optimality: when t is extracted from the
      ## frontier its distance is final.  Stop relaxing further.
      visited(u) = true;
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
          pred(v) = u;
        endif
      endif
    endfor
  endfor

  d = dist(t);
  if (! isfinite (d))
    ## Unreachable: return a 0-by-1 empty column.
    path = zeros (0, 1);
    return;
  endif

  ## Reconstruct path by walking predecessors backwards from t to s.
  ## Preallocate an upper-bound-sized buffer and trim at the end to
  ## avoid growing on each iteration.
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
%! [P, d] = __shortestpath_dijkstra__ (W, 2, 2);
%! assert (P, 2);
%! assert (d, 0);

## Single edge 1->2, unweighted: path is [1; 2].
%!test
%! W = sparse (1, 2, 1, 2, 2);
%! [P, d] = __shortestpath_dijkstra__ (W, 1, 2);
%! assert (P, [1; 2]);
%! assert (d, 1);

## Chain 1->2->3: path 1 to 3 is [1; 2; 3], d=2.
%!test
%! W = sparse ([1 2], [2 3], [1 1], 3, 3);
%! [P, d] = __shortestpath_dijkstra__ (W, 1, 3);
%! assert (P, [1; 2; 3]);
%! assert (d, 2);

## Weighted 3-cycle: 1->2 direct, d=5.
%!test
%! W = sparse ([1 2 3], [2 3 1], [5 10 15], 3, 3);
%! [P, d] = __shortestpath_dijkstra__ (W, 1, 2);
%! assert (P, [1; 2]);
%! assert (d, 5);

## Weighted 3-cycle: 3->2 requires 3->1->2, d=20.
%!test
%! W = sparse ([1 2 3], [2 3 1], [5 10 15], 3, 3);
%! [P, d] = __shortestpath_dijkstra__ (W, 3, 2);
%! assert (P, [3; 1; 2]);
%! assert (d, 20);

## Prefers cheaper two-hop path over expensive direct edge.
%!test
%! W = sparse ([1 1 2], [2 3 3], [5 100 1], 3, 3);
%! [P, d] = __shortestpath_dijkstra__ (W, 1, 3);
%! assert (P, [1; 2; 3]);
%! assert (d, 6);

## Unreachable: returns empty 0x1 path, d=Inf.
%!test
%! W = sparse (3, 3);
%! [P, d] = __shortestpath_dijkstra__ (W, 1, 2);
%! assert (size (P), [0, 1]);
%! assert (d, Inf);

## Disjoint components: path across components is unreachable.
%!test
%! W = sparse ([1 3], [2 4], [1 1], 4, 4);
%! [P, d] = __shortestpath_dijkstra__ (W, 1, 3);
%! assert (size (P), [0, 1]);
%! assert (d, Inf);

## Direction respected: reverse of 1->2 is unreachable.
%!test
%! W = sparse (1, 2, 1, 2, 2);
%! [P, d] = __shortestpath_dijkstra__ (W, 2, 1);
%! assert (size (P), [0, 1]);
%! assert (d, Inf);

## Self-loop on source node is ignored (does not appear in path).
%!test
%! W = sparse ([1 1], [1 2], [99 1], 2, 2);
%! [P, d] = __shortestpath_dijkstra__ (W, 1, 2);
%! assert (P, [1; 2]);
%! assert (d, 1);

## Negative weight errors.
%!error <negative edge weights>
%! W = sparse ([1 2], [2 3], [1 -1], 3, 3);
%! __shortestpath_dijkstra__ (W, 1, 3);

## Dense input accepted (not just sparse).
%!test
%! W = [0 1 0; 0 0 1; 0 0 0];
%! [P, d] = __shortestpath_dijkstra__ (W, 1, 3);
%! assert (P, [1; 2; 3]);
%! assert (d, 2);

## No-arg call errors via print_usage.
%!error __shortestpath_dijkstra__ ()

## Wrong argcount errors via print_usage.
%!error __shortestpath_dijkstra__ (sparse (1, 1), 1)

## Symmetric input (undirected-style): 1-2-3 chain, 1 to 3 is [1;2;3].
%!test
%! W = sparse ([1 2 2 3], [2 1 3 2], [1 1 1 1], 3, 3);
%! [P, d] = __shortestpath_dijkstra__ (W, 1, 3);
%! assert (P, [1; 2; 3]);
%! assert (d, 2);
