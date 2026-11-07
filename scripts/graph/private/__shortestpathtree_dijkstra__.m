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
## @deftypefn {} {[@var{pred}, @var{dist}] =} __shortestpathtree_dijkstra__ (@var{W}, @var{s})
## Private helper: compute the single-source shortest path tree from
## node @var{s} using Dijkstra's algorithm and return the predecessor
## and distance vectors.
##
## @var{W} is the square sparse or dense weight matrix: @var{W}(i, j)
## is the weight of the edge from node @math{i} to node @math{j}; a
## zero entry means no edge.  For an undirected graph the caller
## supplies a symmetric @var{W}; for a directed graph @var{W} is
## typically asymmetric.  All stored weights must be non-negative.
##
## @var{s} is a scalar positive integer node index in
## @code{1:size (W, 1)}.
##
## The first return value @var{pred} is an @code{N}-by-@code{1} column
## vector of predecessor indices: @code{pred(i)} is the index of the
## predecessor of node @math{i} in the shortest path tree rooted at
## @var{s}, or @code{0} if node @math{i} is not reachable from
## @var{s}.  @code{pred(@var{s})} is @code{0} (the source has no
## predecessor); the caller is expected to fix this to @var{s} when
## producing MATLAB-style predecessor vectors.  Self-loops in @var{W}
## are ignored.
##
## The second return value @var{dist} is an @code{N}-by-@code{1} column
## vector of distances: @code{dist(i)} is the length of the shortest
## path from @var{s} to node @math{i} under @var{W}, @code{Inf} when
## @math{i} is not reachable, and @code{0} for @math{i == @var{s}}.
##
## Raises @code{Octave:invalid-input-arg} when @var{W} contains a
## negative entry.
##
## This helper is internal to the graph/digraph classes and is not
## intended to be called directly by user code.
## @seealso{shortestpathtree, graph, digraph, __shortestpath_dijkstra__}
## @end deftypefn

function [pred, dist] = __shortestpathtree_dijkstra__ (W, s)

  if (nargin != 2)
    print_usage ();
  endif

  N = size (W, 1);

  ## Validate non-negative weights.  Use nonzeros() so zero entries in
  ## sparse storage (i.e. non-edges) are skipped automatically.
  if (any (nonzeros (W) < 0))
    error ("Octave:invalid-input-arg", ...
           "shortestpathtree: negative edge weights are not supported by the default Dijkstra method");
  endif

  s = double (s);

  dist = inf (N, 1);
  pred = zeros (N, 1);
  visited = false (N, 1);

  if (N == 0)
    return;
  endif

  dist(s) = 0;

  ## Precompute neighbor lists for speed.  For each node u collect its
  ## outgoing non-zero neighbors and their weights once.
  neighbors = cell (N, 1);
  weights = cell (N, 1);
  for u = 1:N
    [~, nb, w] = find (W(u, :));
    neighbors{u} = nb(:);
    weights{u} = w(:);
  endfor

  ## Classic Dijkstra with O(N) extract-min per iteration.  For dense
  ## BIST-sized inputs this is fine; revisit if performance warrants
  ## a proper priority-queue implementation.
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
          pred(v) = u;
        endif
      endif
    endfor
  endfor

endfunction


## ------------------------------------------------------------------
## BIST blocks
## ------------------------------------------------------------------

## Empty graph: N=0 returns zero-length pred and dist.
%!test
%! W = sparse (0, 0);
%! [p, d] = __shortestpathtree_dijkstra__ (W, 1);
%! assert (size (p), [0, 1]);
%! assert (size (d), [0, 1]);

## Single node: pred is 0, dist is 0.
%!test
%! W = sparse (1, 1);
%! [p, d] = __shortestpathtree_dijkstra__ (W, 1);
%! assert (p, 0);
%! assert (d, 0);

## Chain 1->2->3: from 1, pred = [0; 1; 2], dist = [0; 1; 2].
%!test
%! W = sparse ([1 2], [2 3], [1 1], 3, 3);
%! [p, d] = __shortestpathtree_dijkstra__ (W, 1);
%! assert (p, [0; 1; 2]);
%! assert (d, [0; 1; 2]);

## Unreachable: from 1 on an edgeless 3-node W, only dist(1)==0.
%!test
%! W = sparse (3, 3);
%! [p, d] = __shortestpathtree_dijkstra__ (W, 1);
%! assert (p, [0; 0; 0]);
%! assert (d, [0; Inf; Inf]);

## Disjoint components: from 1, only component {1,2} reachable.
%!test
%! W = sparse ([1 3], [2 4], [1 1], 4, 4);
%! [p, d] = __shortestpathtree_dijkstra__ (W, 1);
%! assert (p, [0; 1; 0; 0]);
%! assert (d, [0; 1; Inf; Inf]);

## Weighted: short indirect path beats direct edge.
%!test
%! W = sparse ([1 1 2], [2 3 3], [5 100 1], 3, 3);
%! [p, d] = __shortestpathtree_dijkstra__ (W, 1);
%! assert (p, [0; 1; 2]);
%! assert (d, [0; 5; 6]);

## Source in the middle of a chain (reverse unreachable on digraph).
%!test
%! W = sparse ([1 2], [2 3], [1 1], 3, 3);
%! [p, d] = __shortestpathtree_dijkstra__ (W, 2);
%! assert (p, [0; 0; 2]);
%! assert (d, [Inf; 0; 1]);

## Symmetric (undirected-style): from 2, both 1 and 3 are reachable.
%!test
%! W = sparse ([1 2 2 3], [2 1 3 2], [1 1 1 1], 3, 3);
%! [p, d] = __shortestpathtree_dijkstra__ (W, 2);
%! assert (p, [2; 0; 2]);
%! assert (d, [1; 0; 1]);

## Self-loop on source is ignored.
%!test
%! W = sparse ([1 1], [1 2], [99 1], 2, 2);
%! [p, d] = __shortestpathtree_dijkstra__ (W, 1);
%! assert (p, [0; 1]);
%! assert (d, [0; 1]);

## Dense input accepted.
%!test
%! W = [0 1 0; 0 0 1; 0 0 0];
%! [p, d] = __shortestpathtree_dijkstra__ (W, 1);
%! assert (p, [0; 1; 2]);
%! assert (d, [0; 1; 2]);

## Negative weights error.
%!error <negative edge weights>
%! W = sparse ([1 2], [2 3], [1 -1], 3, 3);
%! __shortestpathtree_dijkstra__ (W, 1);

## No-args call errors.
%!error __shortestpathtree_dijkstra__ ()

## Wrong argcount errors.
%!error __shortestpathtree_dijkstra__ (sparse (1, 1))

## 4-node DAG: 1->2(1), 1->3(10), 2->3(1), 3->4(1).
## Tree from 1: 1->2, 2->3, 3->4.  pred = [0; 1; 2; 3], dist = [0; 1; 2; 3].
%!test
%! W = sparse ([1 1 2 3], [2 3 3 4], [1 10 1 1], 4, 4);
%! [p, d] = __shortestpathtree_dijkstra__ (W, 1);
%! assert (p, [0; 1; 2; 3]);
%! assert (d, [0; 1; 2; 3]);
