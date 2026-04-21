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
## @deftypefn {} {@var{c} =} __centrality_betweenness__ (@var{G})
## Private helper: compute unnormalised betweenness centrality on the
## @code{graph} or @code{digraph} @var{G} using Brandes' algorithm.
##
## For a node @math{v} the returned betweenness centrality is
##
## @example
## @group
## c(v) = sum_{s != v != t} sigma_{s, t}(v) / sigma_{s, t}
## @end group
## @end example
##
## where @math{sigma_{s, t}} is the number of shortest paths from
## @math{s} to @math{t} and @math{sigma_{s, t}(v)} is the number of
## those paths that pass through @math{v}.  Edges are treated as
## unweighted (weights are ignored for the default MATLAB call; the
## @code{"Cost"} name-value option is a future story) and self-loops
## contribute no shortest paths.
##
## On an undirected @code{graph} the sum above counts each unordered
## pair @math{@{s, t@}} once rather than twice, so the all-sources
## iteration below divides the raw total by two at the end.  On a
## @code{digraph} the ordered-pair semantics are preserved and no
## division occurs.
##
## The all-sources formulation runs a BFS per source, so the overall
## complexity is @math{O (N (N + E))} in time and @math{O (N + E)} in
## working memory per source.  For @math{N = 0} the result is
## @code{zeros (0, 1)}; for @math{N = 1} it is @code{zeros (1, 1)}.
##
## Reference: U. Brandes, "A Faster Algorithm for Betweenness
## Centrality", Journal of Mathematical Sociology 25(2):163-177, 2001.
##
## This helper is internal to the graph/digraph classes and is not
## intended to be called directly by user code.
## @seealso{centrality, distances}
## @end deftypefn

function c = __centrality_betweenness__ (G)

  if (nargin != 1)
    print_usage ();
  endif

  if (! (isa (G, "graph") || isa (G, "digraph")))
    error ("Octave:invalid-input-arg", ...
           "__centrality_betweenness__: G must be a graph or digraph");
  endif

  N = numnodes (G);
  c = zeros (N, 1);
  if (N <= 1)
    return;
  endif

  directed = isa (G, "digraph");

  ## Binary successor pattern.  For a graph this is the symmetric
  ## adjacency; for a digraph it is the out-neighbour pattern.  spones
  ## collapses parallel edges in a multigraph (MATLAB-equivalent
  ## behaviour for the default, unweighted betweenness call).
  A = spones (adjacency (G));

  ## Precompute successor index lists once.
  succ = cell (N, 1);
  for u = 1:N
    nb = find (A(u, :));
    ## Drop self-loops: (u, u) contributes no distinct shortest path.
    nb = nb(nb != u);
    succ{u} = nb(:).';
  endfor

  for s = 1:N
    ## --------------------------------------------------------------
    ## Phase 1: single-source BFS.  Track sigma(v) = number of
    ## shortest paths from s to v, and P{v} = predecessors of v on
    ## any shortest path from s.  Push visited vertices onto stack
    ## S in the order they were popped from the queue (i.e. in
    ## non-decreasing distance from s), so Phase 2 can walk them in
    ## reverse and accumulate dependency.
    ## --------------------------------------------------------------
    sigma = zeros (N, 1);
    sigma(s) = 1;
    dist = -ones (N, 1);
    dist(s) = 0;
    P = cell (N, 1);

    queue = zeros (N, 1);
    queue(1) = s;
    qhead = 1;
    qtail = 1;

    S = zeros (N, 1);
    stop_idx = 0;

    while (qhead <= qtail)
      v = queue(qhead);
      qhead = qhead + 1;
      stop_idx = stop_idx + 1;
      S(stop_idx) = v;
      nb = succ{v};
      dv = dist(v);
      for k = 1:numel (nb)
        w = nb(k);
        if (dist(w) < 0)
          dist(w) = dv + 1;
          qtail = qtail + 1;
          queue(qtail) = w;
        endif
        if (dist(w) == dv + 1)
          sigma(w) = sigma(w) + sigma(v);
          P{w} = [P{w}, v];
        endif
      endfor
    endwhile

    ## --------------------------------------------------------------
    ## Phase 2: reverse accumulation.  Walk S back-to-front; for
    ## each w, every predecessor v receives a dependency increment
    ## delta(v) += sigma(v) / sigma(w) * (1 + delta(w)), and when w
    ## is not the source s itself its own delta is added to the
    ## cumulative betweenness c(w).
    ## --------------------------------------------------------------
    delta = zeros (N, 1);
    for k = stop_idx:-1:1
      w = S(k);
      preds = P{w};
      inv_sigma_w = 1 / sigma(w);
      factor = (1 + delta(w)) * inv_sigma_w;
      for j = 1:numel (preds)
        v = preds(j);
        delta(v) = delta(v) + sigma(v) * factor;
      endfor
      if (w != s)
        c(w) = c(w) + delta(w);
      endif
    endfor
  endfor

  if (! directed)
    ## Undirected: each unordered pair {s, t} was visited twice in
    ## the all-sources loop (once as (s, t) and once as (t, s)).
    c = c / 2;
  endif

endfunction


## ------------------------------------------------------------------
## BIST blocks
## ------------------------------------------------------------------

## Path graph 1-2-3: centre node betweenness 1.
%!test
%! G = graph ([1 2], [2 3]);
%! assert (__centrality_betweenness__ (G), [0; 1; 0], 1e-12);

## Path digraph 1->2->3: same value (both formulations give 1).
%!test
%! G = digraph ([1 2], [2 3]);
%! assert (__centrality_betweenness__ (G), [0; 1; 0], 1e-12);

## Triangle: no intermediary on any pair.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! assert (__centrality_betweenness__ (G), zeros (3, 1), 1e-12);

## Star K_{1,5}: centre on all C(5, 2) = 10 leaf-leaf paths.
%!test
%! G = graph (ones (1, 5), 2:6);
%! assert (__centrality_betweenness__ (G), [10; 0; 0; 0; 0; 0], 1e-12);

## 4-cycle undirected: every node has betweenness 0.5.
%!test
%! G = graph ([1 2 3 4], [2 3 4 1]);
%! assert (__centrality_betweenness__ (G), ...
%!         [0.5; 0.5; 0.5; 0.5], 1e-12);

## Directed 3-cycle 1->2->3->1: every node has betweenness 1.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! assert (__centrality_betweenness__ (G), [1; 1; 1], 1e-12);

## Directed diamond 1->{2,3}->4: two middle nodes share (1, 4) evenly.
%!test
%! G = digraph ([1 1 2 3], [2 3 4 4]);
%! assert (__centrality_betweenness__ (G), ...
%!         [0; 0.5; 0.5; 0], 1e-12);

## Undirected diamond: every node on one non-adjacent pair.
%!test
%! G = graph ([1 1 2 3], [2 3 4 4]);
%! assert (__centrality_betweenness__ (G), ...
%!         [0.5; 0.5; 0.5; 0.5], 1e-12);

## Disconnected graph: betweenness is zero everywhere.
%!test
%! G = graph ([1 3], [2 4]);
%! assert (__centrality_betweenness__ (G), zeros (4, 1), 1e-12);

## Empty graph / digraph: zeros(0, 1).
%!test
%! assert (__centrality_betweenness__ (graph ()),   zeros (0, 1));
%! assert (__centrality_betweenness__ (digraph ()), zeros (0, 1));

## Single-node graph / digraph: zeros(1, 1).
%!test
%! assert (__centrality_betweenness__ (graph (1)),   0);
%! assert (__centrality_betweenness__ (digraph (1)), 0);

## Edgeless multi-node: zeros.
%!test
%! assert (__centrality_betweenness__ (graph (4)),   zeros (4, 1));
%! assert (__centrality_betweenness__ (digraph (4)), zeros (4, 1));

## Self-loops are ignored.
%!test
%! G1 = graph ([1 2 1], [1 2 3]);
%! G2 = graph (1, 3, [], 3);
%! assert (__centrality_betweenness__ (G1), ...
%!         __centrality_betweenness__ (G2), 1e-12);

## Weights ignored: identical topology gives identical centrality
## regardless of stored weights.
%!test
%! G1 = graph ([1 2], [2 3]);
%! G2 = graph ([1 2], [2 3], [0.25 100]);
%! assert (__centrality_betweenness__ (G1), ...
%!         __centrality_betweenness__ (G2), 1e-12);

## Column double output.
%!test
%! G = graph ([1 2], [2 3]);
%! c = __centrality_betweenness__ (G);
%! assert (size (c), [3, 1]);
%! assert (class (c), "double");

## Invalid first argument errors.
%!error <must be a graph or digraph>
%! __centrality_betweenness__ (42);

%!error <must be a graph or digraph>
%! __centrality_betweenness__ ("foo");

## Missing input is an error.
%!error __centrality_betweenness__ ()
