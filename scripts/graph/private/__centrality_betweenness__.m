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
## @deftypefn  {} {@var{c} =} __centrality_betweenness__ (@var{G})
## @deftypefnx {} {@var{c} =} __centrality_betweenness__ (@var{G}, @qcode{"Cost"}, @var{W})
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
## those paths that pass through @math{v}.  Without the
## @qcode{"Cost"} option the edges are treated as unweighted (BFS
## Brandes); self-loops contribute no shortest paths.
##
## On an undirected @code{graph} the sum above counts each unordered
## pair @math{@{s, t@}} once rather than twice, so the all-sources
## iteration below divides the raw total by two at the end.  On a
## @code{digraph} the ordered-pair semantics are preserved and no
## division occurs.
##
## The unweighted all-sources formulation runs a BFS per source, so
## the overall complexity is @math{O (N (N + E))} in time and
## @math{O (N + E)} in working memory per source.  For @math{N = 0}
## the result is @code{zeros (0, 1)}; for @math{N = 1} it is
## @code{zeros (1, 1)}.
##
## The optional @qcode{"Cost"} Name-Value pair supplies a vector of
## positive per-edge costs of length @code{numedges (@var{G})} that
## overrides any stored edge weights.  When supplied, Dijkstra's
## algorithm replaces BFS for single-source shortest paths so the
## dependency accumulation runs over weighted shortest paths; the
## per-source cost is @math{O (N^2)} rather than @math{O (N + E)}.
##
## Reference: U. Brandes, "A Faster Algorithm for Betweenness
## Centrality", Journal of Mathematical Sociology 25(2):163-177, 2001.
##
## This helper is internal to the graph/digraph classes and is not
## intended to be called directly by user code.
## @seealso{centrality, distances}
## @end deftypefn

function c = __centrality_betweenness__ (G, varargin)

  if (nargin < 1)
    print_usage ();
  endif

  if (! (isa (G, "graph") || isa (G, "digraph")))
    error ("Octave:invalid-input-arg", ...
           "__centrality_betweenness__: G must be a graph or digraph");
  endif

  [have_cost, cost] = __parse_cost_option__ (G, "betweenness", varargin);

  N = numnodes (G);
  c = zeros (N, 1);
  if (N <= 1)
    return;
  endif

  directed = isa (G, "digraph");

  if (have_cost)
    ## Dijkstra Brandes over cost-weighted adjacency.  adjacency (G,
    ## cost) is symmetric for a graph and directed for a digraph.
    W = adjacency (G, cost);
    c = __brandes_weighted__ (W, N);
    if (! directed)
      c = c / 2;
    endif
    return;
  endif

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

## Parse the "Cost" Name-Value option.  See __centrality_closeness__
## for the canonical copy; this is duplicated here so the helper
## remains self-contained (private helpers are not on the search path
## of one another and are meant to avoid cross-helper dependencies).
function [have_cost, cost] = __parse_cost_option__ (G, name, args)
  have_cost = false;
  cost = [];
  if (isempty (args))
    return;
  endif
  if (mod (numel (args), 2) != 0)
    error ("Octave:invalid-input-arg", ...
           ["centrality: %s Name-Value arguments must come in pairs ", ...
            "(missing value for option '%s')"], name, args{end});
  endif
  for k = 1:2:numel (args)
    opt = args{k};
    val = args{k+1};
    if (! ischar (opt) || ! isrow (opt))
      error ("Octave:invalid-input-arg", ...
             ["centrality: %s option name must be a character ", ...
              "row vector (string)"], name);
    endif
    switch (lower (opt))
      case "cost"
        M = numedges (G);
        if (! isnumeric (val) || ! isreal (val))
          error ("Octave:invalid-input-arg", ...
                 "centrality: 'Cost' must be a numeric real vector");
        endif
        if (! isempty (val) && ! isvector (val))
          error ("Octave:invalid-input-arg", ...
                 "centrality: 'Cost' must be a vector");
        endif
        if (numel (val) != M)
          error ("Octave:invalid-input-arg", ...
                 ["centrality: 'Cost' must have length %d ", ...
                  "(numedges (G))"], M);
        endif
        if (any (! isfinite (val)))
          error ("Octave:invalid-input-arg", ...
                 "centrality: 'Cost' entries must be finite");
        endif
        if (any (val <= 0))
          error ("Octave:invalid-input-arg", ...
                 "centrality: 'Cost' entries must be positive");
        endif
        cost = double (val(:));
        have_cost = true;
      otherwise
        error ("Octave:invalid-input-arg", ...
               "centrality: unknown %s option '%s'", name, opt);
    endswitch
  endfor
endfunction

## Weighted Brandes' algorithm over the cost-weighted adjacency
## matrix @var{W}.  Returns an unscaled betweenness vector of length
## @var{N}; the caller divides by 2 for an undirected graph.  The
## Dijkstra inner loop uses a plain O(N) extract-min scan (no
## priority-queue data structure in core Octave); that keeps the
## per-source complexity at @math{O (N^2)} which is adequate for the
## small graphs in the BIST suite and for the MATLAB-parity test
## topologies a typical user will throw at us.  The stack of settled
## vertices is built in non-decreasing distance order so the Phase 2
## dependency accumulation walks it back-to-front exactly as in the
## classical Brandes presentation.
function c = __brandes_weighted__ (W, N)
  c = zeros (N, 1);
  if (N <= 1)
    return;
  endif
  ## Precompute sparse out-neighbour lists with their edge weights.
  ## find on the transpose gives (dst, src, w) in column-major order
  ## which matches the iteration pattern below when we use find(W)
  ## directly: (src, dst, w).
  [sr, dc, ww] = find (W);
  succ = cell (N, 1);
  succ_w = cell (N, 1);
  if (! isempty (sr))
    for u = 1:N
      mask = (sr == u);
      nb = dc(mask).';
      wn = ww(mask).';
      ## Drop self-loops: zero-length hops on the shortest-path tree
      ## contribute no paths between distinct vertices.
      keep = (nb != u);
      succ{u} = nb(keep);
      succ_w{u} = wn(keep);
    endfor
  else
    for u = 1:N
      succ{u} = zeros (1, 0);
      succ_w{u} = zeros (1, 0);
    endfor
  endif

  for s = 1:N
    dist = inf (N, 1);  dist(s) = 0;
    sigma = zeros (N, 1);  sigma(s) = 1;
    P = cell (N, 1);
    settled = false (N, 1);
    S = zeros (N, 1);
    top = 0;

    ## Dijkstra with O(N) extract-min.
    while (true)
      ## Find the unsettled vertex with the smallest finite distance.
      cand_u = 0;
      cand_d = Inf;
      for v = 1:N
        if (! settled(v) && dist(v) < cand_d)
          cand_d = dist(v);
          cand_u = v;
        endif
      endfor
      if (cand_u == 0)
        break;           # everything reachable has been settled
      endif
      v = cand_u;
      settled(v) = true;
      top = top + 1;
      S(top) = v;
      nb = succ{v};
      wn = succ_w{v};
      dv = dist(v);
      for k = 1:numel (nb)
        w = nb(k);
        alt = dv + wn(k);
        if (alt < dist(w))
          dist(w) = alt;
          sigma(w) = sigma(v);
          P{w} = v;
        elseif (alt == dist(w))
          sigma(w) = sigma(w) + sigma(v);
          P{w} = [P{w}, v];
        endif
      endfor
    endwhile

    ## Phase 2: reverse accumulation.
    delta = zeros (N, 1);
    for k = top:-1:1
      w = S(k);
      preds = P{w};
      if (! isempty (preds))
        factor = (1 + delta(w)) / sigma(w);
        for j = 1:numel (preds)
          v = preds(j);
          delta(v) = delta(v) + sigma(v) * factor;
        endfor
      endif
      if (w != s)
        c(w) = c(w) + delta(w);
      endif
    endfor
  endfor
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

## -------------------- Cost Name-Value option --------------------

## Cost = ones reproduces unweighted betweenness on a simple graph.
%!test
%! G = graph ([1 2], [2 3]);
%! c0 = __centrality_betweenness__ (G);
%! c1 = __centrality_betweenness__ (G, "Cost", [1; 1]);
%! assert (c1, c0, 1e-12);

## Cost = ones reproduces unweighted betweenness on a digraph.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! c0 = __centrality_betweenness__ (G);
%! c1 = __centrality_betweenness__ (G, "Cost", [1; 1; 1]);
%! assert (c1, c0, 1e-12);

## Cost uniformly-scales do not affect the result (ratios are the same).
%!test
%! G = graph ([1 1 2 3], [2 3 3 4]);
%! c0 = __centrality_betweenness__ (G);
%! c5 = __centrality_betweenness__ (G, "Cost", 5 * ones (4, 1));
%! assert (c5, c0, 1e-12);

## Weighted triangle + external path: Cost makes the short edge
## redirect the shortest path.  Edges (in lex order):
##   1-2, 1-3, 2-3, 3-4
## Without Cost: 1 and 3 are intermediaries on the 2-4 and 1-4
## paths respectively; centre depends on which shortest path is
## chosen by the BFS tie-breaking.  With Cost [10, 1, 1, 1], the
## 1->2 edge is very expensive so path 1-to-2 goes via 3 (length 2
## < 10).  Path 2-to-4 becomes 2->3->4 (length 2).  Path 1-to-4 is
## 1->3->4 (length 2).  So node 3 is on 3 of the 3 pairs with an
## intermediary: (1,2), (1,4), (2,4).  Undirected -> 3/2 ? No:
## betweenness is c(3) = sum_{s!=3, t!=3, s<t} sigma_{st}(3)/sigma_{st}
## With unique shortest paths here, c(3) = 3.
%!test
%! G = graph ([1 1 2 3], [2 3 3 4]);
%! c = __centrality_betweenness__ (G, "Cost", [10; 1; 1; 1]);
%! assert (c(3), 3, 1e-9);
%! assert (c(1), 0, 1e-9);
%! assert (c(2), 0, 1e-9);
%! assert (c(4), 0, 1e-9);

## Directed diamond 1->{2,3}->4 with equal Cost: two paths of length 2
## evenly split (matches unweighted default).
%!test
%! G = digraph ([1 1 2 3], [2 3 4 4]);
%! c = __centrality_betweenness__ (G, "Cost", [1; 1; 1; 1]);
%! assert (c, [0; 0.5; 0.5; 0], 1e-9);

## Directed diamond with unequal Cost: the cheaper middle node
## becomes the unique shortest-path intermediary.
## Edges (lex order): 1->2, 1->3, 2->4, 3->4.
## Cost [1; 10; 1; 10]: path 1->4 goes 1->2->4 (length 2) strictly
## less than 1->3->4 (length 20).  So node 2 is the only betweenness
## holder: c = [0; 1; 0; 0].
%!test
%! G = digraph ([1 1 2 3], [2 3 4 4]);
%! c = __centrality_betweenness__ (G, "Cost", [1; 10; 1; 10]);
%! assert (c, [0; 1; 0; 0], 1e-9);

## Self-loop edges contribute no shortest path even with Cost.
%!test
%! G = graph ([1 2 3], [1 2 3]);           # three self-loops
%! c = __centrality_betweenness__ (G, "Cost", [1; 1; 1]);
%! assert (c, zeros (3, 1), 1e-12);

## Disconnected graph: betweenness still zero.
%!test
%! G = graph ([1 3], [2 4]);
%! c = __centrality_betweenness__ (G, "Cost", [1; 1]);
%! assert (c, zeros (4, 1), 1e-12);

## Empty graph with empty Cost -> zeros(0, 1).
%!test
%! G = graph ();
%! c = __centrality_betweenness__ (G, "Cost", zeros (0, 1));
%! assert (c, zeros (0, 1));

## Column double output with Cost.
%!test
%! G = graph ([1 2], [2 3]);
%! c = __centrality_betweenness__ (G, "Cost", [1; 1]);
%! assert (size (c), [3, 1]);
%! assert (class (c), "double");

## Cost option name is case-insensitive.
%!test
%! G = graph ([1 2], [2 3]);
%! c1 = __centrality_betweenness__ (G, "Cost", [1; 1]);
%! c2 = __centrality_betweenness__ (G, "COST", [1; 1]);
%! c3 = __centrality_betweenness__ (G, "cost", [1; 1]);
%! assert (c2, c1, 1e-12);
%! assert (c3, c1, 1e-12);

## Cost wrong length errors.
%!error <Cost.*length>
%! __centrality_betweenness__ (graph ([1 2], [2 3]), "Cost", [1; 1; 1]);

## Cost with zero entry errors.
%!error <Cost.*positive>
%! __centrality_betweenness__ (graph ([1 2], [2 3]), "Cost", [1; 0]);

## Cost with negative entry errors.
%!error <Cost.*positive>
%! __centrality_betweenness__ (graph ([1 2], [2 3]), "Cost", [1; -2]);

## Cost with NaN errors.
%!error <Cost.*finite>
%! __centrality_betweenness__ (graph ([1 2], [2 3]), "Cost", [1; NaN]);

## Cost non-numeric errors.
%!error <Cost.*numeric>
%! __centrality_betweenness__ (graph ([1 2], [2 3]), "Cost", "hi");

## Unknown option errors.
%!error <unknown betweenness option>
%! __centrality_betweenness__ (graph ([1 2], [2 3]), "Importance", [1; 1]);

## Odd number of Name-Value args errors.
%!error <pair>
%! __centrality_betweenness__ (graph ([1 2], [2 3]), "Cost");
