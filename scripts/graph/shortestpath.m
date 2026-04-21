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
## @deftypefn  {} {@var{P} =} shortestpath (@var{G}, @var{s}, @var{t})
## @deftypefnx {} {[@var{P}, @var{d}] =} shortestpath (@var{G}, @var{s}, @var{t})
## @deftypefnx {} {[@var{P}, @var{d}, @var{edgepath}] =} shortestpath (@var{G}, @var{s}, @var{t})
## @deftypefnx {} {[@dots{}] =} shortestpath (@dots{}, @qcode{"Method"}, @var{method})
## Return a single shortest path between nodes @var{s} and @var{t} of
## the graph or digraph @var{G}.
##
## @var{G} must be a @code{graph} or @code{digraph} object.  @var{s} and
## @var{t} are scalar node identifiers: a positive integer node index,
## a character row vector naming a node, or a 1-element cell array of
## strings naming a node.  When @var{G} has node names a mix of numeric
## and name identifiers is allowed.
##
## @var{P} is a row vector giving the sequence of node identifiers
## traversed from @var{s} to @var{t} under the stored edge weights
## (every edge has weight @code{1} when @var{G} is unweighted).  When
## @var{s} and @var{t} are both numeric @var{P} is a numeric row
## vector; when either @var{s} or @var{t} is a name @var{P} is a
## @code{1}-by-@var{k} cell array of strings.  When @code{@var{s} ==
## @var{t}} the path is the single-node vector @code{[@var{s}]}.
##
## @var{d} is the total distance along @var{P} (the sum of the weights
## of the edges in the path), or @code{Inf} when no path from @var{s}
## to @var{t} exists.
##
## @var{edgepath} is a @code{1}-by-@code{numel (@var{P}) - 1} row vector
## of edge indices into @code{@var{G}.Edges} identifying the edges
## traversed by @var{P}.  When @var{P} has zero or one elements (i.e.,
## @var{s} is unreachable from @var{t}, or @code{@var{s} == @var{t}})
## @var{edgepath} is a @code{1}-by-@code{0} empty row vector.
##
## When @var{t} is not reachable from @var{s} the outputs are empty:
## @var{P} is a @code{1}-by-@code{0} empty vector (numeric or cellstr
## following the input type), @var{d} is @code{Inf}, and
## @var{edgepath} is a @code{1}-by-@code{0} empty row vector.
##
## For the undirected @code{graph} class, edges may be traversed in
## either direction.  For the directed @code{digraph} class, the path
## must follow edge direction, so @code{shortestpath (@var{G},
## @var{s}, @var{t})} is in general not the same as
## @code{shortestpath (@var{G}, @var{t}, @var{s})}.  For a @code{digraph}
## with parallel edges (multigraph), the cheapest of the parallel edges
## connecting each pair of endpoints is used; @var{edgepath} returns
## the index of that cheapest edge.
##
## The optional @qcode{"Method"} Name-Value pair selects the algorithm
## used for the computation.  Supported values (case-insensitive) are:
##
## @table @asis
## @item @qcode{"auto"} (default)
## Pick automatically: Dijkstra when all edge weights are
## non-negative, and Bellman-Ford when any weight is negative.
## @item @qcode{"positive"}
## Run Dijkstra's algorithm.  Every edge weight must be non-negative;
## a negative weight raises an error.
## @item @qcode{"mixed"}
## Run Bellman-Ford.  Negative edge weights are allowed provided no
## negative cycle is reachable from @var{s}; a negative cycle raises
## an error.  For an undirected graph, any negative weight is a
## negative cycle (@math{u-v-u}) and is always rejected.
## @end table
##
## Self-loops do not influence the path: a self-loop on node
## @math{i} never appears in @var{P} and never contributes to @var{d}.
##
## @example
## @group
## G = digraph ([1 1 2], [2 3 3], [5 100 1]);
## [P, d, ep] = shortestpath (G, 1, 3)
##          @result{}  P  = [1 2 3]
##              d  = 6
##              ep = [1 3]
##
## H = graph ([1 2 3], [2 3 1]);
## [P, d] = shortestpath (H, 1, 3)
##          @result{}  P = [1 3]
##              d = 1
## @end group
## @end example
##
## @seealso{graph, digraph, distances, shortestpathtree, allpaths}
## @end deftypefn

function [P, d, edgepath] = shortestpath (G, varargin)

  ## NOTE: When called with a graph or digraph object, Octave's
  ## classdef method dispatch runs the class-internal
  ## @code{shortestpath} method and this free-function body is not
  ## reached.  This file exists both as a canonical documentation
  ## target (so @code{help shortestpath} works outside the context of
  ## an instance) and as a fallback that gives a helpful error for
  ## non-graph inputs.

  if (nargin < 1)
    print_usage ();
  endif

  if (! (isa (G, "graph") || isa (G, "digraph")))
    error ("Octave:invalid-input-arg", ...
           "shortestpath: G must be a graph or digraph object");
  endif

  ## Defensive delegation: classdef method dispatch should intercept
  ## any call with a graph/digraph first arg, but route through dot
  ## notation just in case.
  if (nargout <= 1)
    P = G.shortestpath (varargin{:});
  elseif (nargout == 2)
    [P, d] = G.shortestpath (varargin{:});
  else
    [P, d, edgepath] = G.shortestpath (varargin{:});
  endif

endfunction


## ------------------------------------------------------------------
## BIST blocks
## ------------------------------------------------------------------

## -------------------- basic error cases --------------------

## shortestpath on a non-graph numeric input is an error.
%!error <must be a graph or digraph object>
%! shortestpath (42, 1, 2);

## shortestpath on a non-graph string input is an error.
%!error <must be a graph or digraph object>
%! shortestpath ("foo", 1, 2);

## shortestpath with no args is an error via print_usage.
%!error shortestpath ()

## shortestpath with G alone is an error (missing s and t).
%!error shortestpath (digraph ())

## shortestpath with G and s only is an error (missing t).
%!error shortestpath (digraph (3), 1)

## -------------------- trivial cases --------------------

## Source == target on a single-node digraph returns [1], d=0, ep=[].
%!test
%! G = digraph (1);
%! [P, d, ep] = shortestpath (G, 1, 1);
%! assert (P, 1);
%! assert (d, 0);
%! assert (size (ep), [1, 0]);
%! assert (isa (ep, "double"));

## Source == target on a single-node graph returns [1], d=0, ep=[].
%!test
%! G = graph (1);
%! [P, d, ep] = shortestpath (G, 1, 1);
%! assert (P, 1);
%! assert (d, 0);
%! assert (size (ep), [1, 0]);

## Source == target on a multi-node digraph returns [src], d=0, ep=[].
%!test
%! G = digraph ([1 2 3], [2 3 1], [5 10 15]);
%! [P, d, ep] = shortestpath (G, 2, 2);
%! assert (P, 2);
%! assert (d, 0);
%! assert (size (ep), [1, 0]);

## -------------------- unreachable --------------------

## Edgeless 3-node digraph: shortestpath from 1 to 2 is unreachable.
%!test
%! G = digraph (3);
%! [P, d, ep] = shortestpath (G, 1, 2);
%! assert (size (P), [1, 0]);
%! assert (isa (P, "double"));
%! assert (d, Inf);
%! assert (size (ep), [1, 0]);
%! assert (isa (ep, "double"));

## Edgeless 3-node graph: shortestpath from 1 to 2 is unreachable.
%!test
%! G = graph (3);
%! [P, d, ep] = shortestpath (G, 1, 2);
%! assert (size (P), [1, 0]);
%! assert (d, Inf);
%! assert (size (ep), [1, 0]);

## Two disjoint components (digraph): shortestpath across components
## is unreachable.
%!test
%! G = digraph ([1 3], [2 4]);
%! [P, d, ep] = shortestpath (G, 1, 3);
%! assert (size (P), [1, 0]);
%! assert (d, Inf);
%! assert (size (ep), [1, 0]);

## Two disjoint components (graph): shortestpath across components
## is unreachable.
%!test
%! G = graph ([1 3], [2 4]);
%! [P, d, ep] = shortestpath (G, 1, 3);
%! assert (size (P), [1, 0]);
%! assert (d, Inf);
%! assert (size (ep), [1, 0]);

## Directed edge 1->2: reverse direction 2->1 is unreachable.
%!test
%! G = digraph (1, 2);
%! [P, d, ep] = shortestpath (G, 2, 1);
%! assert (size (P), [1, 0]);
%! assert (d, Inf);
%! assert (size (ep), [1, 0]);

## -------------------- unweighted digraph --------------------

## Single directed edge 1->2: path is [1 2], d=1, ep=[1].
%!test
%! G = digraph (1, 2);
%! [P, d, ep] = shortestpath (G, 1, 2);
%! assert (P, [1, 2]);
%! assert (d, 1);
%! assert (ep, 1);
%! assert (size (ep), [1, 1]);

## 3-node chain 1->2->3: shortestpath from 1 to 3 is [1 2 3], d=2.
%!test
%! G = digraph ([1 2], [2 3]);
%! [P, d, ep] = shortestpath (G, 1, 3);
%! assert (P, [1, 2, 3]);
%! assert (d, 2);
%! assert (size (ep), [1, 2]);

## 3-cycle 1->2->3->1 unweighted, 1->2 path is [1 2], d=1.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! [P, d, ep] = shortestpath (G, 1, 2);
%! assert (P, [1, 2]);
%! assert (d, 1);
%! assert (numel (ep), 1);

## 3-cycle 1->2->3->1 unweighted, 2->1 requires going the long way
## [2 3 1], d=2.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! [P, d, ep] = shortestpath (G, 2, 1);
%! assert (P, [2, 3, 1]);
%! assert (d, 2);
%! assert (numel (ep), 2);

## -------------------- weighted digraph --------------------

## Weighted 3-cycle: 1->2 direct edge has weight 5, path is [1 2].
%!test
%! G = digraph ([1 2 3], [2 3 1], [5 10 15]);
%! [P, d, ep] = shortestpath (G, 1, 2);
%! assert (P, [1, 2]);
%! assert (d, 5);
%! assert (numel (ep), 1);

## Weighted 3-cycle: 3->2 requires going 3->1->2, d=15+5=20.
%!test
%! G = digraph ([1 2 3], [2 3 1], [5 10 15]);
%! [P, d, ep] = shortestpath (G, 3, 2);
%! assert (P, [3, 1, 2]);
%! assert (d, 20);
%! assert (numel (ep), 2);

## Weighted: short indirect path preferred over long direct edge.
## G = digraph([1 1 2], [2 3 3], [5 100 1]); shortestpath(G, 1, 3)
## is [1 2 3] with d=5+1=6 (vs direct 100).
%!test
%! G = digraph ([1 1 2], [2 3 3], [5 100 1]);
%! [P, d, ep] = shortestpath (G, 1, 3);
%! assert (P, [1, 2, 3]);
%! assert (d, 6);
%! assert (numel (ep), 2);

## -------------------- undirected graph --------------------

## Single undirected edge 1--2: forward path.
%!test
%! G = graph (1, 2);
%! [P, d, ep] = shortestpath (G, 1, 2);
%! assert (P, [1, 2]);
%! assert (d, 1);

## Single undirected edge 1--2: reverse path (symmetric).
%!test
%! G = graph (1, 2);
%! [P, d, ep] = shortestpath (G, 2, 1);
%! assert (P, [2, 1]);
%! assert (d, 1);

## Undirected path 1--2--3: forward.
%!test
%! G = graph ([1 2], [2 3]);
%! [P, d, ep] = shortestpath (G, 1, 3);
%! assert (P, [1, 2, 3]);
%! assert (d, 2);

## Undirected path 1--2--3: reverse (symmetric).
%!test
%! G = graph ([1 2], [2 3]);
%! [P, d, ep] = shortestpath (G, 3, 1);
%! assert (P, [3, 2, 1]);
%! assert (d, 2);

## Weighted undirected graph: shortest path prefers indirect route
## when it's cheaper than the direct edge.
%!test
%! G = graph ([1 1 2], [2 3 3], [5 100 1]);
%! [P, d, ep] = shortestpath (G, 1, 3);
%! assert (P, [1, 2, 3]);
%! assert (d, 6);

## -------------------- self-loops ignored --------------------

## Self-loop on source node is not included in the path (digraph).
%!test
%! G = digraph ([1 1], [1 2], [3, 7]);
%! [P, d, ep] = shortestpath (G, 1, 2);
%! assert (P, [1, 2]);
%! assert (d, 7);

## Self-loop on source node is not included in the path (graph).
%!test
%! G = graph ([1 1], [1 2], [3, 7]);
%! [P, d, ep] = shortestpath (G, 1, 2);
%! assert (P, [1, 2]);
%! assert (d, 7);

## -------------------- output shape --------------------

## P is a row vector (1 x k) for numeric input.
%!test
%! G = digraph ([1 2], [2 3]);
%! P = shortestpath (G, 1, 3);
%! assert (size (P, 1), 1);
%! assert (size (P, 2), 3);

## edgepath is a row vector (1 x (k-1)).
%!test
%! G = digraph ([1 2 3], [2 3 4]);
%! [P, d, ep] = shortestpath (G, 1, 4);
%! assert (size (ep, 1), 1);
%! assert (size (ep, 2), numel (P) - 1);

## Single-output form returns only P.
%!test
%! G = digraph ([1 2], [2 3]);
%! P = shortestpath (G, 1, 3);
%! assert (P, [1, 2, 3]);

## Two-output form returns P and d.
%!test
%! G = digraph ([1 2 3], [2 3 1], [5 10 15]);
%! [P, d] = shortestpath (G, 1, 3);
%! assert (P, [1, 2, 3]);
%! assert (d, 15);

## -------------------- edgepath validity --------------------

## For a digraph, G.Edges.EndNodes(ep(i), :) equals [P(i), P(i+1)].
%!test
%! G = digraph ([1 1 2 2 3], [2 3 3 4 4], [1 4 2 5 1]);
%! [P, d, ep] = shortestpath (G, 1, 4);
%! E = G.Edges.EndNodes;
%! for ii = 1:numel (ep)
%!   assert (E(ep(ii), :), [P(ii), P(ii+1)]);
%! endfor

## For a weighted digraph, sum of Weight(ep) equals d.
%!test
%! G = digraph ([1 1 2 2 3], [2 3 3 4 4], [1 4 2 5 1]);
%! [P, d, ep] = shortestpath (G, 1, 4);
%! w = G.Edges.Weight;
%! assert (sum (w(ep)), d);

## For an undirected graph, edgepath indices correspond to sorted
## endpoint pairs in either direction along P.
%!test
%! G = graph ([1 2 3], [2 3 1], [5 10 15]);
%! [P, d, ep] = shortestpath (G, 2, 3);
%! E = G.Edges.EndNodes;
%! for ii = 1:numel (ep)
%!   a = P(ii);
%!   b = P(ii+1);
%!   assert (sort (E(ep(ii), :)), sort ([a, b]));
%! endfor

## -------------------- multigraph (digraph) --------------------

## Parallel edges: shortestpath uses the cheapest.  The digraph
## [1->2 weight 7] and [1->2 weight 3] has two parallel edges;
## shortestpath should return d=3.  The edge index of the cheapest
## parallel edge is reported in edgepath, so
## @code{sum (G.Edges.Weight (ep))} equals @var{d}.
%!test
%! G = digraph ([1 1], [2 2], [7, 3], "multigraph");
%! [P, d, ep] = shortestpath (G, 1, 2);
%! assert (P, [1, 2]);
%! assert (d, 3);
%! assert (numel (ep), 1);
%! ## The reported edge must be one of the two 1->2 edges and carry
%! ## the min weight along the pair.
%! E = G.Edges.EndNodes;
%! assert (E(ep, :), [1, 2]);
%! assert (sum (G.Edges.Weight(ep)), d);

## Parallel edges, unweighted multigraph: edgepath refers to the
## first parallel (all have weight 1 so any is acceptable).
%!test
%! G = digraph ([1 1], [2 2], "multigraph");
%! [P, d, ep] = shortestpath (G, 1, 2);
%! assert (P, [1, 2]);
%! assert (d, 1);
%! assert (numel (ep), 1);

## Parallel edges on a 3-node chain multigraph: picks min along each
## segment so sum(Weight(ep)) equals d.
%!test
%! G = digraph ([1 1 2 2], [2 2 3 3], [10 2 20 1], "multigraph");
%! [P, d, ep] = shortestpath (G, 1, 3);
%! assert (P, [1, 2, 3]);
%! assert (d, 3);
%! assert (sum (G.Edges.Weight(ep)), 3);

## -------------------- named nodes --------------------

## Numeric s and t on a named digraph return numeric P.
%!test
%! G = digraph ([1 2 3], [2 3 1], [5 10 15], {"a", "b", "c"});
%! [P, d] = shortestpath (G, 1, 2);
%! assert (P, [1, 2]);
%! assert (isa (P, "double"));
%! assert (d, 5);

## String s on a named digraph returns cellstr P.
%!test
%! G = digraph ([1 2 3], [2 3 1], [5 10 15], {"a", "b", "c"});
%! [P, d] = shortestpath (G, "a", 2);
%! assert (iscellstr (P));
%! assert (P, {"a", "b"});
%! assert (d, 5);

## Cellstr s and t return cellstr P (row vector).
%!test
%! G = digraph ([1 2 3], [2 3 1], [5 10 15], {"a", "b", "c"});
%! [P, d] = shortestpath (G, {"b"}, {"a"});
%! assert (iscellstr (P));
%! assert (P, {"b", "c", "a"});
%! assert (size (P, 1), 1);
%! assert (d, 25);

## Mixed numeric and string on named graph.
%!test
%! G = graph ([1 2 3], [2 3 1], [5 10 15], {"a", "b", "c"});
%! [P, d] = shortestpath (G, "a", 3);
%! assert (iscellstr (P));
%! assert (P, {"a", "c"});
%! assert (d, 15);

## Unreachable named returns empty cellstr path (1 x 0).
%!test
%! G = digraph ([1 2], [2 3], [], {"a", "b", "c"});
%! [P, d, ep] = shortestpath (G, "c", "a");
%! assert (iscellstr (P));
%! assert (size (P), [1, 0]);
%! assert (d, Inf);
%! assert (size (ep), [1, 0]);

## -------------------- named-nodes error cases -------------------

## String src on a digraph without node names errors.
%!error <no node names>
%! G = digraph (3);
%! shortestpath (G, "a", 2);

## Missing node name on src errors.
%!error <not found>
%! G = digraph ([1 2], [2 3], [], {"a", "b", "c"});
%! shortestpath (G, "z", "a");

## Missing node name on tgt errors.
%!error <not found>
%! G = digraph ([1 2], [2 3], [], {"a", "b", "c"});
%! shortestpath (G, "a", "z");

## -------------------- numeric-index validation -----------------

## Out-of-range numeric src errors.
%!error <invalid node index>
%! G = digraph (3);
%! shortestpath (G, 5, 1);

## Zero numeric src errors.
%!error <invalid node index>
%! G = digraph (3);
%! shortestpath (G, 0, 1);

## Non-integer numeric src errors.
%!error <invalid node index>
%! G = digraph (3);
%! shortestpath (G, 1.5, 1);

## Out-of-range numeric tgt errors.
%!error <invalid node index>
%! G = digraph (3);
%! shortestpath (G, 1, 5);

## Non-scalar numeric src errors (vector not allowed for s).
%!error <scalar node identifier>
%! G = digraph (3);
%! shortestpath (G, [1 2], 3);

## -------------------- dot notation dispatch --------------------

## G.shortestpath(s, t) matches shortestpath(G, s, t) for digraph.
%!test
%! G = digraph ([1 2 3], [2 3 1], [5 10 15]);
%! [P1, d1, e1] = shortestpath (G, 1, 3);
%! [P2, d2, e2] = G.shortestpath (1, 3);
%! assert (P1, P2);
%! assert (d1, d2);
%! assert (e1, e2);

## G.shortestpath(s, t) matches shortestpath(G, s, t) for graph.
%!test
%! G = graph ([1 2 3], [2 3 1], [5 10 15]);
%! [P1, d1, e1] = shortestpath (G, 2, 1);
%! [P2, d2, e2] = G.shortestpath (2, 1);
%! assert (P1, P2);
%! assert (d1, d2);
%! assert (e1, e2);

## -------------------- US-P08 'Method' default auto-dispatch -----

## On a digraph with a negative edge weight and no negative cycle,
## the default 'auto' method promotes to Bellman-Ford ("mixed") and
## succeeds.  1->2->3 has cost 1 + (-1) = 0; no direct 1->3 edge.
%!test
%! G = digraph ([1 2], [2 3], [1, -1]);
%! [P, d, ep] = shortestpath (G, 1, 3);
%! assert (P, [1, 2, 3]);
%! assert (d, 0);
%! assert (numel (ep), 2);

## On an undirected graph with any negative edge weight, the default
## 'auto' method promotes to 'mixed' and errors: an undirected
## negative edge is a negative cycle by itself (u-v-u = 2*w < 0).
%!error <negative cycle>
%! G = graph ([1 2], [2 3], [1, -1]);
%! shortestpath (G, 1, 3);

## -------------------- US-P08 'Method','mixed' ------------------

## 'mixed' on a digraph with negative weights finds the cheapest path.
## G = digraph([1 2 1], [2 3 3], [5 -3 10]); 1->2->3 has d = 2,
## 1->3 direct has d = 10; Bellman-Ford picks d=2.
%!test
%! G = digraph ([1 2 1], [2 3 3], [5 -3 10]);
%! [P, d, ep] = shortestpath (G, 1, 3, "Method", "mixed");
%! assert (P, [1, 2, 3]);
%! assert (d, 2);
%! assert (numel (ep), 2);

## 'mixed' on a negative-weight DAG of length 3.
%!test
%! G = digraph ([1 2 1], [2 3 3], [-2 -3 -10]);
%! [P, d] = shortestpath (G, 1, 3, "Method", "mixed");
%! assert (d, -10);
%! assert (P, [1, 3]);

## 'mixed' on a nonneg-weighted digraph matches default (Dijkstra).
%!test
%! G = digraph ([1 2 3], [2 3 1], [5 10 15]);
%! [P1, d1] = shortestpath (G, 1, 3);
%! [P2, d2] = shortestpath (G, 1, 3, "Method", "mixed");
%! assert (P1, P2);
%! assert (d1, d2);

## 'mixed' errors on a negative cycle (directed 3-cycle total < 0).
%!error <negative cycle>
%! G = digraph ([1 2 3], [2 3 1], [1 1 -10]);
%! shortestpath (G, 1, 3, "Method", "mixed");

## 'mixed' on undirected graph with negative weight errors
## (u-v-u = 2*w < 0 is a negative cycle).
%!error <negative cycle>
%! G = graph ([1 2], [2 3], [1, -1]);
%! shortestpath (G, 1, 3, "Method", "mixed");

## 'mixed' on undirected graph with nonneg weights matches default.
%!test
%! G = graph ([1 2 3], [2 3 1], [5 10 15]);
%! [P1, d1] = shortestpath (G, 1, 3);
%! [P2, d2] = shortestpath (G, 1, 3, "Method", "mixed");
%! assert (P1, P2);
%! assert (d1, d2);

## 'mixed' on a named digraph preserves cellstr output.
%!test
%! G = digraph ([1 2 1], [2 3 3], [5 -3 10], {"a", "b", "c"});
%! P = shortestpath (G, "a", "c", "Method", "mixed");
%! assert (iscellstr (P));
%! assert (P, {"a", "b", "c"});

## 'mixed' returns empty path for unreachable target.
%!test
%! G = digraph ([1 2], [2 3], [1, -1]);
%! [P, d, ep] = shortestpath (G, 3, 1, "Method", "mixed");
%! assert (size (P), [1, 0]);
%! assert (d, Inf);
%! assert (size (ep), [1, 0]);

## 'mixed' with s == t returns the trivial path.
%!test
%! G = digraph ([1 2 1], [2 3 3], [5 -3 10]);
%! [P, d, ep] = shortestpath (G, 2, 2, "Method", "mixed");
%! assert (P, 2);
%! assert (d, 0);
%! assert (size (ep), [1, 0]);

## 'mixed' on a multigraph digraph: cheapest parallel edge (even if
## negative) is selected.  Edges 1->2 with weights [2, -1, 5]; the
## min parallel edge is -1, so d=-1 and ep points to that edge.
%!test
%! G = digraph ([1 1 1], [2 2 2], [2, -1, 5], "multigraph");
%! [P, d, ep] = shortestpath (G, 1, 2, "Method", "mixed");
%! assert (P, [1, 2]);
%! assert (d, -1);
%! assert (numel (ep), 1);
%! assert (G.Edges.Weight(ep), -1);

## 'mixed' Bellman-Ford classic example (CLRS Figure 24.4).  Node
## mapping s=1, t=2, y=3, x=4, z=5.  Edges: 1->2(6), 1->3(7), 2->3(8),
## 2->4(5), 2->5(-4), 3->4(-3), 3->5(9), 4->2(-2), 5->1(2), 5->4(7).
## Shortest path 1->5 is [1 3 4 2 5] with d = 7 + (-3) + (-2) + (-4) = -2.
%!test
%! s = [1 1 2 2 2 3 3 4 5 5];
%! t = [2 3 3 4 5 4 5 2 1 4];
%! w = [6 7 8 5 -4 -3 9 -2 2 7];
%! G = digraph (s, t, w);
%! [P, d] = shortestpath (G, 1, 5, "Method", "mixed");
%! assert (d, -2);
%! assert (P, [1, 3, 4, 2, 5]);

## -------------------- US-P08 'Method','positive' ----------------

## 'positive' errors on a negative edge weight (digraph).
%!error <negative edge weights>
%! G = digraph ([1 2], [2 3], [1, -1]);
%! shortestpath (G, 1, 3, "Method", "positive");

## 'positive' errors on a negative edge weight (graph).
%!error <negative edge weights>
%! G = graph ([1 2], [2 3], [1, -1]);
%! shortestpath (G, 1, 3, "Method", "positive");

## 'positive' matches default on nonneg-weighted digraph.
%!test
%! G = digraph ([1 2 3], [2 3 1], [5 10 15]);
%! [P1, d1] = shortestpath (G, 1, 3);
%! [P2, d2] = shortestpath (G, 1, 3, "Method", "positive");
%! assert (P1, P2);
%! assert (d1, d2);

## -------------------- US-P08 'Method','auto' --------------------

## 'auto' is the explicit default and matches.
%!test
%! G = digraph ([1 2 3], [2 3 1], [5 10 15]);
%! [P1, d1] = shortestpath (G, 1, 3);
%! [P2, d2] = shortestpath (G, 1, 3, "Method", "auto");
%! assert (P1, P2);
%! assert (d1, d2);

## 'auto' on digraph with negative weights uses Bellman-Ford.
%!test
%! G = digraph ([1 2 1], [2 3 3], [5 -3 10]);
%! [P, d] = shortestpath (G, 1, 3, "Method", "auto");
%! assert (d, 2);
%! assert (P, [1, 2, 3]);

## 'auto' errors on a negative cycle (via its Bellman-Ford promotion).
%!error <negative cycle>
%! G = digraph ([1 2 3], [2 3 1], [1 1 -10]);
%! shortestpath (G, 1, 2, "Method", "auto");

## -------------------- US-P08 'Method' case-insensitive ---------

## Method key and value are both case-insensitive.
%!test
%! G = digraph ([1 2 1], [2 3 3], [5 -3 10]);
%! [P1, d1] = shortestpath (G, 1, 3, "Method", "mixed");
%! [P2, d2] = shortestpath (G, 1, 3, "METHOD", "MIXED");
%! [P3, d3] = shortestpath (G, 1, 3, "method", "Mixed");
%! assert (P1, P2);
%! assert (P1, P3);
%! assert (d1, d2);
%! assert (d1, d3);

## -------------------- US-P08 'Method' error cases --------------

## Unknown Method value errors.
%!error <Method|method|unknown>
%! G = digraph ([1 2], [2 3]);
%! shortestpath (G, 1, 3, "Method", "bogus");

## Missing Method value (odd NV pair).
%!error <Method|value|pair>
%! G = digraph ([1 2], [2 3]);
%! shortestpath (G, 1, 3, "Method");

## Numeric Method value errors.
%!error <Method.*string|string>
%! G = digraph ([1 2], [2 3]);
%! shortestpath (G, 1, 3, "Method", 7);

## -------------------- US-P08 dot-notation dispatch ------------

## Dot-notation with 'Method','mixed' matches free-function call.
%!test
%! G = digraph ([1 2 1], [2 3 3], [5 -3 10]);
%! [P1, d1] = shortestpath (G, 1, 3, "Method", "mixed");
%! [P2, d2] = G.shortestpath (1, 3, "Method", "mixed");
%! assert (P1, P2);
%! assert (d1, d2);

## Dot-notation on a graph with 'Method','positive' matches.
%!test
%! G = graph ([1 2 3], [2 3 1], [5 10 15]);
%! [P1, d1] = shortestpath (G, 1, 3, "Method", "positive");
%! [P2, d2] = G.shortestpath (1, 3, "Method", "positive");
%! assert (P1, P2);
%! assert (d1, d2);

## -------------------- larger graph sanity ----------------------

## Siever-style 9-node digraph: shortestpath(1, 9) is
## [1 2 3 4 5 9], d=5.
%!test
%! s = [1 2 3 3 4 5 5 6 7 7 8 9];
%! t = [2 3 2 4 5 6 9 7 8 9 7 4];
%! G = digraph (s, t);
%! [P, d, ep] = shortestpath (G, 1, 9);
%! assert (P, [1, 2, 3, 4, 5, 9]);
%! assert (d, 5);
%! assert (numel (ep), 5);

## Chain 1->2->...->10: shortestpath(1, 10) = 1:10, d=9.
%!test
%! G = digraph (1:9, 2:10);
%! [P, d, ep] = shortestpath (G, 1, 10);
%! assert (P, 1:10);
%! assert (d, 9);
%! assert (numel (ep), 9);

## Undirected chain 1--2--...--10: shortestpath(3, 7) = 3:7, d=4.
%!test
%! G = graph (1:9, 2:10);
%! [P, d, ep] = shortestpath (G, 3, 7);
%! assert (P, 3:7);
%! assert (d, 4);
%! assert (numel (ep), 4);
