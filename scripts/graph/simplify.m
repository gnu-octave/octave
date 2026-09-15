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
## @deftypefn  {} {@var{H} =} simplify (@var{G})
## @deftypefnx {} {@var{H} =} simplify (@var{G}, @var{method})
## @deftypefnx {} {@var{H} =} simplify (@var{G}, @dots{}, @qcode{"omitselfloops"})
## @deftypefnx {} {@var{H} =} simplify (@var{G}, @dots{}, @var{Name}, @var{Value})
## Return a simplified copy of the graph or digraph @var{G}: parallel
## edges are collapsed into a single edge, and (optionally) self-loops
## are dropped.
##
## @var{G} may be any @code{graph} or @code{digraph} object.  The output
## @var{H} has the same class and the same node set as @var{G} (same
## number of nodes in the same order, @code{Nodes.Name} preserved when
## present) but at most one edge between each ordered pair of nodes.
## For an undirected @code{graph}, which in this Octave build cannot
## store parallel edges, the edge set is passed through unchanged except
## for any self-loop removal the caller requests.
##
## When @var{G} has explicit edge weights, parallel edges are collapsed
## by aggregating their weights with @var{method}:
##
## @itemize
## @item @qcode{"sum"} (the default): weights are summed.
## @item @qcode{"mean"}: weights are averaged.
## @item @qcode{"min"}: the smallest weight wins.
## @item @qcode{"max"}: the largest weight wins.
## @end itemize
##
## @noindent
## Unweighted inputs produce unweighted outputs regardless of
## @var{method} (parallel-edge counts are not written back as weights,
## matching MATLAB).
##
## The following Name-Value options are supported (case-insensitive):
##
## @itemize
## @item @qcode{"SelfLoops"}: @qcode{"keep"} (the default) retains any
## self-loops; @qcode{"discard"} drops them.
## @item @qcode{"AggregationVariables"}: synonym for the positional
## @var{method} argument.  Accepts @qcode{"sum"}, @qcode{"mean"},
## @qcode{"min"}, or @qcode{"max"}; the last specification wins.
## @end itemize
##
## The bare trailing flag @qcode{"omitselfloops"} (without a paired
## value) is accepted as a concise synonym for
## @qcode{"SelfLoops", "discard"}.  It matches the spelling used by the
## @code{digraph} constructor.
##
## @example
## @group
## G = digraph ([1 1 2], [2 2 3], [10 20 30], "multigraph");
## H = simplify (G);            # sum aggregation, 2 edges
## H.Edges.EndNodes             # @result{} [1 2; 2 3]
## H.Edges.Weight               # @result{} [30; 30]
##
## H2 = simplify (G, "max");    # max aggregation
## H2.Edges.Weight              # @result{} [20; 30]
##
## G2 = digraph ([1 2 3], [2 2 3]);
## H3 = simplify (G2, "omitselfloops");
## H3.Edges.EndNodes            # @result{} [1 2]
## @end group
## @end example
##
## @seealso{digraph, graph, ismultigraph, numedges}
## @end deftypefn

function H = simplify (G, varargin)

  ## NOTE: When called with a graph or digraph object, Octave's classdef
  ## method dispatch runs the class-internal @code{simplify} method and
  ## this free-function body is not reached.  This file exists both as
  ## a canonical documentation target (so @code{help simplify} works
  ## outside the context of an instance) and as a fallback that gives a
  ## helpful error for non-graph, non-digraph inputs.

  if (nargin < 1)
    print_usage ();
  endif

  if (! (isa (G, "graph") || isa (G, "digraph")))
    error ("Octave:invalid-input-arg", ...
           "simplify: G must be a graph or digraph object");
  endif

  ## Defensive delegation: classdef method dispatch should intercept any
  ## call with a graph/digraph first arg, but route through dot notation
  ## just in case.
  H = G.simplify (varargin{:});

endfunction


## ------------------------------------------------------------------
## BIST blocks
## ------------------------------------------------------------------

## -------------------- basic error cases --------------------

## simplify on a non-graph numeric input is an error.
%!error <must be a graph or digraph>
%! simplify (42);

## simplify on a non-graph string input is an error.
%!error <must be a graph or digraph>
%! simplify ("foo");

## simplify on a non-graph cell input is an error.
%!error <must be a graph or digraph>
%! simplify ({1, 2});

## simplify with no args is an error via print_usage.
%!error simplify ()

## -------------------- return type --------------------

## simplify on a digraph returns a digraph.
%!test
%! G = digraph ([1 2], [2 3]);
%! H = simplify (G);
%! assert (isa (H, "digraph"));

## simplify on a graph returns a graph.
%!test
%! G = graph ([1 2], [2 3]);
%! H = simplify (G);
%! assert (isa (H, "graph"));

## simplify on an empty digraph returns an empty digraph.
%!test
%! G = digraph ();
%! H = simplify (G);
%! assert (isa (H, "digraph"));
%! assert (numnodes (H), 0);
%! assert (numedges (H), 0);

## simplify on an empty graph returns an empty graph.
%!test
%! G = graph ();
%! H = simplify (G);
%! assert (isa (H, "graph"));
%! assert (numnodes (H), 0);
%! assert (numedges (H), 0);

## -------------------- digraph: simple-graph pass-through ------

## simplify on a simple (non-multigraph) digraph is a no-op.
%!test
%! G = digraph ([1 2 3], [2 3 4]);
%! H = simplify (G);
%! assert (numnodes (H), numnodes (G));
%! assert (numedges (H), numedges (G));
%! assert (H.Edges.EndNodes, G.Edges.EndNodes);

## Simple digraph with weights: pass-through preserves weights.
%!test
%! G = digraph ([1 2 3], [2 3 4], [10 20 30]);
%! H = simplify (G);
%! assert (H.Edges.EndNodes, G.Edges.EndNodes);
%! assert (H.Edges.Weight, G.Edges.Weight);

## Five isolated nodes: still five isolated nodes after simplify.
%!test
%! G = digraph (5);
%! H = simplify (G);
%! assert (numnodes (H), 5);
%! assert (numedges (H), 0);

## -------------------- digraph: multigraph simplification ------

## Multigraph with parallel edges: collapse to simple digraph.
%!test
%! G = digraph ([1 1 2], [2 2 3], "multigraph");
%! H = simplify (G);
%! assert (! ismultigraph (H));
%! assert (numedges (H), 2);
%! assert (H.Edges.EndNodes, [1 2; 2 3]);

## Multigraph with unweighted parallel edges: result stays unweighted.
%!test
%! G = digraph ([1 1 2], [2 2 3], "multigraph");
%! H = simplify (G);
%! assert (! isfield (H.Edges, "Weight"));

## Weighted multigraph: default sum aggregation collapses weights.
%!test
%! G = digraph ([1 1 2], [2 2 3], [10 20 30], "multigraph");
%! H = simplify (G);
%! assert (numedges (H), 2);
%! assert (H.Edges.EndNodes, [1 2; 2 3]);
%! assert (H.Edges.Weight, [30; 30]);

## Three parallel edges collapse into one with sum.
%!test
%! G = digraph ([1 1 1], [2 2 2], [10 20 30], "multigraph");
%! H = simplify (G);
%! assert (numedges (H), 1);
%! assert (H.Edges.EndNodes, [1 2]);
%! assert (H.Edges.Weight, 60);

## -------------------- digraph: aggregation methods ------------

## Mean aggregation.
%!test
%! G = digraph ([1 1 2], [2 2 3], [10 20 30], "multigraph");
%! H = simplify (G, "mean");
%! assert (H.Edges.Weight, [15; 30]);

## Mean aggregation on three parallel edges.
%!test
%! G = digraph ([1 1 1], [2 2 2], [10 20 30], "multigraph");
%! H = simplify (G, "mean");
%! assert (H.Edges.Weight, 20);

## Min aggregation.
%!test
%! G = digraph ([1 1 1], [2 2 2], [10 5 20], "multigraph");
%! H = simplify (G, "min");
%! assert (H.Edges.Weight, 5);

## Max aggregation.
%!test
%! G = digraph ([1 1 1], [2 2 2], [10 5 20], "multigraph");
%! H = simplify (G, "max");
%! assert (H.Edges.Weight, 20);

## Explicit sum aggregation is the same as the default.
%!test
%! G = digraph ([1 1 2], [2 2 3], [1 2 3], "multigraph");
%! H1 = simplify (G);
%! H2 = simplify (G, "sum");
%! assert (H1.Edges.Weight, H2.Edges.Weight);

## Case-insensitive method name.
%!test
%! G = digraph ([1 1], [2 2], [10 20], "multigraph");
%! H1 = simplify (G, "SUM");
%! H2 = simplify (G, "Mean");
%! assert (H1.Edges.Weight, 30);
%! assert (H2.Edges.Weight, 15);

## Min with negative weights.
%!test
%! G = digraph ([1 1], [2 2], [-1 -5], "multigraph");
%! H = simplify (G, "min");
%! assert (H.Edges.Weight, -5);

## Max with negative weights.
%!test
%! G = digraph ([1 1], [2 2], [-1 -5], "multigraph");
%! H = simplify (G, "max");
%! assert (H.Edges.Weight, -1);

## -------------------- digraph: self-loops ---------------------

## omitselfloops flag drops self-loops.
%!test
%! G = digraph ([1 2 3], [2 2 3]);
%! H = simplify (G, "omitselfloops");
%! assert (numedges (H), 1);
%! assert (H.Edges.EndNodes, [1 2]);

## omitselfloops drops multiple self-loops across many nodes.
%!test
%! G = digraph ([1 1 2 3], [1 2 2 3]);
%! H = simplify (G, "omitselfloops");
%! assert (numedges (H), 1);
%! assert (H.Edges.EndNodes, [1 2]);

## Simple digraph with no self-loops: omitselfloops is a no-op.
%!test
%! G = digraph ([1 2], [2 3]);
%! H = simplify (G, "omitselfloops");
%! assert (H.Edges.EndNodes, G.Edges.EndNodes);

## SelfLoops Name-Value: discard drops self-loops.
%!test
%! G = digraph ([1 2 3], [2 2 3]);
%! H = simplify (G, "SelfLoops", "discard");
%! assert (numedges (H), 1);
%! assert (H.Edges.EndNodes, [1 2]);

## SelfLoops Name-Value: keep retains self-loops (default).
%!test
%! G = digraph ([1 2 3], [2 2 3]);
%! H = simplify (G, "SelfLoops", "keep");
%! assert (numedges (H), 3);

## Self-loop in a multigraph is aggregated and optionally kept.
%!test
%! G = digraph ([1 1 2], [1 1 2], [10 20 30], "multigraph");
%! H = simplify (G);
%! assert (numedges (H), 2);
%! assert (H.Edges.EndNodes, [1 1; 2 2]);
%! assert (H.Edges.Weight, [30; 30]);

## Self-loop in a multigraph is dropped with omitselfloops.
%!test
%! G = digraph ([1 1 2 2], [1 1 2 3], [10 20 30 40], "multigraph");
%! H = simplify (G, "sum", "omitselfloops");
%! assert (numedges (H), 1);
%! assert (H.Edges.EndNodes, [2 3]);
%! assert (H.Edges.Weight, 40);

## -------------------- digraph: named nodes --------------------

## Named digraph: names are preserved across simplify.
%!test
%! G = digraph ([1 1 2], [2 2 3], [], {"a","b","c"}, "multigraph");
%! H = simplify (G);
%! assert (H.Nodes.Name, {"a"; "b"; "c"});
%! assert (numedges (H), 2);
%! assert (H.Edges.EndNodes, [1 2; 2 3]);

## Named digraph with weights.
%!test
%! G = digraph ([1 1 2], [2 2 3], [10 20 30], {"a","b","c"}, "multigraph");
%! H = simplify (G, "sum");
%! assert (H.Nodes.Name, {"a"; "b"; "c"});
%! assert (H.Edges.Weight, [30; 30]);

## Named digraph with isolated nodes preserved.
%!test
%! G = digraph ([1 1], [2 2], [], {"a","b","c","d"}, "multigraph");
%! H = simplify (G);
%! assert (numnodes (H), 4);
%! assert (H.Nodes.Name, {"a"; "b"; "c"; "d"});
%! assert (numedges (H), 1);

## -------------------- digraph: isolated nodes -----------------

## Simplify preserves isolated nodes (no-edge digraph with N nodes).
%!test
%! G = digraph ([1 1], [2 2], [10 20], 5, "multigraph");
%! H = simplify (G);
%! assert (numnodes (H), 5);
%! assert (numedges (H), 1);
%! assert (H.Edges.EndNodes, [1 2]);

## -------------------- digraph: directed is preserved ----------

## Reverse-direction parallel edges are kept as distinct edges.
%!test
%! G = digraph ([1 2 2], [2 1 1], "multigraph");
%! H = simplify (G);
%! assert (numedges (H), 2);
%! assert (H.Edges.EndNodes, [1 2; 2 1]);

## Asymmetric weighted parallel edges: each direction aggregates
## independently.
%!test
%! G = digraph ([1 1 2 2], [2 2 1 1], [10 20 30 40], "multigraph");
%! H = simplify (G, "sum");
%! assert (H.Edges.EndNodes, [1 2; 2 1]);
%! assert (H.Edges.Weight, [30; 70]);

## -------------------- graph class -----------------------------

## simplify on a simple graph is a no-op.
%!test
%! G = graph ([1 2 3], [2 3 4]);
%! H = simplify (G);
%! assert (numnodes (H), numnodes (G));
%! assert (numedges (H), numedges (G));
%! assert (H.Edges.EndNodes, G.Edges.EndNodes);

## Weighted simple graph pass-through.
%!test
%! G = graph ([1 2], [2 3], [10 20]);
%! H = simplify (G);
%! assert (H.Edges.EndNodes, G.Edges.EndNodes);
%! assert (H.Edges.Weight, G.Edges.Weight);

## graph with self-loop: omitselfloops drops it.
%!test
%! G = graph ([1 2 3], [2 2 3]);
%! H = simplify (G, "omitselfloops");
%! assert (numedges (H), 1);
%! assert (H.Edges.EndNodes, [1 2]);

## graph with self-loops and SelfLoops=discard.
%!test
%! G = graph ([1 2 3], [2 2 3]);
%! H = simplify (G, "SelfLoops", "discard");
%! assert (numedges (H), 1);

## graph with named nodes: names preserved after simplify.
%!test
%! G = graph ([1 2], [2 3], [], {"a","b","c"});
%! H = simplify (G);
%! assert (H.Nodes.Name, {"a"; "b"; "c"});
%! assert (numedges (H), 2);

## graph with isolated nodes preserved.
%!test
%! G = graph ([1], [2], [], 5);
%! H = simplify (G);
%! assert (numnodes (H), 5);
%! assert (numedges (H), 1);

## -------------------- AggregationVariables --------------------

## AggregationVariables Name-Value form with sum.
%!test
%! G = digraph ([1 1], [2 2], [10 20], "multigraph");
%! H = simplify (G, "AggregationVariables", "sum");
%! assert (H.Edges.Weight, 30);

## AggregationVariables Name-Value form with mean.
%!test
%! G = digraph ([1 1], [2 2], [10 20], "multigraph");
%! H = simplify (G, "AggregationVariables", "mean");
%! assert (H.Edges.Weight, 15);

## AggregationVariables overrides a positional method (last wins).
%!test
%! G = digraph ([1 1], [2 2], [10 20], "multigraph");
%! H = simplify (G, "sum", "AggregationVariables", "max");
%! assert (H.Edges.Weight, 20);

## -------------------- option error cases ----------------------

## Unknown option name.
%!error <unknown option|SelfLoops|AggregationVariables>
%! G = digraph ([1 2], [2 3]);
%! simplify (G, "Bogus", "stable");

## Invalid SelfLoops value.
%!error <SelfLoops>
%! G = digraph ([1 2], [2 3]);
%! simplify (G, "SelfLoops", "whatever");

## Non-char SelfLoops value.
%!error <SelfLoops>
%! G = digraph ([1 2], [2 3]);
%! simplify (G, "SelfLoops", 7);

## Invalid AggregationVariables value.
%!error <AggregationVariables>
%! G = digraph ([1 2], [2 3]);
%! simplify (G, "AggregationVariables", "bogus");

## Non-char AggregationVariables value.
%!error <AggregationVariables>
%! G = digraph ([1 2], [2 3]);
%! simplify (G, "AggregationVariables", 7);

## Missing value after SelfLoops.
%!error <missing|pair>
%! G = digraph ([1 2], [2 3]);
%! simplify (G, "SelfLoops");

## -------------------- dot notation dispatch -------------------

## G.simplify() matches simplify(G) for digraph.
%!test
%! G = digraph ([1 1 2], [2 2 3], "multigraph");
%! H1 = simplify (G);
%! H2 = G.simplify ();
%! assert (H1.Edges.EndNodes, H2.Edges.EndNodes);
%! assert (numnodes (H1), numnodes (H2));

## G.simplify(method) matches simplify(G, method).
%!test
%! G = digraph ([1 1], [2 2], [10 20], "multigraph");
%! H1 = simplify (G, "mean");
%! H2 = G.simplify ("mean");
%! assert (H1.Edges.Weight, H2.Edges.Weight);

## G.simplify() on a graph.
%!test
%! G = graph ([1 2 3], [2 2 3]);
%! H1 = simplify (G, "omitselfloops");
%! H2 = G.simplify ("omitselfloops");
%! assert (H1.Edges.EndNodes, H2.Edges.EndNodes);

## -------------------- idempotence -----------------------------

## simplify is idempotent: simplify(simplify(G)) == simplify(G).
%!test
%! G = digraph ([1 1 2 2], [2 2 2 3], [1 2 3 4], "multigraph");
%! H1 = simplify (G, "sum");
%! H2 = simplify (H1, "sum");
%! assert (H1.Edges.EndNodes, H2.Edges.EndNodes);
%! assert (H1.Edges.Weight, H2.Edges.Weight);

## Idempotence on a non-multigraph input.
%!test
%! G = digraph ([1 2 3], [2 3 4]);
%! H1 = simplify (G);
%! H2 = simplify (H1);
%! assert (H1.Edges.EndNodes, H2.Edges.EndNodes);

## -------------------- larger example --------------------------

## Mixed multigraph with self-loops and parallel edges.
%!test
%! ## Edges: 1->2 (x3), 2->3 (x2), 3->3 (x1), 1->3 (x1)
%! s = [1 1 1 2 2 3 1];
%! t = [2 2 2 3 3 3 3];
%! w = [1 2 3 4 5 6 7];
%! G = digraph (s, t, w, "multigraph");
%! H = simplify (G, "sum");
%! assert (numedges (H), 4);
%! assert (H.Edges.EndNodes, [1 2; 1 3; 2 3; 3 3]);
%! assert (H.Edges.Weight, [6; 7; 9; 6]);

## Same mixed multigraph with omitselfloops.
%!test
%! s = [1 1 1 2 2 3 1];
%! t = [2 2 2 3 3 3 3];
%! w = [1 2 3 4 5 6 7];
%! G = digraph (s, t, w, "multigraph");
%! H = simplify (G, "sum", "omitselfloops");
%! assert (numedges (H), 3);
%! assert (H.Edges.EndNodes, [1 2; 1 3; 2 3]);
%! assert (H.Edges.Weight, [6; 7; 9]);
