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
## @deftypefn  {} {@var{mf} =} maxflow (@var{G}, @var{s}, @var{t})
## @deftypefnx {} {@var{mf} =} maxflow (@var{G}, @var{s}, @var{t}, @var{algorithm})
## Return the maximum flow value @var{mf} that can be sent from node
## @var{s} to node @var{t} in the graph or digraph @var{G}.
##
## @var{G} must be a @code{graph} or @code{digraph} object.  @var{s}
## and @var{t} are scalar node identifiers: a positive integer node
## index, a character row vector naming a node, or a 1-element cell
## array of strings naming a node.  When @var{G} has node names, a
## mix of numeric and name identifiers is allowed.
##
## Edge weights are interpreted as capacities and must be
## non-negative; @code{NaN} or negative weights raise an error.  When
## @var{G} is unweighted every edge has capacity @code{1}.  For a
## @code{digraph} that is a multigraph, the capacities of parallel
## edges are summed.  For a @code{graph} each undirected edge acts as
## a pair of antiparallel arcs with the stored capacity available in
## either direction.  Self-loops do not contribute to any @math{s-t}
## flow.
##
## @var{mf} is a scalar non-negative real number.  When @code{@var{s}
## == @var{t}} or when @var{t} is not reachable from @var{s} along
## edges with positive capacity, @var{mf} is @code{0}.
##
## The optional @var{algorithm} argument selects the solver
## (case-insensitive):
## @table @asis
## @item @qcode{"augmentpath"} (default)
## The Edmonds-Karp implementation of Ford-Fulkerson, which augments
## along a shortest (fewest-edge) residual path at each iteration.
## @item @qcode{"searchtrees"}
## A dual-search-tree augmenting-path method inspired by the
## Boykov-Kolmogorov algorithm, growing one BFS tree from @var{s} and
## a second BFS tree backward from @var{t}, then augmenting along the
## shortest joining path at every iteration.
## @end table
## Both algorithms return the same maximum flow value; only the
## execution path through the residual graph differs.
##
## @example
## @group
## ## 4-node diamond digraph (capacities on edges).
## G = digraph ([1 1 2 3], [2 3 4 4], [5 8 7 3]);
## maxflow (G, 1, 4)
##          @result{} 8
##
## ## Classical CLRS Figure 26.1 network.  mf = 23.
## s = [1 1 2 2 3 3 4 4 5 5];
## t = [2 3 3 4 2 5 3 6 4 6];
## w = [16 13 10 12 4 14 9 20 7 4];
## G = digraph (s, t, w);
## maxflow (G, 1, 6)
##          @result{} 23
## @end group
## @end example
##
## @seealso{graph, digraph, shortestpath, distances}
## @end deftypefn

function mf = maxflow (G, varargin)

  ## NOTE: When called with a graph or digraph object, Octave's
  ## classdef method dispatch runs the class-internal @code{maxflow}
  ## method and this free-function body is not reached.  This file
  ## exists both as a canonical documentation target (so @code{help
  ## maxflow} works outside the context of an instance) and as a
  ## fallback that gives a helpful error for non-graph inputs.

  if (nargin < 1)
    print_usage ();
  endif

  if (! (isa (G, "graph") || isa (G, "digraph")))
    error ("Octave:invalid-input-arg", ...
           "maxflow: G must be a graph or digraph object");
  endif

  ## Defensive delegation: classdef method dispatch should intercept
  ## any call with a graph/digraph first arg, but route through dot
  ## notation just in case.
  mf = G.maxflow (varargin{:});

endfunction


## ------------------------------------------------------------------
## BIST blocks
## ------------------------------------------------------------------

## -------------------- basic error cases --------------------

## maxflow on a non-graph numeric input is an error.
%!error <must be a graph or digraph object>
%! maxflow (42, 1, 2);

## maxflow on a non-graph string input is an error.
%!error <must be a graph or digraph object>
%! maxflow ("foo", 1, 2);

## maxflow with no args is an error via print_usage.
%!error maxflow ()

## maxflow with G alone is an error (missing s and t).
%!error maxflow (digraph ())

## maxflow with G and s only is an error (missing t).
%!error maxflow (digraph (3), 1)

## -------------------- trivial cases --------------------

## Source == target on a single-node digraph returns 0.
%!test
%! G = digraph (1);
%! assert (maxflow (G, 1, 1), 0);

## Source == target on a single-node graph returns 0.
%!test
%! G = graph (1);
%! assert (maxflow (G, 1, 1), 0);

## Source == target on a multi-node digraph with edges returns 0.
%!test
%! G = digraph ([1 2 3], [2 3 1], [5 10 15]);
%! assert (maxflow (G, 2, 2), 0);

## -------------------- unreachable --------------------

## Edgeless 3-node digraph: maxflow from 1 to 2 is 0.
%!test
%! G = digraph (3);
%! assert (maxflow (G, 1, 2), 0);

## Edgeless 3-node graph: maxflow from 1 to 2 is 0.
%!test
%! G = graph (3);
%! assert (maxflow (G, 1, 2), 0);

## Two disjoint components (digraph): cross-component maxflow = 0.
%!test
%! G = digraph ([1 3], [2 4]);
%! assert (maxflow (G, 1, 3), 0);
%! assert (maxflow (G, 1, 4), 0);

## Two disjoint components (graph): cross-component maxflow = 0.
%!test
%! G = graph ([1 3], [2 4]);
%! assert (maxflow (G, 1, 3), 0);

## Single directed edge 1->2: reverse direction 2->1 maxflow = 0.
%!test
%! G = digraph (1, 2);
%! assert (maxflow (G, 2, 1), 0);

## -------------------- unweighted digraph --------------------

## Single directed edge 1->2 unweighted: maxflow = 1.
%!test
%! G = digraph (1, 2);
%! assert (maxflow (G, 1, 2), 1);

## 3-node chain 1->2->3 unweighted: maxflow 1->3 = 1.
%!test
%! G = digraph ([1 2], [2 3]);
%! assert (maxflow (G, 1, 3), 1);

## 3-cycle 1->2->3->1 unweighted: maxflow 1->3 = 1 (via 1->2->3).
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! assert (maxflow (G, 1, 3), 1);

## Two parallel paths 1->2->4 and 1->3->4 unweighted: maxflow = 2.
%!test
%! G = digraph ([1 1 2 3], [2 3 4 4]);
%! assert (maxflow (G, 1, 4), 2);

## -------------------- weighted digraph --------------------

## Single weighted edge 1->2 cap 5: maxflow = 5.
%!test
%! G = digraph (1, 2, 5);
%! assert (maxflow (G, 1, 2), 5);

## Chain 1->2(5), 2->3(10): maxflow from 1 to 3 = 5 (bottleneck).
%!test
%! G = digraph ([1 2], [2 3], [5 10]);
%! assert (maxflow (G, 1, 3), 5);

## Chain 1->2(10), 2->3(5): maxflow from 1 to 3 = 5.
%!test
%! G = digraph ([1 2], [2 3], [10 5]);
%! assert (maxflow (G, 1, 3), 5);

## Diamond 1->2(5), 1->3(8), 2->4(7), 3->4(3): maxflow 1->4 = 8.
## Path 1->2->4 carries min(5,7)=5; path 1->3->4 carries min(8,3)=3;
## total = 5 + 3 = 8.
%!test
%! G = digraph ([1 1 2 3], [2 3 4 4], [5 8 7 3]);
%! assert (maxflow (G, 1, 4), 8);

## Diamond 1->2(5), 1->3(5), 2->4(5), 3->4(5): maxflow 1->4 = 10.
%!test
%! G = digraph ([1 1 2 3], [2 3 4 4], [5 5 5 5]);
%! assert (maxflow (G, 1, 4), 10);

## Weighted graph with a bottleneck: s=1, t=4. Edges 1->2(100),
## 2->3(1), 3->4(100).  The 2->3 link is the bottleneck; mf = 1.
%!test
%! G = digraph ([1 2 3], [2 3 4], [100 1 100]);
%! assert (maxflow (G, 1, 4), 1);

## CLRS Figure 26.1 (Introduction to Algorithms, 3rd ed.): 6-node
## network s=1, v1=2, v2=3, v3=4, v4=5, t=6, with edges 1->2(16),
## 1->3(13), 2->3(10), 3->2(4), 2->4(12), 4->3(9), 3->5(14), 5->4(7),
## 4->6(20), 5->6(4).  The textbook maximum flow is 23.
%!test
%! s = [1 1 2 3 2 4 3 5 4 5];
%! t = [2 3 3 2 4 3 5 4 6 6];
%! w = [16 13 10 4 12 9 14 7 20 4];
%! G = digraph (s, t, w);
%! assert (maxflow (G, 1, 6), 23);

## -------------------- undirected graph --------------------

## Single undirected edge {1,2} cap 5: maxflow = 5 in either
## direction (graph is symmetric).
%!test
%! G = graph (1, 2, 5);
%! assert (maxflow (G, 1, 2), 5);
%! assert (maxflow (G, 2, 1), 5);

## Chain 1--2--3 with caps 5, 10: maxflow 1->3 = 5 (bottleneck).
%!test
%! G = graph ([1 2], [2 3], [5 10]);
%! assert (maxflow (G, 1, 3), 5);
%! assert (maxflow (G, 3, 1), 5);

## Undirected triangle, uniform cap 5: maxflow 1->2 = 10
## (direct edge cap 5 plus indirect 1--3--2 bottlenecked at min(5,5) = 5).
%!test
%! G = graph ([1 2 3], [2 3 1], [5 5 5]);
%! assert (maxflow (G, 1, 2), 10);

## Undirected diamond 1--2(5), 1--3(8), 2--4(7), 3--4(3): maxflow
## 1->4 = 5+3 = 8 (same as directed diamond with the same caps).
%!test
%! G = graph ([1 1 2 3], [2 3 4 4], [5 8 7 3]);
%! assert (maxflow (G, 1, 4), 8);

## Unweighted undirected graph: cap 1 per edge.
%!test
%! G = graph ([1 1 2 3], [2 3 4 4]);
%! assert (maxflow (G, 1, 4), 2);

## -------------------- self-loops ignored --------------------

## Self-loop on source node does not contribute to flow (digraph).
%!test
%! G = digraph ([1 1], [1 2], [100, 5]);
%! assert (maxflow (G, 1, 2), 5);

## Self-loop on source node does not contribute to flow (graph).
%!test
%! G = graph ([1 1], [1 2], [100, 5]);
%! assert (maxflow (G, 1, 2), 5);

## Self-loop on target node does not contribute.
%!test
%! G = digraph ([1 2], [2 2], [5, 100]);
%! assert (maxflow (G, 1, 2), 5);

## Self-loop on an intermediate node does not contribute.
%!test
%! G = digraph ([1 2 2], [2 2 3], [5, 100, 3]);
%! assert (maxflow (G, 1, 3), 3);

## -------------------- multigraph (digraph) --------------------

## Parallel edges sum capacities.  Two parallel 1->2 edges with caps
## 3 and 7 give maxflow = 10.
%!test
%! G = digraph ([1 1], [2 2], [3 7], "multigraph");
%! assert (maxflow (G, 1, 2), 10);

## Three parallel 1->2 edges: caps 3, 5, 7 -> maxflow = 15.
%!test
%! G = digraph ([1 1 1], [2 2 2], [3 5 7], "multigraph");
%! assert (maxflow (G, 1, 2), 15);

## Parallel edges on a chain: 1->2 has caps 2, 3; 2->3 has caps 4, 5.
## First segment total cap = 5; second segment = 9.  Bottleneck = 5.
%!test
%! G = digraph ([1 1 2 2], [2 2 3 3], [2 3 4 5], "multigraph");
%! assert (maxflow (G, 1, 3), 5);

## Unweighted multigraph: each parallel edge has cap 1.  Two parallel
## 1->2 edges: maxflow = 2.
%!test
%! G = digraph ([1 1], [2 2], "multigraph");
%! assert (maxflow (G, 1, 2), 2);

## -------------------- named nodes --------------------

## Numeric s and t on a named digraph.
%!test
%! G = digraph ([1 2 3], [2 3 1], [5 10 15], {"a", "b", "c"});
%! assert (maxflow (G, 1, 3), 5);

## String s on a named digraph.
%!test
%! G = digraph ([1 1 2 3], [2 3 4 4], [5 8 7 3], {"a","b","c","d"});
%! assert (maxflow (G, "a", "d"), 8);

## Cellstr s and t.
%!test
%! G = digraph ([1 1 2 3], [2 3 4 4], [5 8 7 3], {"a","b","c","d"});
%! assert (maxflow (G, {"a"}, {"d"}), 8);

## Mixed numeric and string on named graph.
%!test
%! G = graph ([1 2 3], [2 3 1], [5 5 5], {"a", "b", "c"});
%! assert (maxflow (G, "a", 2), 10);

## -------------------- named-nodes error cases -------------------

## String src on a digraph without node names errors.
%!error <no node names>
%! G = digraph (3);
%! maxflow (G, "a", 2);

## Missing node name on src errors.
%!error <not found>
%! G = digraph ([1 2], [2 3], [], {"a", "b", "c"});
%! maxflow (G, "z", "a");

## Missing node name on tgt errors.
%!error <not found>
%! G = digraph ([1 2], [2 3], [], {"a", "b", "c"});
%! maxflow (G, "a", "z");

## -------------------- numeric-index validation -----------------

## Out-of-range numeric src errors.
%!error <invalid node index>
%! G = digraph (3);
%! maxflow (G, 5, 1);

## Zero numeric src errors.
%!error <invalid node index>
%! G = digraph (3);
%! maxflow (G, 0, 1);

## Non-integer numeric src errors.
%!error <invalid node index>
%! G = digraph (3);
%! maxflow (G, 1.5, 1);

## Out-of-range numeric tgt errors.
%!error <invalid node index>
%! G = digraph (3);
%! maxflow (G, 1, 5);

## Non-scalar numeric src errors (vector not allowed for s).
%!error <scalar node identifier>
%! G = digraph (3);
%! maxflow (G, [1 2], 3);

## -------------------- capacity validation -------------------

## Negative weight errors.
%!error <negative|non-negative>
%! G = digraph ([1 2], [2 3], [5, -1]);
%! maxflow (G, 1, 3);

## Negative weight on undirected graph errors.
%!error <negative|non-negative>
%! G = graph ([1 2], [2 3], [5, -1]);
%! maxflow (G, 1, 3);

## NaN weight errors.
%!error <NaN|finite>
%! G = digraph ([1 2], [2 3], [5, NaN]);
%! maxflow (G, 1, 3);

## -------------------- dot notation dispatch --------------------

## G.maxflow(s, t) matches maxflow(G, s, t) for digraph.
%!test
%! G = digraph ([1 1 2 3], [2 3 4 4], [5 8 7 3]);
%! assert (maxflow (G, 1, 4), G.maxflow (1, 4));

## G.maxflow(s, t) matches maxflow(G, s, t) for graph.
%!test
%! G = graph ([1 1 2 3], [2 3 4 4], [5 8 7 3]);
%! assert (maxflow (G, 1, 4), G.maxflow (1, 4));

## -------------------- output shape --------------------

## mf is a scalar double.
%!test
%! G = digraph (1, 2, 5);
%! mf = maxflow (G, 1, 2);
%! assert (isscalar (mf));
%! assert (isa (mf, "double"));
%! assert (mf, 5);

## -------------------- larger graph sanity ----------------------

## Chain 1->2->...->10 unweighted: maxflow(1, 10) = 1 (single path,
## unit cap bottleneck).
%!test
%! G = digraph (1:9, 2:10);
%! assert (maxflow (G, 1, 10), 1);

## Complete-bipartite-like: s=1, sinks 2..5, all caps = 1.  Edges
## 1->2, 1->3, 1->4, 1->5 and 2->6, 3->6, 4->6, 5->6.  t=6.  mf = 4.
%!test
%! s = [1 1 1 1 2 3 4 5];
%! t = [2 3 4 5 6 6 6 6];
%! G = digraph (s, t);
%! assert (maxflow (G, 1, 6), 4);

## A larger weighted network: hybrid of CLRS Fig 26.1 and an extra
## 1->4 shortcut.  Adding 1->4 capacity 100 raises the bottleneck
## cut to 13 + 100 + 16 = 29; the 4 bottleneck edges now include
## 1->4(100), 3->4(9), 3->5(14) reverse, 2->5(indirect) -- the
## min s-t cut is {s} vs rest with cap 16+13+100 = 129, but we are
## still limited by the 4->6(20) + 5->6(4) = 24 out-of-sink cut.
## The actual mf on this graph is 24.
%!test
%! s = [1 1 1 2 3 2 4 3 5 4 5];
%! t = [2 3 4 3 2 4 3 5 4 6 6];
%! w = [16 13 100 10 4 12 9 14 7 20 4];
%! G = digraph (s, t, w);
%! assert (maxflow (G, 1, 6), 24);

## -------------------- Method / algorithm option --------------------

## Default algorithm matches explicit 'augmentpath' on a diamond.
%!test
%! G = digraph ([1 1 2 3], [2 3 4 4], [5 8 7 3]);
%! mf_default = maxflow (G, 1, 4);
%! mf_augment = maxflow (G, 1, 4, "augmentpath");
%! assert (mf_default, mf_augment);

## 'augmentpath' positional arg returns 23 on CLRS Fig 26.1.
%!test
%! s = [1 1 2 3 2 4 3 5 4 5];
%! t = [2 3 3 2 4 3 5 4 6 6];
%! w = [16 13 10 4 12 9 14 7 20 4];
%! G = digraph (s, t, w);
%! assert (maxflow (G, 1, 6, "augmentpath"), 23);

## 'searchtrees' positional arg returns 23 on CLRS Fig 26.1.
%!test
%! s = [1 1 2 3 2 4 3 5 4 5];
%! t = [2 3 3 2 4 3 5 4 6 6];
%! w = [16 13 10 4 12 9 14 7 20 4];
%! G = digraph (s, t, w);
%! assert (maxflow (G, 1, 6, "searchtrees"), 23);

## 'searchtrees' on diamond digraph.
%!test
%! G = digraph ([1 1 2 3], [2 3 4 4], [5 8 7 3]);
%! assert (maxflow (G, 1, 4, "searchtrees"), 8);

## 'searchtrees' on diamond undirected graph.
%!test
%! G = graph ([1 1 2 3], [2 3 4 4], [5 8 7 3]);
%! assert (maxflow (G, 1, 4, "searchtrees"), 8);

## 'searchtrees' on undirected triangle uniform caps.
%!test
%! G = graph ([1 2 3], [2 3 1], [5 5 5]);
%! assert (maxflow (G, 1, 2, "searchtrees"), 10);

## 'searchtrees' on undirected chain bottleneck.
%!test
%! G = graph ([1 2], [2 3], [5 10]);
%! assert (maxflow (G, 1, 3, "searchtrees"), 5);

## 'searchtrees' on multigraph sums parallel edges.
%!test
%! G = digraph ([1 1], [2 2], [3 7], "multigraph");
%! assert (maxflow (G, 1, 2, "searchtrees"), 10);

## 'searchtrees' on multigraph chain bottleneck.
%!test
%! G = digraph ([1 1 2 2], [2 2 3 3], [2 3 4 5], "multigraph");
%! assert (maxflow (G, 1, 3, "searchtrees"), 5);

## 'searchtrees' ignores self-loops.
%!test
%! G = digraph ([1 1 2], [1 2 2], [100 5 50]);
%! assert (maxflow (G, 1, 2, "searchtrees"), 5);

## 'searchtrees' on unweighted digraph uses unit capacities.
%!test
%! G = digraph ([1 1 2 3], [2 3 4 4]);
%! assert (maxflow (G, 1, 4, "searchtrees"), 2);

## 'searchtrees' on trivial s == t returns 0.
%!test
%! G = digraph ([1 2 3], [2 3 1], [5 10 15]);
%! assert (maxflow (G, 2, 2, "searchtrees"), 0);

## 'searchtrees' on unreachable target returns 0.
%!test
%! G = digraph ([1 3], [2 4]);
%! assert (maxflow (G, 1, 3, "searchtrees"), 0);

## 'searchtrees' on disconnected graph returns 0.
%!test
%! G = graph ([1 3], [2 4]);
%! assert (maxflow (G, 1, 3, "searchtrees"), 0);

## 'searchtrees' on edgeless graph returns 0.
%!test
%! G = digraph (5);
%! assert (maxflow (G, 1, 5, "searchtrees"), 0);

## 'searchtrees' on single edge returns weight.
%!test
%! G = digraph (1, 2, 42);
%! assert (maxflow (G, 1, 2, "searchtrees"), 42);

## 'searchtrees' reverse direction on directed edge returns 0.
%!test
%! G = digraph (1, 2, 5);
%! assert (maxflow (G, 2, 1, "searchtrees"), 0);

## Case-insensitive algorithm name (digraph).
%!test
%! G = digraph ([1 1 2 3], [2 3 4 4], [5 8 7 3]);
%! assert (maxflow (G, 1, 4, "AUGMENTPATH"), 8);
%! assert (maxflow (G, 1, 4, "SearchTrees"), 8);
%! assert (maxflow (G, 1, 4, "searchTREES"), 8);

## Case-insensitive algorithm name (graph).
%!test
%! G = graph ([1 1 2 3], [2 3 4 4], [5 8 7 3]);
%! assert (maxflow (G, 1, 4, "AUGMENTPATH"), 8);
%! assert (maxflow (G, 1, 4, "SEARCHTREES"), 8);

## 'augmentpath' and 'searchtrees' return the same mf on a varied digraph.
%!test
%! s = [1 1 2 2 3 3 4 4 5 5 6 6 7];
%! t = [2 3 4 5 5 6 6 7 6 8 7 8 8];
%! w = [10 5 8 4 3 6 2 9 1 7 5 3 6];
%! G = digraph (s, t, w);
%! mf_ap = maxflow (G, 1, 8, "augmentpath");
%! mf_st = maxflow (G, 1, 8, "searchtrees");
%! assert (mf_ap, mf_st);

## 'augmentpath' and 'searchtrees' agree on a varied undirected graph.
%!test
%! s = [1 1 1 2 2 3 3 4 5 6];
%! t = [2 3 4 5 6 6 7 7 7 7];
%! w = [5 10 3 2 8 4 6 7 9 1];
%! G = graph (s, t, w);
%! mf_ap = maxflow (G, 1, 7, "augmentpath");
%! mf_st = maxflow (G, 1, 7, "searchtrees");
%! assert (mf_ap, mf_st);

## 'augmentpath' and 'searchtrees' agree on unweighted CLRS-like graph.
%!test
%! s = [1 1 2 3 2 4 3 5 4 5];
%! t = [2 3 3 2 4 3 5 4 6 6];
%! G = digraph (s, t);
%! mf_ap = maxflow (G, 1, 6, "augmentpath");
%! mf_st = maxflow (G, 1, 6, "searchtrees");
%! assert (mf_ap, mf_st);

## 'augmentpath' and 'searchtrees' agree on multigraph.
%!test
%! G = digraph ([1 1 1 2 2 3 3 4], [2 2 3 3 4 4 5 5], ...
%!              [4 2 5 3 1 4 2 6], "multigraph");
%! mf_ap = maxflow (G, 1, 5, "augmentpath");
%! mf_st = maxflow (G, 1, 5, "searchtrees");
%! assert (mf_ap, mf_st);

## Negative weights still error with algorithm option (digraph).
%!error <negative|non-negative>
%! G = digraph ([1 2], [2 3], [5, -1]);
%! maxflow (G, 1, 3, "searchtrees");

## Negative weights still error with algorithm option (graph).
%!error <negative|non-negative>
%! G = graph ([1 2], [2 3], [5, -1]);
%! maxflow (G, 1, 3, "searchtrees");

## NaN weights still error with algorithm option.
%!error <NaN|finite>
%! G = digraph ([1 2], [2 3], [5, NaN]);
%! maxflow (G, 1, 3, "searchtrees");

## Unknown algorithm name errors (digraph).
%!error <algorithm|unknown|invalid>
%! G = digraph (1, 2, 5);
%! maxflow (G, 1, 2, "bogus");

## Unknown algorithm name errors (graph).
%!error <algorithm|unknown|invalid>
%! G = graph (1, 2, 5);
%! maxflow (G, 1, 2, "bogus");

## Non-string algorithm arg errors.
%!error <algorithm|string|invalid>
%! G = digraph (1, 2, 5);
%! maxflow (G, 1, 2, 42);

## Empty-string algorithm arg errors.
%!error <algorithm|string|invalid|non-empty>
%! G = digraph (1, 2, 5);
%! maxflow (G, 1, 2, "");

## Cellstr algorithm arg errors (MATLAB requires a plain char row).
%!error <algorithm|string|invalid>
%! G = digraph (1, 2, 5);
%! maxflow (G, 1, 2, {"augmentpath"});

## 'pushrelabel' (MATLAB has it, we don't) errors.
%!error <algorithm|unknown|invalid>
%! G = digraph (1, 2, 5);
%! maxflow (G, 1, 2, "pushrelabel");

## Dot-notation dispatch with algorithm on digraph.
%!test
%! G = digraph ([1 1 2 3], [2 3 4 4], [5 8 7 3]);
%! assert (G.maxflow (1, 4, "searchtrees"), 8);
%! assert (G.maxflow (1, 4, "augmentpath"), 8);

## Dot-notation dispatch with algorithm on graph.
%!test
%! G = graph ([1 1 2 3], [2 3 4 4], [5 8 7 3]);
%! assert (G.maxflow (1, 4, "searchtrees"), 8);
%! assert (G.maxflow (1, 4, "augmentpath"), 8);

## Named-node identifiers work with algorithm.
%!test
%! G = digraph ([1 1 2 3], [2 3 4 4], [5 8 7 3], {"a","b","c","d"});
%! assert (maxflow (G, "a", "d", "searchtrees"), 8);
%! assert (maxflow (G, "a", "d", "augmentpath"), 8);

## Named-node identifiers work with algorithm on graph.
%!test
%! G = graph ([1 2 3], [2 3 1], [5 5 5], {"a", "b", "c"});
%! assert (maxflow (G, "a", 2, "searchtrees"), 10);
%! assert (maxflow (G, "a", 2, "augmentpath"), 10);
