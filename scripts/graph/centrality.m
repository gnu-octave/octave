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
## @deftypefn {} {@var{c} =} centrality (@var{G}, @var{type})
## Return the centrality of each node in the graph or digraph @var{G}.
##
## @var{G} must be a @code{graph} or @code{digraph} object.  @var{type}
## is a character row vector (case-insensitive) selecting which
## centrality measure to compute.
##
## The following centrality measures are available:
##
## @table @code
## @item "degree"
## For an undirected @code{graph}, the number of edges incident to each
## node.  A self-loop contributes 2 to the degree of the looped node
## (MATLAB convention).  @code{"degree"} is not defined for a
## @code{digraph}; use @code{"indegree"} or @code{"outdegree"} there.
##
## @item "indegree"
## For a @code{digraph}, the number of edges ending at each node.
## Each self-loop contributes 1.  @code{"indegree"} is not defined for
## an undirected @code{graph}.
##
## @item "outdegree"
## For a @code{digraph}, the number of edges starting at each node.
## Each self-loop contributes 1.  @code{"outdegree"} is not defined for
## an undirected @code{graph}.
##
## @item "closeness"
## Closeness centrality, computed as
## @math{(N-1) / sum_{j != i} d(i, j)} where @math{d(i, j)} is the
## shortest-path distance from node @math{i} to node @math{j}.
## Unreachable pairs contribute @code{Inf} to the denominator, so
## nodes in an isolated component receive centrality zero.  On a
## @code{digraph} this value uses outgoing distances (it is an alias
## for @code{"outcloseness"}); on an undirected @code{graph} the
## distance matrix is symmetric so the choice does not matter.
##
## @item "incloseness"
## For a @code{digraph}, closeness centrality using incoming
## distances, @math{(N-1) / sum_{j != i} d(j, i)}.  Not defined for an
## undirected @code{graph}.
##
## @item "outcloseness"
## For a @code{digraph}, closeness centrality using outgoing
## distances, @math{(N-1) / sum_{j != i} d(i, j)}.  Not defined for an
## undirected @code{graph}.
## @end table
##
## The return value @var{c} is always a column vector of length
## @code{numnodes (@var{G})} with class @code{double}.
##
## Degree-based variants count parallel edges in a multigraph
## individually so the centrality reflects the true edge count, not
## the number of distinct neighbours.  Degree-based variants ignore
## edge weights; closeness variants use the stored edge weights via
## @code{distances (@var{G})} (BFS on unweighted graphs, Dijkstra
## otherwise).  The @code{"Cost"} and @code{"Importance"} name-value
## options that MATLAB supports for weighted centralities are not yet
## implemented.
##
## @example
## @group
## G = graph ([1 1 2 3], [2 3 3 4]);
## centrality (G, "degree")      # @result{}  2
##                               ##           2
##                               ##           3
##                               ##           1
## centrality (G, "closeness")   # @result{}  3/4
##                               ##           3/4
##                               ##           1
##                               ##           3/5
##
## D = digraph ([1 2 3 1], [2 3 1 3]);
## centrality (D, "indegree")    # @result{}  1
##                               ##           1
##                               ##           2
## centrality (D, "outdegree")   # @result{}  2
##                               ##           1
##                               ##           1
## centrality (D, "outcloseness")
## centrality (D, "incloseness")
## @end group
## @end example
##
## @seealso{graph, digraph, degree, indegree, outdegree, distances}
## @end deftypefn

function c = centrality (G, type, varargin)

  ## NOTE: When called with a graph or digraph object, Octave's
  ## classdef method dispatch runs the class-internal @code{centrality}
  ## method and this free-function body is not reached.  This file
  ## exists both as a canonical documentation target (so @code{help
  ## centrality} works outside the context of an instance) and as a
  ## fallback that gives a helpful error for non-graph inputs.

  if (nargin < 2)
    print_usage ();
  endif

  if (! (isa (G, "graph") || isa (G, "digraph")))
    error ("Octave:invalid-input-arg", ...
           "centrality: G must be a graph or digraph object");
  endif

  ## Defensive delegation: classdef method dispatch should intercept
  ## any call with a graph/digraph first arg, but route through dot
  ## notation just in case.
  c = G.centrality (type, varargin{:});

endfunction


## ------------------------------------------------------------------
## BIST blocks
## ------------------------------------------------------------------

## -------------------- basic error cases --------------------

## centrality on a non-graph numeric input is an error.
%!error <must be a graph or digraph object>
%! centrality (42, "degree");

## centrality on a non-graph string input is an error.
%!error <must be a graph or digraph object>
%! centrality ("foo", "degree");

## centrality with no args is an error via print_usage.
%!error centrality ()

## centrality with G alone is an error (missing type).
%!error centrality (graph ())
%!error centrality (digraph ())

## -------------------- graph + 'degree' -------------------------

## Simple graph: centrality('degree') matches degree().
%!test
%! G = graph ([1 1 2 3], [2 3 3 4]);
%! c = centrality (G, "degree");
%! assert (c, [2; 2; 3; 1]);

## Triangle: every node has degree 2.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! assert (centrality (G, "degree"), [2; 2; 2]);

## Empty graph returns a 0-by-1 column.
%!test
%! G = graph ();
%! c = centrality (G, "degree");
%! assert (c, zeros (0, 1));

## Edgeless graph returns zeros(N, 1).
%!test
%! G = graph (5);
%! c = centrality (G, "degree");
%! assert (c, zeros (5, 1));

## Self-loop contributes 2 to the degree (MATLAB convention).
%!test
%! G = graph ([1 2 3], [1 3 3]);
%! assert (centrality (G, "degree"), [2; 1; 3]);

## Star graph K_{1,5}: centre has degree 5, leaves each degree 1.
%!test
%! G = graph (ones (1, 5), 2:6);
%! assert (centrality (G, "degree"), [5; 1; 1; 1; 1; 1]);

## Complete graph K4: every node has degree 3.
%!test
%! G = graph ([1 1 1 2 2 3], [2 3 4 3 4 4]);
%! assert (centrality (G, "degree"), [3; 3; 3; 3]);

## Path graph: endpoints degree 1, interior degree 2.
%!test
%! G = graph ([1 2 3 4], [2 3 4 5]);
%! assert (centrality (G, "degree"), [1; 2; 2; 2; 1]);

## Weighted graph: weights ignored, edge counts only.
%!test
%! G = graph ([1 2 3], [2 3 1], [0.5 0.25 0.75]);
%! assert (centrality (G, "degree"), [2; 2; 2]);

## Weighted graph: negative weights are also ignored (edge counts only).
%!test
%! G = graph ([1 2 3], [2 3 1], [-1 -2 -3]);
%! assert (centrality (G, "degree"), [2; 2; 2]);

## Result is a column vector of class double.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! c = centrality (G, "degree");
%! assert (size (c), [3, 1]);
%! assert (class (c), "double");

## Named graph: centrality indexed by node order.
%!test
%! G = graph ([1 1 2], [2 3 3], [], {"alpha", "beta", "gamma"});
%! assert (centrality (G, "degree"), [2; 2; 2]);

## Adjacency-matrix constructor round-trip.
%!test
%! A = [0 1 1; 1 0 1; 1 1 0];
%! G = graph (A);
%! assert (centrality (G, "degree"), [2; 2; 2]);

## Isolated trailing nodes (N form).
%!test
%! G = graph ([1 2], [2 3], [], 5);
%! assert (centrality (G, "degree"), [1; 2; 1; 0; 0]);

## -------------------- graph: invalid type -----------------------

## 'indegree' is not defined for an undirected graph.
%!error <indegree.*not defined|only defined for|digraph>
%! centrality (graph ([1 2], [2 3]), "indegree");

## 'outdegree' is not defined for an undirected graph.
%!error <outdegree.*not defined|only defined for|digraph>
%! centrality (graph ([1 2], [2 3]), "outdegree");

## -------------------- digraph + 'indegree' ----------------------

## Simple digraph: centrality('indegree') matches indegree().
%!test
%! G = digraph ([1 2 3 1], [2 3 1 3]);
%! assert (centrality (G, "indegree"), [1; 1; 2]);

## Empty digraph returns a 0-by-1 column.
%!test
%! G = digraph ();
%! c = centrality (G, "indegree");
%! assert (c, zeros (0, 1));

## Edgeless digraph returns zeros(N, 1).
%!test
%! G = digraph (4);
%! c = centrality (G, "indegree");
%! assert (c, zeros (4, 1));

## Digraph with self-loop: indegree for looped node is 1.
%!test
%! G = digraph ([1 2 3], [1 3 2]);
%! assert (centrality (G, "indegree"), [1; 1; 1]);

## Multigraph: parallel edges each count individually.
%!test
%! G = digraph ([1 1 1 2], [2 2 2 3], "multigraph");
%! assert (centrality (G, "indegree"), [0; 3; 1]);

## Weighted digraph: weights ignored.
%!test
%! G = digraph ([1 2 3], [2 3 1], [0.5 0.25 0.75]);
%! assert (centrality (G, "indegree"), [1; 1; 1]);

## Result is column double.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! c = centrality (G, "indegree");
%! assert (size (c), [3, 1]);
%! assert (class (c), "double");

## Named digraph: works by name ordering.
%!test
%! G = digraph ([1 1 2], [2 3 3], [], {"alpha", "beta", "gamma"});
%! assert (centrality (G, "indegree"), [0; 1; 2]);

## -------------------- digraph + 'outdegree' ---------------------

## Simple digraph: centrality('outdegree') matches outdegree().
%!test
%! G = digraph ([1 2 3 1], [2 3 1 3]);
%! assert (centrality (G, "outdegree"), [2; 1; 1]);

## Edgeless digraph outdegree is zeros.
%!test
%! G = digraph (3);
%! assert (centrality (G, "outdegree"), zeros (3, 1));

## Self-loop contributes 1 to outdegree.
%!test
%! G = digraph ([1 2 3], [1 3 2]);
%! assert (centrality (G, "outdegree"), [1; 1; 1]);

## Multigraph: parallel out-edges each counted.
%!test
%! G = digraph ([1 1 1 2], [2 2 2 3], "multigraph");
%! assert (centrality (G, "outdegree"), [3; 1; 0]);

## Named digraph outdegree.
%!test
%! G = digraph ([1 1 2], [2 3 3], [], {"alpha", "beta", "gamma"});
%! assert (centrality (G, "outdegree"), [2; 1; 0]);

## Indegree + outdegree sum = degree sum of all edges (when no
## self-loops).  This is a loose consistency check.
%!test
%! G = digraph ([1 2 3 1 2], [2 3 1 3 1]);
%! assert (sum (centrality (G, "indegree")), numedges (G));
%! assert (sum (centrality (G, "outdegree")), numedges (G));

## -------------------- digraph: invalid type ---------------------

## 'degree' is not defined for a directed graph.
%!error <degree.*not defined|only defined for|graph>
%! centrality (digraph ([1 2], [2 3]), "degree");

## -------------------- case-insensitivity ------------------------

## 'Degree', 'DEGREE', 'dEgReE' all match 'degree'.
%!test
%! G = graph ([1 2], [2 3]);
%! assert (centrality (G, "Degree"), [1; 2; 1]);
%! assert (centrality (G, "DEGREE"), [1; 2; 1]);
%! assert (centrality (G, "dEgReE"), [1; 2; 1]);

## 'InDegree' / 'INDEGREE' work too.
%!test
%! G = digraph ([1 2 3 1], [2 3 1 3]);
%! assert (centrality (G, "InDegree"), [1; 1; 2]);
%! assert (centrality (G, "INDEGREE"), [1; 1; 2]);

## 'OutDegree' / 'OUTDEGREE' work too.
%!test
%! G = digraph ([1 2 3 1], [2 3 1 3]);
%! assert (centrality (G, "OutDegree"), [2; 1; 1]);
%! assert (centrality (G, "OUTDEGREE"), [2; 1; 1]);

## -------------------- unknown type errors -----------------------

## Unknown type name is an error.
%!error <unknown|invalid|unrecognized>
%! centrality (graph ([1 2], [2 3]), "nonsense");

## Unknown type on digraph is an error.
%!error <unknown|invalid|unrecognized>
%! centrality (digraph ([1 2], [2 3]), "nonsense");

## Misspelled common types error with a helpful message.
%!error <unknown|invalid|unrecognized>
%! centrality (graph ([1 2], [2 3]), "dergree");

## -------------------- type argument validation -------------------

## Non-string type is an error.
%!error <type.*char|must be a.*string|must be.*char>
%! centrality (graph ([1 2], [2 3]), 42);

## Empty string type is an error.
%!error <type.*empty|must not be empty|unknown|invalid|unrecognized>
%! centrality (graph ([1 2], [2 3]), "");

## -------------------- dot-notation dispatch ---------------------

## Dot-notation dispatch for graph.
%!test
%! G = graph ([1 1 2 3], [2 3 3 4]);
%! assert (G.centrality ("degree"), [2; 2; 3; 1]);

## Dot-notation dispatch for digraph.
%!test
%! G = digraph ([1 2 3 1], [2 3 1 3]);
%! assert (G.centrality ("indegree"), [1; 1; 2]);
%! assert (G.centrality ("outdegree"), [2; 1; 1]);

## -------------------- siever fixture ----------------------------

## Siever 9-node digraph in/out-degrees via centrality.
%!test
%! s = [1 2 3 3 4 5 5 6 7 7 8 9];
%! t = [2 3 2 4 5 6 9 7 8 9 7 4];
%! G = digraph (s, t);
%! assert (centrality (G, "indegree"), [0; 2; 1; 2; 1; 1; 2; 1; 2]);
%! ## outdegree: node 1: 1 (-> 2), 2: 1, 3: 2, 4: 1, 5: 2, 6: 1,
%! ##           7: 2, 8: 1, 9: 1.
%! assert (centrality (G, "outdegree"), [1; 1; 2; 1; 2; 1; 2; 1; 1]);

## -------------------- graph + 'closeness' -----------------------

## Triangle: every node symmetric, c = (N-1)/sum = 2/2 = 1.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! assert (centrality (G, "closeness"), [1; 1; 1], 1e-12);

## Path graph 1-2-3-4-5: endpoints lower, centre highest.
%!test
%! G = graph ([1 2 3 4], [2 3 4 5]);
%! c = centrality (G, "closeness");
%! ## distance sums (exclude diagonal 0):
%! ##   node 1: 1+2+3+4 = 10
%! ##   node 2: 1+1+2+3 = 7
%! ##   node 3: 2+1+1+2 = 6
%! ##   node 4: 3+2+1+1 = 7
##     ##   node 5: 4+3+2+1 = 10
%! ## (N-1)/sum with N=5.
%! assert (c, [4/10; 4/7; 4/6; 4/7; 4/10], 1e-12);

## Star K_{1,5}: centre node has c=1, leaves equal.
%!test
%! G = graph (ones (1, 5), 2:6);
%! c = centrality (G, "closeness");
%! ## Centre (node 1): sum = 5 (one step to each leaf).  c = 5/5 = 1.
%! ## Each leaf: d(leaf,centre)=1, d(leaf,other_leaf)=2 x 4 leaves.
%! ## sum = 1 + 4*2 = 9.  c = 5/9.
%! assert (c, [1; 5/9; 5/9; 5/9; 5/9; 5/9], 1e-12);

## Complete graph K4: every node c = 3/3 = 1.
%!test
%! G = graph ([1 1 1 2 2 3], [2 3 4 3 4 4]);
%! assert (centrality (G, "closeness"), [1; 1; 1; 1], 1e-12);

## Disconnected two-component graph: every node has unreachable nodes
## so all sums are Inf, giving c=0 everywhere.
%!test
%! G = graph ([1 3], [2 4]);
%! assert (centrality (G, "closeness"), zeros (4, 1));

## Edgeless 5-node graph: sums all Inf, c=0.
%!test
%! G = graph (5);
%! assert (centrality (G, "closeness"), zeros (5, 1));

## Weighted graph: uses stored edge weights.
%!test
%! G = graph ([1 2 1], [2 3 3], [1 1 5]);
%! ## d(1,2)=1, d(2,3)=1, d(1,3)=min(5, 1+1)=2.
%! ## sums: node 1 -> 1+2 = 3, node 2 -> 1+1 = 2, node 3 -> 2+1 = 3.
%! ## N-1 = 2.
%! c = centrality (G, "closeness");
%! assert (c, [2/3; 1; 2/3], 1e-12);

## Self-loops are ignored (diagonal is 0 in distances).
%!test
%! G1 = graph ([1 2 1], [1 2 3]);     # self-loops on 1 & 2 plus edge 1-3
%! G2 = graph ([1], [3], [], 3);      # single edge 1-3 in 3-node graph
%! assert (centrality (G1, "closeness"), centrality (G2, "closeness"), 1e-12);

## Result is a column vector of class double.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! c = centrality (G, "closeness");
%! assert (size (c), [3, 1]);
%! assert (class (c), "double");

## Named graph: results follow node order, not names.
%!test
%! G = graph ([1 2 3], [2 3 1], [], {"a", "b", "c"});
%! assert (centrality (G, "closeness"), [1; 1; 1], 1e-12);

## Empty graph returns zeros(0,1).
%!test
%! G = graph ();
%! assert (centrality (G, "closeness"), zeros (0, 1));

## Single-node graph returns zeros(1,1) (no other nodes to be central to).
%!test
%! G = graph (1);
%! c = centrality (G, "closeness");
%! assert (size (c), [1, 1]);
%! assert (c, 0);

## -------------------- graph: incloseness/outcloseness error ------

## 'incloseness' is not defined for an undirected graph.
%!error <incloseness.*only defined|only defined for|digraph>
%! centrality (graph ([1 2], [2 3]), "incloseness");

## 'outcloseness' is not defined for an undirected graph.
%!error <outcloseness.*only defined|only defined for|digraph>
%! centrality (graph ([1 2], [2 3]), "outcloseness");

## -------------------- digraph + 'outcloseness' ------------------

## 3-cycle digraph 1->2->3->1: d(i,j) is 1 or 2, sum=3, c=2/3.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! assert (centrality (G, "outcloseness"), [2/3; 2/3; 2/3], 1e-12);

## 'closeness' on a digraph is an alias for 'outcloseness'.
%!test
%! G = digraph ([1 2 3 1 4], [2 3 4 3 1]);
%! assert (centrality (G, "closeness"), ...
%!         centrality (G, "outcloseness"), 1e-12);

## Fork 1->{2,3}: only node 1 reaches others, so outcloseness(1)=1,
## outcloseness(2)=outcloseness(3)=0 (sums have Inf).
%!test
%! G = digraph ([1 1], [2 3]);
%! assert (centrality (G, "outcloseness"), [1; 0; 0], 1e-12);

## 3-node chain 1->2->3: outgoing distances sum.
%!test
%! G = digraph ([1 2], [2 3]);
%! ## d(1,2)=1, d(1,3)=2, d(2,3)=1; sums 3, 1, 0 (Inf for node 3).
%! ## For node 3 no out-path exists to 1,2 → sum=Inf → c=0.
%! ## For node 2 cannot reach 1 → sum=Inf → c=0.
%! c = centrality (G, "outcloseness");
%! assert (c(1), 2/3, 1e-12);
%! assert (c(2), 0, 1e-12);
%! assert (c(3), 0, 1e-12);

## Empty digraph returns zeros(0,1).
%!test
%! G = digraph ();
%! assert (centrality (G, "outcloseness"), zeros (0, 1));
%! assert (centrality (G, "closeness"), zeros (0, 1));

## Edgeless digraph: c=0 for N>=2, all unreachable.
%!test
%! G = digraph (4);
%! assert (centrality (G, "outcloseness"), zeros (4, 1));
%! assert (centrality (G, "closeness"), zeros (4, 1));

## Self-loops on a digraph are ignored by outcloseness.
%!test
%! G1 = digraph ([1 2 3], [1 3 1]);   # self-loop on 1, plus 2->3, 3->1
%! G2 = digraph ([2 3], [3 1]);       # same topology without the self-loop
%! assert (centrality (G1, "outcloseness"), ...
%!         centrality (G2, "outcloseness"), 1e-12);

## Weighted digraph (3-cycle with asymmetric weights).
%!test
%! G = digraph ([1 2 3], [2 3 1], [1 2 3]);
%! ## d(1,2)=1, d(1,3)=3 (via 2); d(2,3)=2, d(2,1)=5; d(3,1)=3, d(3,2)=4.
%! ## Out sums: node1 -> 1+3=4, node2 -> 2+5=7, node3 -> 3+4=7.
%! assert (centrality (G, "outcloseness"), [2/4; 2/7; 2/7], 1e-12);

## Named digraph.
%!test
%! G = digraph ([1 2 3], [2 3 1], [], {"x", "y", "z"});
%! assert (centrality (G, "outcloseness"), [2/3; 2/3; 2/3], 1e-12);

## Result is column double.
%!test
%! G = digraph ([1 2], [2 3]);
%! c = centrality (G, "outcloseness");
%! assert (size (c), [3, 1]);
%! assert (class (c), "double");

## Single-node digraph returns 0.
%!test
%! G = digraph (1);
%! assert (centrality (G, "outcloseness"), 0);
%! assert (centrality (G, "closeness"), 0);

## -------------------- digraph + 'incloseness' -------------------

## 3-cycle: symmetric in reversed sense.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! assert (centrality (G, "incloseness"), [2/3; 2/3; 2/3], 1e-12);

## Converse fork {2,3}->1: only node 1 is reached.
%!test
%! G = digraph ([2 3], [1 1]);
%! assert (centrality (G, "incloseness"), [1; 0; 0], 1e-12);

## Chain 1->2->3: incloseness from the other end.
%!test
%! G = digraph ([1 2], [2 3]);
%! ## distances to node i (column sums excluding diagonal):
%! ## d(*,1): d(2,1)=Inf, d(3,1)=Inf → Inf; c=0
%! ## d(*,2): d(1,2)=1, d(3,2)=Inf → Inf; c=0
%! ## d(*,3): d(1,3)=2, d(2,3)=1 → sum=3; c=2/3
%! c = centrality (G, "incloseness");
%! assert (c(1), 0, 1e-12);
%! assert (c(2), 0, 1e-12);
%! assert (c(3), 2/3, 1e-12);

## Weighted digraph incloseness (same 3-cycle as above).
%!test
%! G = digraph ([1 2 3], [2 3 1], [1 2 3]);
%! ## In sums (column sums): node1 -> 5+3=8, node2 -> 1+4=5, node3 -> 3+2=5.
%! assert (centrality (G, "incloseness"), [2/8; 2/5; 2/5], 1e-12);

## Empty digraph.
%!test
%! G = digraph ();
%! assert (centrality (G, "incloseness"), zeros (0, 1));

## Edgeless digraph.
%!test
%! G = digraph (3);
%! assert (centrality (G, "incloseness"), zeros (3, 1));

## Named digraph.
%!test
%! G = digraph ([1 2 3], [2 3 1], [], {"x", "y", "z"});
%! assert (centrality (G, "incloseness"), [2/3; 2/3; 2/3], 1e-12);

## Single-node digraph incloseness = 0.
%!test
%! G = digraph (1);
%! assert (centrality (G, "incloseness"), 0);

## -------------------- closeness case-insensitivity --------------

## 'Closeness' / 'CLOSENESS' / 'cLoSeNeSs' all match 'closeness'.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! assert (centrality (G, "Closeness"), [1; 1; 1], 1e-12);
%! assert (centrality (G, "CLOSENESS"), [1; 1; 1], 1e-12);
%! assert (centrality (G, "cLoSeNeSs"), [1; 1; 1], 1e-12);

## 'InCloseness' / 'INCLOSENESS' for digraph.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! assert (centrality (G, "InCloseness"), [2/3; 2/3; 2/3], 1e-12);
%! assert (centrality (G, "INCLOSENESS"), [2/3; 2/3; 2/3], 1e-12);

## 'OutCloseness' / 'OUTCLOSENESS' for digraph.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! assert (centrality (G, "OutCloseness"), [2/3; 2/3; 2/3], 1e-12);
%! assert (centrality (G, "OUTCLOSENESS"), [2/3; 2/3; 2/3], 1e-12);

## -------------------- dot-notation dispatch (closeness) ---------

## Dot-notation dispatch for graph closeness.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! assert (G.centrality ("closeness"), [1; 1; 1], 1e-12);

## Dot-notation dispatch for digraph closeness/incloseness/outcloseness.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! assert (G.centrality ("closeness"), [2/3; 2/3; 2/3], 1e-12);
%! assert (G.centrality ("incloseness"), [2/3; 2/3; 2/3], 1e-12);
%! assert (G.centrality ("outcloseness"), [2/3; 2/3; 2/3], 1e-12);

## -------------------- not-yet-implemented types -----------------
## These should error until US-CT03+ add them.

%!error <unknown|invalid|unrecognized|not yet|not implemented>
%! centrality (graph ([1 2], [2 3]), "betweenness");

%!error <unknown|invalid|unrecognized|not yet|not implemented>
%! centrality (graph ([1 2], [2 3]), "pagerank");

%!error <unknown|invalid|unrecognized|not yet|not implemented>
%! centrality (graph ([1 2], [2 3]), "eigenvector");

%!error <unknown|invalid|unrecognized|not yet|not implemented>
%! centrality (digraph ([1 2], [2 3]), "hubs");

%!error <unknown|invalid|unrecognized|not yet|not implemented>
%! centrality (digraph ([1 2], [2 3]), "authorities");
