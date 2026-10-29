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
## @deftypefn {} {@var{C} =} condensation (@var{G})
## Compute the graph condensation of the directed graph @var{G}.
##
## @var{G} must be a @code{digraph} object.  Condensation is not
## defined on the undirected @code{graph} class; calling
## @code{condensation} on a @code{graph} raises an error.
##
## The condensation @var{C} is a @code{digraph} in which every node
## represents one strongly connected component (SCC) of @var{G} and
## there is an edge from node @math{i} to node @math{j} whenever at
## least one edge of @var{G} goes from a node in SCC @math{i} to a
## node in SCC @math{j} (with @math{i \neq j}).  Parallel cross-SCC
## edges are merged: if @var{G} has edge weights, the result's edge
## weights are the sums of the original weights across the merged
## edges.  Self-loops and within-SCC edges are dropped so @var{C} is
## always a directed acyclic graph (DAG).
##
## The result @var{C} carries a @code{Component} column on
## @code{C.Nodes} that lists, for each new node, the original members
## of the corresponding SCC.  When @var{G} has named nodes each
## @code{Component@{k@}} is a column cell array of name strings;
## otherwise it is a column vector of original node indices.
##
## SCC numbering follows @code{conncomp (@var{G}, @qcode{"Type"},
## @qcode{"strong"})}: the SCC containing the smallest unlabelled
## index always receives the next unused label.  Thus the node
## containing @code{1} becomes node @code{1} of @var{C} and so on.
##
## @example
## @group
## G = digraph ([1 2 3 4], [2 3 1 5]);
## C = condensation (G);
## numnodes (C)            # @result{} 3
## numedges (C)            # @result{} 1
## C.Edges.EndNodes        # @result{} [2 3]
## C.Nodes.Component       # @result{} @{[1;2;3]; 4; 5@}
## @end group
## @end example
##
## @seealso{digraph, conncomp, bfsearch, dfsearch}
## @end deftypefn

function C = condensation (G)

  ## NOTE: When called with a graph or digraph object, Octave's
  ## classdef method dispatch runs the class-internal
  ## @code{condensation} method and this free-function body is not
  ## reached.  This file exists both as a canonical documentation
  ## target (so @code{help condensation} works outside the context of
  ## an instance) and as a fallback that gives a helpful error for
  ## non-digraph inputs.

  if (nargin < 1)
    print_usage ();
  endif

  if (isa (G, "graph"))
    error ("Octave:invalid-input-arg", ...
           "condensation: not defined for an undirected graph; condensation requires a digraph");
  endif

  if (! isa (G, "digraph"))
    error ("Octave:invalid-input-arg", ...
           "condensation: G must be a digraph object");
  endif

  ## Defensive delegation: classdef method dispatch should intercept
  ## any call with a digraph first arg, but route through dot notation
  ## just in case.
  C = G.condensation ();

endfunction


## ------------------------------------------------------------------
## BIST blocks
## ------------------------------------------------------------------

## -------------------- basic error cases --------------------

## condensation on an undirected graph is an error.
%!error <not defined for an undirected graph>
%! G = graph ([1 2], [2 3]);
%! condensation (G);

## condensation on an empty graph is still an error.
%!error <requires a digraph>
%! G = graph ();
%! condensation (G);

## condensation on a non-graph input is an error.
%!error <must be a digraph object>
%! condensation (42);

%!error <must be a digraph object>
%! condensation ("foo");

## condensation with no args is an error via print_usage.
%!error condensation ()

## -------------------- basic structural properties --------------------

## Empty digraph: condensation returns an empty digraph.
%!test
%! G = digraph ();
%! C = condensation (G);
%! assert (isa (C, "digraph"));
%! assert (numnodes (C), 0);
%! assert (numedges (C), 0);

## Empty digraph: Component column is an empty 0x1 cell array.
%!test
%! G = digraph ();
%! C = condensation (G);
%! assert (isfield (C.Nodes, "Component"));
%! assert (size (C.Nodes.Component), [0, 1]);
%! assert (iscell (C.Nodes.Component));

## Single isolated node: one SCC, one node, no edges.
%!test
%! G = digraph (1);
%! C = condensation (G);
%! assert (numnodes (C), 1);
%! assert (numedges (C), 0);
%! assert (C.Nodes.Component, {[1]});

## Three isolated nodes: three SCCs, three nodes, no edges.
%!test
%! G = digraph (3);
%! C = condensation (G);
%! assert (numnodes (C), 3);
%! assert (numedges (C), 0);
%! assert (C.Nodes.Component, {[1]; [2]; [3]});

## A directed 3-cycle collapses into a single node with no self-loop.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! C = condensation (G);
%! assert (numnodes (C), 1);
%! assert (numedges (C), 0);
%! assert (C.Nodes.Component, {[1; 2; 3]});

## Line 1->2->3: three SCCs, two edges (1->2 and 2->3).
%!test
%! G = digraph ([1 2], [2 3]);
%! C = condensation (G);
%! assert (numnodes (C), 3);
%! assert (numedges (C), 2);
%! EN = C.Edges.EndNodes;
%! assert (EN, [1, 2; 2, 3]);
%! assert (C.Nodes.Component, {[1]; [2]; [3]});

## Self-loop only: node keeps itself as its SCC, no edges in C.
%!test
%! G = digraph (1, 1);
%! C = condensation (G);
%! assert (numnodes (C), 1);
%! assert (numedges (C), 0);

## Self-loop on a 2-node digraph: 1->1, 2 isolated. Two SCCs, no edges.
%!test
%! G = digraph ([1], [1], [], 2);
%! C = condensation (G);
%! assert (numnodes (C), 2);
%! assert (numedges (C), 0);

## -------------------- MATLAB doc-style example --------------------

## MATLAB doc example: 1->2->3->1 and 4->5.  Three SCCs: {1,2,3}, {4},
## {5}.  Cross-SCC edges: 4->5 only.
%!test
%! G = digraph ([1 2 3 4], [2 3 1 5]);
%! C = condensation (G);
%! assert (numnodes (C), 3);
%! assert (numedges (C), 1);
%! assert (C.Edges.EndNodes, [2, 3]);
%! assert (C.Nodes.Component, {[1; 2; 3]; [4]; [5]});

## Two back-to-back 2-cycles: {1,2} and {3,4}.  No cross edges.
%!test
%! G = digraph ([1 2 3 4], [2 1 4 3]);
%! C = condensation (G);
%! assert (numnodes (C), 2);
%! assert (numedges (C), 0);
%! assert (C.Nodes.Component, {[1; 2]; [3; 4]});

## Bigger test: two interlocking cycles.  G has SCCs {1,2,3}, {4,5,6,7},
## and {8}.  Cross edges: 7->8.
%!test
%! G = digraph ([1 2 3 4 5 6 7 7 5], [2 3 1 5 6 7 5 8 4]);
%! C = condensation (G);
%! assert (numnodes (C), 3);
%! ## Edge 7->8 maps to (2, 3) in C.
%! assert (C.Edges.EndNodes, [2, 3]);
%! assert (C.Nodes.Component, {[1; 2; 3]; [4; 5; 6; 7]; [8]});

## -------------------- deduplication of parallel SCC edges --------------------

## Two SCCs {1,2} and {3}; two edges cross (1->3 and 2->3) which should
## merge into a single edge (1, 2) in C for an unweighted digraph.
%!test
%! G = digraph ([1 2 1], [2 1 3], [], 3);
%! G = addedge (G, 2, 3);
%! C = condensation (G);
%! assert (numnodes (C), 2);
%! assert (numedges (C), 1);
%! assert (C.Edges.EndNodes, [1, 2]);

## Three SCCs {1,2}, {3,4}, {5}; one edge 5->1 crosses.
%!test
%! G = digraph ([1 2 3 4 5], [2 1 4 3 1]);
%! C = condensation (G);
%! assert (numnodes (C), 3);
%! assert (numedges (C), 1);
%! ## SCC labels: node 1 -> 1, node 3 -> 2, node 5 -> 3. Edge 5->1 maps
%! ## to (3, 1).
%! assert (C.Edges.EndNodes, [3, 1]);

## -------------------- weighted digraph --------------------

## Weighted digraph: single cross edge carries its weight forward.
%!test
%! G = digraph ([1 2], [2 3], [10 20]);
%! C = condensation (G);
%! assert (numnodes (C), 3);
%! assert (numedges (C), 2);
%! assert (C.Edges.EndNodes, [1, 2; 2, 3]);
%! assert (C.Edges.Weight, [10; 20]);

## Weighted digraph with multiple parallel cross-SCC edges: weights sum.
## G has SCCs {1,2}, {3,4}, {5}.  Cross edges: 5->1 (w=10), 5->2 (w=20).
## Both map to (3, 1) in C so weight = 30.
%!test
%! G = digraph ([1 2 3 4 5 5], [2 1 4 3 1 2], [1 1 1 1 10 20]);
%! C = condensation (G);
%! assert (numnodes (C), 3);
%! assert (numedges (C), 1);
%! assert (C.Edges.EndNodes, [3, 1]);
%! assert (C.Edges.Weight, 30);

## Unweighted digraph does NOT gain a Weight field from condensation.
%!test
%! G = digraph ([1 2], [2 3]);
%! C = condensation (G);
%! assert (! isfield (C.Edges, "Weight"));

## Weighted digraph DOES carry a Weight field through condensation.
%!test
%! G = digraph ([1 2], [2 3], [1.5 2.5]);
%! C = condensation (G);
%! assert (isfield (C.Edges, "Weight"));
%! assert (C.Edges.Weight, [1.5; 2.5]);

## -------------------- named nodes --------------------

## Named digraph: Component is a cell of cellstr containing original names.
%!test
%! G = digraph ([1 2 3 4], [2 3 1 5], [], {"a","b","c","d","e"});
%! C = condensation (G);
%! assert (numnodes (C), 3);
%! assert (numedges (C), 1);
%! assert (iscell (C.Nodes.Component));
%! assert (iscellstr (C.Nodes.Component{1}));
%! assert (C.Nodes.Component{1}, {"a"; "b"; "c"});
%! assert (C.Nodes.Component{2}, {"d"});
%! assert (C.Nodes.Component{3}, {"e"});

## Named digraph but still a DAG at node level: each name ends up alone.
%!test
%! G = digraph ([1 2], [2 3], [], {"x","y","z"});
%! C = condensation (G);
%! assert (numnodes (C), 3);
%! assert (C.Nodes.Component, {{"x"}; {"y"}; {"z"}});

## -------------------- DAG property of the output --------------------

## Condensation result has no self-loops.
%!test
%! G = digraph ([1 2 3 4], [2 3 1 5]);
%! C = condensation (G);
%! if (numedges (C) > 0)
%!   EN = C.Edges.EndNodes;
%!   assert (all (EN(:, 1) != EN(:, 2)));
%! endif

## Condensation is a DAG: every node of C is its own strongly connected
## component (conncomp 'strong' labels are 1..numnodes).
%!test
%! G = digraph ([1 2 3 4], [2 3 1 5]);
%! C = condensation (G);
%! N = numnodes (C);
%! if (N > 0)
%!   scc = conncomp (C, "Type", "strong");
%!   assert (scc, 1:N);
%! endif

## DAG property also holds on the interlocking-cycles example.
%!test
%! G = digraph ([1 2 3 4 5 6 7 7 5], [2 3 1 5 6 7 5 8 4]);
%! C = condensation (G);
%! N = numnodes (C);
%! scc = conncomp (C, "Type", "strong");
%! assert (scc, 1:N);

## DAG property on a path graph (already a DAG).
%!test
%! G = digraph ([1 2 3 4], [2 3 4 5]);
%! C = condensation (G);
%! assert (numnodes (C), 5);
%! assert (numedges (C), 4);
%! N = numnodes (C);
%! scc = conncomp (C, "Type", "strong");
%! assert (scc, 1:N);

## DAG property on disconnected components.
%!test
%! G = digraph ([1 3], [2 4]);
%! C = condensation (G);
%! N = numnodes (C);
%! scc = conncomp (C, "Type", "strong");
%! assert (scc, 1:N);

## A digraph that is already a DAG condenses to an isomorphic digraph.
%!test
%! G = digraph ([1 1 2 2], [2 3 4 5]);
%! C = condensation (G);
%! assert (numnodes (C), 5);
%! assert (numedges (C), 4);
%! ## Same edge set (lex sorted): (1,2),(1,3),(2,4),(2,5)
%! assert (C.Edges.EndNodes, [1 2; 1 3; 2 4; 2 5]);

## -------------------- dot notation dispatch --------------------

## Dot-notation call: G.condensation() returns the same as
## condensation (G).
%!test
%! G = digraph ([1 2 3 4], [2 3 1 5]);
%! C1 = condensation (G);
%! C2 = G.condensation ();
%! assert (numnodes (C1), numnodes (C2));
%! assert (numedges (C1), numedges (C2));
%! assert (C1.Edges.EndNodes, C2.Edges.EndNodes);
%! assert (C1.Nodes.Component, C2.Nodes.Component);

## -------------------- Component column details --------------------

## Component column is a column cell (not a row cell).
%!test
%! G = digraph ([1 2], [2 3]);
%! C = condensation (G);
%! assert (size (C.Nodes.Component, 2), 1);
%! assert (size (C.Nodes.Component, 1), numnodes (C));

## Component{k} is a column vector for numeric-indexed digraphs.
%!test
%! G = digraph ([1 2 3 4], [2 3 1 5]);
%! C = condensation (G);
%! for k = 1:numnodes (C)
%!   assert (size (C.Nodes.Component{k}, 2), 1);
%! endfor

## Component entries are sorted (smallest member index first).
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! C = condensation (G);
%! mem = C.Nodes.Component{1};
%! assert (mem, sort (mem));

## Every node of G appears exactly once across all Component{k}.
%!test
%! G = digraph ([1 2 3 4 5 6 7 7 5], [2 3 1 5 6 7 5 8 4]);
%! C = condensation (G);
%! all_members = vertcat (C.Nodes.Component{:});
%! assert (sort (all_members), (1:numnodes (G))');

## Component labels follow conncomp('strong'): the SCC of node 1 is
## always the first Component cell.
%!test
%! G = digraph ([2 3 1 4 5], [3 1 2 5 4]);
%! C = condensation (G);
%! ## Nodes 1,2,3 form one SCC; nodes 4,5 form another.  Smallest index
%! ## 1 is in the first SCC, smallest unlabelled after that is 4 -> SCC 2.
%! assert (C.Nodes.Component, {[1; 2; 3]; [4; 5]});

## -------------------- larger example --------------------

## 20-node interlocking example -> condensation has fewer nodes, is a DAG.
%!test
%! s = [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20];
%! t = [2 3 1 5 6 4 8 9 7 11 12 10 14 15 13 17 18 16 20 19];
%! ## 6 3-cycles {1,2,3},{4,5,6},{7,8,9},{10,11,12},{13,14,15},{16,17,18}
%! ## plus a 2-cycle {19,20}.  7 SCCs, no cross edges.
%! G = digraph (s, t);
%! C = condensation (G);
%! assert (numnodes (C), 7);
%! assert (numedges (C), 0);
%! N = numnodes (C);
%! scc = conncomp (C, "Type", "strong");
%! assert (scc, 1:N);
