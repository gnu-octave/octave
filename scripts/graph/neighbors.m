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
## @deftypefn  {} {@var{n} =} neighbors (@var{G}, @var{nodeID})
## Return the nodes adjacent to @var{nodeID} in the graph or digraph
## @var{G}.
##
## @var{G} must be a @code{graph} or @code{digraph} object.  For the
## undirected @code{graph} class, the neighbours are the nodes directly
## connected to @var{nodeID} by an incident edge.  For the directed
## @code{digraph} class, the neighbours are the union of the successors
## and predecessors of @var{nodeID} (i.e.@: all nodes connected by any
## directed edge, regardless of direction).
##
## @var{nodeID} is a @emph{scalar} node identifier:
##
## @itemize
## @item
## A positive integer scalar in the range @code{1:numnodes (@var{G})} is
## treated as a 1-based node index.  In that case @var{n} is a numeric
## column vector of node indices.
## @item
## A character row vector (or a 1-element cell array of strings) is
## treated as a node name, which must appear in @code{@var{G}.Nodes.Name}.
## In that case @var{n} is a column cell array of character vectors
## containing the names of the neighbour nodes.
## @end itemize
##
## The return type always matches the input type.  A self-loop at
## @var{nodeID} contributes @var{nodeID} once to the output.  An
## isolated node (no incident edges) yields an empty column (shape
## @code{[0 1]}).  For a multigraph, each parallel edge between
## @var{nodeID} and another node contributes one entry to the output, so
## duplicates are possible.
##
## @example
## @group
## G = graph ([1 1 2 3], [2 3 3 4]);
## neighbors (G, 1)
##    @result{} 2
##       3
## D = digraph ([1 1 2 3], [2 3 3 1]);
## neighbors (D, 1)
##    @result{} 2
##       3
## @end group
## @end example
##
## @seealso{graph, digraph, successors, predecessors, degree, indegree, outdegree}
## @end deftypefn

function n = neighbors (G, nodeID)

  ## NOTE: When called with a graph or digraph object, Octave's classdef
  ## method dispatch runs the class-internal @code{neighbors} method and
  ## this free-function body is not reached.  This file exists both as a
  ## canonical documentation target (so @code{help neighbors} works
  ## outside the context of an instance) and as a fallback that gives a
  ## helpful error for non-graph inputs.

  if (nargin != 2)
    print_usage ();
  endif

  if (! isa (G, "graph") && ! isa (G, "digraph"))
    error ("Octave:invalid-input-arg", ...
           "neighbors: G must be a graph or digraph object");
  endif

  ## Defensive delegation: if class dispatch ever skips past the free
  ## function (e.g. future subclassing edge cases) route back to the
  ## class method via dot notation, which is always class-dispatched.
  n = G.neighbors (nodeID);

endfunction


## ------------------------------------------------------------------
## BIST blocks
## ------------------------------------------------------------------

## ---------------- graph (undirected) -----------------------------

## Empty neighbours (isolated node).
%!test
%! G = graph (3);
%! nb = neighbors (G, 1);
%! assert (nb, zeros (0, 1));

## Single neighbour.
%!test
%! G = graph ([1], [2]);
%! nb = neighbors (G, 1);
%! assert (nb, 2);
%!test
%! G = graph ([1], [2]);
%! nb = neighbors (G, 2);
%! assert (nb, 1);

## Multiple neighbours returned in increasing index order.
%!test
%! G = graph ([1 1 1], [3 2 4]);
%! nb = neighbors (G, 1);
%! assert (nb, [2; 3; 4]);

## Column-vector shape.
%!test
%! G = graph ([1 1], [2 3]);
%! nb = neighbors (G, 1);
%! assert (size (nb), [2, 1]);

## Numeric input returns numeric output (class double).
%!test
%! G = graph ([1 2], [2 3]);
%! nb = neighbors (G, 2);
%! assert (class (nb), "double");
%! assert (nb, [1; 3]);

## Undirected neighbours (path graph) - middle node has 2 neighbours.
%!test
%! G = graph ([1 2 3], [2 3 4]);
%! assert (neighbors (G, 1), 2);
%! assert (neighbors (G, 2), [1; 3]);
%! assert (neighbors (G, 3), [2; 4]);
%! assert (neighbors (G, 4), 3);

## Self-loop: node is its own neighbour (reported once).
%!test
%! G = graph ([1 2], [1 3]);
%! assert (neighbors (G, 1), 1);

## Self-loop plus another edge at the same node.
%!test
%! G = graph ([1 1 2], [1 2 3]);
%! nb = neighbors (G, 1);
%! assert (nb, [1; 2]);

## Weighted graph: neighbours ignores weights.
%!test
%! G = graph ([1 1 1], [2 3 4], [0.5 0.25 0.75]);
%! nb = neighbors (G, 1);
%! assert (nb, [2; 3; 4]);

## Named graph + string node name -> cellstr result.
%!test
%! G = graph ([1 1 2], [2 3 3], [], {"alpha", "beta", "gamma"});
%! nb = neighbors (G, "alpha");
%! assert (iscellstr (nb));
%! assert (nb, {"beta"; "gamma"});

## Named graph + 1-element cellstr -> cellstr result.
%!test
%! G = graph ([1 1 2], [2 3 3], [], {"alpha", "beta", "gamma"});
%! nb = neighbors (G, {"beta"});
%! assert (iscellstr (nb));
%! assert (nb, {"alpha"; "gamma"});

## Named graph + numeric index -> numeric result.
%!test
%! G = graph ([1 1 2], [2 3 3], [], {"alpha", "beta", "gamma"});
%! nb = neighbors (G, 2);
%! assert (class (nb), "double");
%! assert (nb, [1; 3]);

## Named graph + name input + isolated node -> empty cellstr column.
%!test
%! G = graph ([1 1], [2 3], [], {"a", "b", "c", "d"});
%! nb = neighbors (G, "d");
%! assert (iscell (nb));
%! assert (size (nb), [0, 1]);

## Adjacency-matrix constructor round-trip (symmetric).
%!test
%! A = [0 1 1 0; 1 0 1 0; 1 1 0 1; 0 0 1 0];
%! G = graph (A);
%! assert (neighbors (G, 1), [2; 3]);
%! assert (neighbors (G, 2), [1; 3]);
%! assert (neighbors (G, 3), [1; 2; 4]);
%! assert (neighbors (G, 4), 3);

## N-node edgeless graph: every node has no neighbours.
%!test
%! G = graph (4);
%! for ii = 1:4
%!   assert (neighbors (G, ii), zeros (0, 1));
%! endfor

## ---------------- digraph (directed) -----------------------------

## Empty neighbours (isolated node in digraph).
%!test
%! G = digraph (3);
%! nb = neighbors (G, 1);
%! assert (nb, zeros (0, 1));

## Digraph: union of successors and predecessors, sorted ascending.
%!test
%! G = digraph ([1 1 2 3], [2 3 3 1]);
%! assert (neighbors (G, 1), [2; 3]);
%! assert (neighbors (G, 2), [1; 3]);
%! assert (neighbors (G, 3), [1; 2]);

## Digraph: a single out-edge makes the destination a neighbour of the source,
## and vice versa.
%!test
%! G = digraph ([1], [2]);
%! assert (neighbors (G, 1), 2);
%! assert (neighbors (G, 2), 1);

## Digraph: node appearing only as a successor vs predecessor still shows up.
%!test
%! G = digraph ([1 2], [2 3]);
%! assert (neighbors (G, 1), 2);
%! assert (neighbors (G, 2), [1; 3]);
%! assert (neighbors (G, 3), 2);

## Digraph: duplicates from successor/predecessor sets are merged (a<->b).
%!test
%! G = digraph ([1 2], [2 1]);
%! assert (neighbors (G, 1), 2);
%! assert (neighbors (G, 2), 1);

## Digraph self-loop: reported once in neighbors.
%!test
%! G = digraph ([1 2], [1 3]);
%! assert (neighbors (G, 1), 1);

## Digraph self-loop plus normal edges: self appears once, others appear once.
%!test
%! G = digraph ([1 1 2], [1 2 3]);
%! nb = neighbors (G, 1);
%! assert (nb, [1; 2]);

## Digraph: siever-style 9-node fixture.  Node 3 has out-edges to {2,4}
## and one in-edge from {2}.  Union sorted (dedup of node 2): [2;4].
%!test
%! G = digraph ([1 2 3 3 4 5 5 6 7 7 8 9], ...
%!              [2 3 2 4 5 6 9 7 8 9 7 4]);
%! assert (neighbors (G, 3), [2; 4]);

## Digraph: another siever-node union check.  Node 4 has out-edge to 5
## and in-edges from {3, 9}.  Union sorted: [3;5;9].
%!test
%! G = digraph ([1 2 3 3 4 5 5 6 7 7 8 9], ...
%!              [2 3 2 4 5 6 9 7 8 9 7 4]);
%! assert (neighbors (G, 4), [3; 5; 9]);

## Digraph column-vector shape.
%!test
%! G = digraph ([1 1 2 3], [2 3 3 1]);
%! nb = neighbors (G, 1);
%! assert (size (nb), [2, 1]);

## Digraph numeric-in -> numeric-out.
%!test
%! G = digraph ([1 2], [2 3]);
%! nb = neighbors (G, 2);
%! assert (class (nb), "double");

## Digraph weighted: neighbours ignores weights.
%!test
%! G = digraph ([1 1 1], [2 3 4], [0.5 0.25 0.75]);
%! nb = neighbors (G, 1);
%! assert (nb, [2; 3; 4]);

## Named digraph + string name -> cellstr result.
%!test
%! G = digraph ([1 1 2], [2 3 3], [], {"alpha", "beta", "gamma"});
%! nb = neighbors (G, "alpha");
%! assert (iscellstr (nb));
%! assert (nb, {"beta"; "gamma"});

## Named digraph + 1-element cellstr -> cellstr result.
%!test
%! G = digraph ([1 1 2], [2 3 3], [], {"alpha", "beta", "gamma"});
%! nb = neighbors (G, {"gamma"});
%! assert (iscellstr (nb));
%! assert (nb, {"alpha"; "beta"});

## Named digraph + numeric index -> numeric result.
%!test
%! G = digraph ([1 1 2], [2 3 3], [], {"alpha", "beta", "gamma"});
%! nb = neighbors (G, 2);
%! assert (class (nb), "double");
%! assert (nb, [1; 3]);

## Named digraph + isolated node with name -> empty cellstr column.
%!test
%! G = digraph ([1 1], [2 3], [], {"a", "b", "c", "d"});
%! nb = neighbors (G, "d");
%! assert (iscell (nb));
%! assert (size (nb), [0, 1]);

## Digraph multigraph: parallel edges contribute one neighbour entry each.
%!test
%! G = digraph ([1 1 1 2], [2 2 3 3], "multigraph");
%! nb = neighbors (G, 1);
%! assert (nb, [2; 2; 3]);

## Digraph multigraph: anti-parallel edges (1->2 and 2->1) each contribute.
%!test
%! G = digraph ([1 2], [2 1], "multigraph");
%! nb = neighbors (G, 1);
%! assert (nb, [2; 2]);

## Digraph multigraph: self-loop stored as one (n,n) edge -> n appears once.
%!test
%! G = digraph ([1 1 2], [1 2 3], "multigraph");
%! nb = neighbors (G, 1);
%! assert (nb, [1; 2]);

## Digraph multigraph: duplicate self-loops -> each counts once per edge.
%!test
%! G = digraph ([1 1 1], [1 1 2], "multigraph");
%! nb = neighbors (G, 1);
%! assert (nb, [1; 1; 2]);

## Digraph adjacency-matrix round-trip.
%!test
%! A = sparse ([0 1 1; 0 0 1; 1 0 0]);
%! G = digraph (A);
%! assert (neighbors (G, 1), [2; 3]);
%! assert (neighbors (G, 2), [1; 3]);
%! assert (neighbors (G, 3), [1; 2]);

## ---------------- error cases ---------------------------------------

## Error: node index out of range (too large) on graph.
%!error <invalid node index> neighbors (graph (3), 4)

## Error: node index zero on graph.
%!error <invalid node index> neighbors (graph (3), 0)

## Error: non-integer node index on graph.
%!error <invalid node index> neighbors (graph (3), 1.5)

## Error: node index out of range on digraph.
%!error <invalid node index> neighbors (digraph (3), 4)

## Error: node index zero on digraph.
%!error <invalid node index> neighbors (digraph (3), 0)

## Error: non-integer node index on digraph.
%!error <invalid node index> neighbors (digraph (3), 1.5)

## Error: non-existent node name on graph.
%!error <not found> ...
%!   neighbors (graph ([1 2], [2 3], [], {"a","b","c"}), "z")

## Error: non-existent node name on digraph.
%!error <not found> ...
%!   neighbors (digraph ([1 2], [2 3], [], {"a","b","c"}), "z")

## Error: node name on graph without names.
%!error <no node names|not found> neighbors (graph (3), "foo")

## Error: node name on digraph without names.
%!error <no node names|not found> neighbors (digraph (3), "foo")

## Error: non-scalar numeric nodeID.
%!error <scalar> neighbors (graph (3), [1 2])
%!error <scalar> neighbors (digraph (3), [1 2])

## Error: multi-element cellstr nodeID.
%!error <scalar> ...
%!   neighbors (graph ([1 2], [2 3], [], {"a","b","c"}), {"a","b"})
%!error <scalar> ...
%!   neighbors (digraph ([1 2], [2 3], [], {"a","b","c"}), {"a","b"})

## Error: non-graph first argument routes through the free-function guard.
%!error <G must be a graph or digraph> neighbors (3, 1)
%!error <G must be a graph or digraph> neighbors ("hello", 1)

## Error: nargin mismatch.
%!error <Invalid call> neighbors ()
%!error <Invalid call> neighbors (graph (3))
%!error <Invalid call> neighbors (digraph (3))
