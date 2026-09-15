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
## @deftypefn  {} {@var{v} =} dfsearch (@var{G}, @var{s})
## @deftypefnx {} {@var{v} =} dfsearch (@var{G}, @var{s}, @var{event})
## @deftypefnx {} {@var{T} =} dfsearch (@var{G}, @var{s}, @var{events})
## Perform a depth-first search of the graph @var{G} starting at node
## @var{s} and return the column vector @var{v} of node indices in the
## order they are discovered.
##
## @var{G} must be a @code{graph} or @code{digraph} object.  @var{s} is
## a @emph{scalar} node identifier:
##
## @itemize
## @item
## A positive integer scalar in the range @code{1:numnodes (@var{G})} is
## treated as a 1-based node index.
## @item
## A character row vector (or a 1-element cell array of strings) is
## treated as a node name, which must appear in @code{@var{G}.Nodes.Name}.
## @end itemize
##
## The returned vector @var{v} is always a numeric column vector of node
## @emph{indices} (not names), even when @var{s} was given as a node
## name.  Its length is the number of nodes reachable from @var{s} in
## @var{G}.  For an undirected @code{graph}, reachability follows
## incident edges in both directions; for a directed @code{digraph},
## reachability follows out-edges.
##
## When multiple unvisited neighbours of a node are available, they are
## visited in ascending order of node index.  This tie-breaking rule
## matches MATLAB's documented DFS traversal order and ensures the
## return value is deterministic.
##
## Self-loops and parallel edges in a multigraph are collapsed for the
## purpose of neighbour enumeration -- DFS visits each neighbour at most
## once regardless of how many parallel edges connect it to the current
## node.  The edge @emph{events} (below) do emit one event per distinct
## @code{(u, v)} pair.
##
## @var{v} starts with @var{s} and contains only the nodes reachable
## from @var{s}.  Nodes in other connected components (or, for a
## digraph, nodes not reachable by following out-edges) are omitted.
##
## @example
## @group
## G = digraph ([1 1 2 2 3 3], [2 3 4 5 6 7]);
## dfsearch (G, 1)
##    @result{} 1
##       2
##       4
##       5
##       3
##       6
##       7
## @end group
## @end example
##
## With an optional third argument @var{events} (a character string or
## a cell array of event names), return the DFS @emph{event log} rather
## than just the discovery order.  Valid event names are
## @qcode{"discovernode"}, @qcode{"finishnode"}, @qcode{"startnode"}
## (node events, return a column vector of indices),
## @qcode{"edgetonew"}, @qcode{"edgetodiscovered"}, and
## @qcode{"edgetofinished"} (edge events, return an @math{m}-by-2
## matrix of @code{[src, dst]} index pairs).  The special argument
## @qcode{"allevents"} or any cell array of event names returns a scalar
## struct with fields @code{Event} (cellstr column), @code{Node}
## (double column), and @code{Edge} (@math{m}-by-2 double).
##
## For DFS, @qcode{"edgetodiscovered"} marks an edge whose target is
## currently on the active DFS stack (a @emph{back edge}), and
## @qcode{"edgetofinished"} marks an edge whose target has already
## finished (a @emph{cross} or @emph{forward edge}).
##
## @seealso{graph, digraph, bfsearch, successors, predecessors, neighbors}
## @end deftypefn

function v = dfsearch (G, s, events, varargin)

  ## NOTE: When called with a graph or digraph object, Octave's classdef
  ## method dispatch runs the class-internal @code{dfsearch} method and
  ## this free-function body is not reached.  This file exists both as a
  ## canonical documentation target (so @code{help dfsearch} works
  ## outside the context of an instance) and as a fallback that gives a
  ## helpful error for non-graph inputs.

  if (nargin < 2)
    print_usage ();
  endif

  if (! isa (G, "graph") && ! isa (G, "digraph"))
    error ("Octave:invalid-input-arg", ...
           "dfsearch: G must be a graph or digraph object");
  endif

  ## Defensive delegation: if class dispatch ever skips past the free
  ## function (e.g. future subclassing edge cases) route back to the
  ## class method via dot notation, which is always class-dispatched.
  if (nargin == 2)
    v = G.dfsearch (s);
  else
    v = G.dfsearch (s, events, varargin{:});
  endif

endfunction


## ------------------------------------------------------------------
## BIST blocks
## ------------------------------------------------------------------

## -------------------- digraph tests --------------------

## Default single-node digraph: DFS from the sole node visits only that node.
%!test
%! G = digraph (1);
%! v = dfsearch (G, 1);
%! assert (v, 1);

## Empty edge set, source node in range: DFS returns just the source.
%!test
%! G = digraph (5);
%! v = dfsearch (G, 3);
%! assert (v, 3);

## Simple directed 3-cycle 1->2->3->1: DFS from 1 discovers [1; 2; 3].
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! v = dfsearch (G, 1);
%! assert (v, [1; 2; 3]);

## DFS from a different source on the same 3-cycle.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! v = dfsearch (G, 2);
%! assert (v, [2; 3; 1]);

## Tie-break: multiple unvisited out-neighbours visited in ascending index order.
%!test
%! G = digraph ([1 1 1], [4 2 3]);
%! v = dfsearch (G, 1);
%! assert (v, [1; 2; 3; 4]);

## DFS vs BFS order on a tree-like digraph.  Tree:
##   1 -> {2, 3}; 2 -> {4, 5}; 3 -> {6, 7}
## DFS preorder from 1: 1, 2, 4, 5, 3, 6, 7 (goes deep before siblings).
%!test
%! G = digraph ([1 1 2 2 3 3], [2 3 4 5 6 7]);
%! v = dfsearch (G, 1);
%! assert (v, [1; 2; 4; 5; 3; 6; 7]);

## DFS tie-break + depth-first on a 7-node binary tree.
%!test
%! G = digraph ([1 1 2 2 3 3], [3 2 5 4 7 6]);  # unsorted input
%! v = dfsearch (G, 1);
%! assert (v, [1; 2; 4; 5; 3; 6; 7]);

## DFS returns a column vector.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! v = dfsearch (G, 1);
%! assert (size (v), [3, 1]);
%! assert (iscolumn (v));

## DFS result is class double.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! v = dfsearch (G, 1);
%! assert (class (v), "double");

## Node name input still returns numeric indices (MATLAB parity).
%!test
%! G = digraph ([1 2 3], [2 3 1], [], {"a", "b", "c"});
%! v = dfsearch (G, "b");
%! assert (class (v), "double");
%! assert (v, [2; 3; 1]);

## 1-element cellstr name input.
%!test
%! G = digraph ([1 2 3], [2 3 1], [], {"a", "b", "c"});
%! v = dfsearch (G, {"c"});
%! assert (v, [3; 1; 2]);

## Digraph where some nodes are unreachable from the source.
## Components: 1->2->3 (from 1) and 4->5 (disjoint).  DFS from 1 omits 4,5.
%!test
%! G = digraph ([1 2 4], [2 3 5]);
%! v = dfsearch (G, 1);
%! assert (v, [1; 2; 3]);

## Digraph with back edge: DFS from 1 on 1->2, 2->3, 3->1 still yields [1;2;3].
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! v = dfsearch (G, 1);
%! assert (v, [1; 2; 3]);

## Digraph: self-loop at source.  The looped node is already discovered when
## the self-loop is encountered; discovery result is unchanged.
%!test
%! G = digraph ([1 1 2], [1 2 3]);
%! v = dfsearch (G, 1);
%! assert (v, [1; 2; 3]);

## Weighted digraph: DFS ignores weights.
%!test
%! G = digraph ([1 2 3], [2 3 1], [7 5 9]);
%! v = dfsearch (G, 1);
%! assert (v, [1; 2; 3]);

## Siever-style 9-node digraph: deterministic DFS from node 1.
##   Edges: 1->2, 2->3, 3->2, 3->4, 4->5, 5->6, 5->9, 6->7, 7->8,
##          7->9, 8->7, 9->4
## Trace from 1 (neighbours ascending, recurse depth-first):
##   1 -> 2 -> 3 (2 is already discovered, back edge only to 2) ...
##     At 3 ascending out-neighbours are {2, 4}.  2 is discovered, skip recurse.
##     4 is undiscovered -> recurse:
##       4 -> 5 -> 6 -> 7 -> 8 (8 out-neighbour is 7, discovered -> skip);
##         finish 8. Back at 7 next out {8,9}; 9 undiscovered -> recurse;
##         9 -> 4 (4 discovered -> skip); finish 9; finish 7; finish 6; finish 5; finish 4;
##       finish 3; finish 2; finish 1.
## Discovery order: [1; 2; 3; 4; 5; 6; 7; 8; 9]
%!test
%! s = [1 2 3 3 4 5 5 6 7 7 8 9];
%! t = [2 3 2 4 5 6 9 7 8 9 7 4];
%! G = digraph (s, t);
%! v = dfsearch (G, 1);
%! assert (v, [1; 2; 3; 4; 5; 6; 7; 8; 9]);

## Multigraph digraph: parallel edges collapse to a single DFS neighbour.
%!test
%! G = digraph ([1 1 1 2], [2 2 3 3], "multigraph");
%! v = dfsearch (G, 1);
%! assert (v, [1; 2; 3]);

## Digraph value semantics: dfsearch does not mutate G.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! dfsearch (G, 1);
%! assert (numnodes (G), 3);
%! assert (numedges (G), 3);
%! assert (class (G), "digraph");

## Dot-notation dispatch works on digraph.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! v = G.dfsearch (1);
%! assert (v, [1; 2; 3]);

## -------------------- graph (undirected) tests --------------------

## Default single-node graph: DFS from the sole node.
%!test
%! G = graph (1);
%! v = dfsearch (G, 1);
%! assert (v, 1);

## Edgeless undirected graph: DFS returns only the source.
%!test
%! G = graph (5);
%! v = dfsearch (G, 3);
%! assert (v, 3);

## Simple undirected path 1--2--3: DFS from 1 gives [1; 2; 3].
%!test
%! G = graph ([1 2], [2 3]);
%! v = dfsearch (G, 1);
%! assert (v, [1; 2; 3]);

## DFS from middle node of a path: neighbours visited in ascending order.
%!test
%! G = graph ([1 2], [2 3]);
%! v = dfsearch (G, 2);
%! assert (v, [2; 1; 3]);

## Star graph: centre visits leaves in ascending order.
%!test
%! G = graph ([1 1 1 1], [2 3 4 5]);
%! v = dfsearch (G, 1);
%! assert (v, [1; 2; 3; 4; 5]);

## DFS from a leaf of a star graph.
%!test
%! G = graph ([1 1 1 1], [2 3 4 5]);
%! v = dfsearch (G, 3);
%! assert (v, [3; 1; 2; 4; 5]);

## Disconnected undirected graph: DFS only visits the connected component.
##   Components: {1,2,3} and {4,5}.  DFS from 1 omits 4,5.
%!test
%! G = graph ([1 2 4], [2 3 5]);
%! v = dfsearch (G, 1);
%! assert (v, [1; 2; 3]);

## Undirected cycle 1-2-3-4-1: DFS from 1 goes 1 -> 2 -> 3 -> 4 -> (back to 1).
## Neighbours of 1 ascending: {2, 4}.  Recurse into 2 first.
##   At 2: {1, 3}.  1 discovered, recurse into 3.
##     At 3: {2, 4}.  2 discovered, recurse into 4.
##       At 4: {1, 3}.  1 discovered (back edge), 3 discovered (back edge).
## Discovery order: [1; 2; 3; 4].
%!test
%! G = graph ([1 2 3 4], [2 3 4 1]);
%! v = dfsearch (G, 1);
%! assert (v, [1; 2; 3; 4]);

## Siever-style 9-node undirected graph: DFS from node 1.
##   Simple (deduplicated) undirected edge set:
##     {1-2, 2-3, 3-4, 4-5, 5-6, 5-9, 6-7, 7-8, 7-9, 4-9}
## Trace from 1:
##   1 -> 2 -> 3 -> 4 (neighbours {3,5,9} -> 5 undiscovered next)
##         4 -> 5 (neighbours {4,6,9}, 4 discovered -> 6)
##                5 -> 6 (neighbours {5,7}, 5 discovered -> 7)
##                   6 -> 7 (neighbours {6,8,9}, 6 discovered -> 8)
##                      7 -> 8 (neighbours {7}, discovered; finish 8)
##                      back at 7, next {9}: 9 undiscovered -> recurse
##                         7 -> 9 (neighbours {4,5,7}, all discovered; finish 9)
##                      finish 7; finish 6; finish 5
##         back at 4, next {9}: 9 already discovered; finish 4;
##   finish 3; finish 2; finish 1.
## Discovery order: [1; 2; 3; 4; 5; 6; 7; 8; 9]
%!test
%! s = [1 2 3 4 5 5 6 7 7 4];
%! t = [2 3 4 5 6 9 7 8 9 9];
%! G = graph (s, t);
%! v = dfsearch (G, 1);
%! assert (v, [1; 2; 3; 4; 5; 6; 7; 8; 9]);

## Undirected self-loop doesn't cause re-visit.
%!test
%! G = graph ([1 1 2], [1 2 3]);
%! v = dfsearch (G, 1);
%! assert (v, [1; 2; 3]);

## Weighted undirected graph: DFS ignores weights.
%!test
%! G = graph ([1 2 3], [2 3 1], [7 5 9]);
%! v = dfsearch (G, 1);
%! assert (v, [1; 2; 3]);

## Named undirected graph + string source.
%!test
%! G = graph ([1 2 3], [2 3 1], [], {"a", "b", "c"});
%! v = dfsearch (G, "b");
%! assert (v, [2; 1; 3]);

## Graph value semantics: dfsearch does not mutate G.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! dfsearch (G, 1);
%! assert (numnodes (G), 3);
%! assert (numedges (G), 3);
%! assert (class (G), "graph");

## Dot-notation dispatch works on graph.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! v = G.dfsearch (1);
%! assert (v, [1; 2; 3]);

## DFS result is column vector for graph too.
%!test
%! G = graph ([1 2], [2 3]);
%! v = dfsearch (G, 1);
%! assert (size (v), [3, 1]);

## -------------------- error cases --------------------

## Error: non-graph first argument routes through the free-function guard.
%!error <G must be a graph or digraph> dfsearch (3, 1)
%!error <G must be a graph or digraph> dfsearch ("hello", 1)
%!error <G must be a graph or digraph> dfsearch (sparse (2, 2), 1)

## Error: nargin mismatch.
%!error <Invalid call> dfsearch ()
%!error <Invalid call> dfsearch (digraph (3))

## Error: numeric source out of range (too large).
%!error <invalid node index> dfsearch (digraph (3), 4)

## Error: numeric source out of range (zero).
%!error <invalid node index> dfsearch (digraph (3), 0)

## Error: numeric source non-integer.
%!error <invalid node index> dfsearch (digraph (3), 1.5)

## Error: non-existent node name.
%!error <not found> ...
%!   dfsearch (digraph ([1 2], [2 3], [], {"a","b","c"}), "z")

## Error: node name given but graph has no names.
%!error <no node names|not found> dfsearch (digraph (3), "foo")

## Error: non-scalar numeric source.
%!error <scalar> dfsearch (digraph (3), [1 2])

## Error: non-scalar numeric source on graph.
%!error <scalar> dfsearch (graph (3), [1 2])

## Error: multi-element cellstr source.
%!error <scalar> ...
%!   dfsearch (digraph ([1 2], [2 3], [], {"a","b","c"}), {"a","b"})

## Error: graph with no names + name source.
%!error <no node names|not found> dfsearch (graph (3), "foo")

## ------------------------------------------------------------------
## Events option -- dfsearch (G, s, events)
## ------------------------------------------------------------------

## Single char 'discovernode' equals the default 2-arg dfsearch result.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! v2 = dfsearch (G, 1);
%! v3 = dfsearch (G, 1, "discovernode");
%! assert (v2, v3);
%! assert (v3, [1; 2; 3]);

## Single char 'finishnode': DFS finish order on a 3-cycle is [3; 2; 1]
## (post-order; reverse of discovery for this chain).
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! v = dfsearch (G, 1, "finishnode");
%! assert (v, [3; 2; 1]);

## Single char 'startnode': just the starting node.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! v = dfsearch (G, 1, "startnode");
%! assert (v, 1);

## Single char 'edgetonew' returns the DFS tree edges on a 3-cycle.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! E = dfsearch (G, 1, "edgetonew");
%! assert (E, [1 2; 2 3]);

## 'edgetodiscovered' on a 3-cycle 1->2->3->1.
## At node 3, edge (3,1) targets node 1 which is still on the DFS stack
## (discovered, not yet finished) -> edgetodiscovered (back edge).
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! E = dfsearch (G, 1, "edgetodiscovered");
%! assert (E, [3 1]);

## 'edgetofinished' on 3-cycle is empty (all targets still on stack).
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! E = dfsearch (G, 1, "edgetofinished");
%! assert (size (E), [0, 2]);
%! assert (class (E), "double");

## Triangle 1->{2,3}, 2->3.
## Trace from 1: 1 -> 2 -> 3 (leaf); finish 3; finish 2; back at 1, edge (1,3):
## 3 is finished -> edgetofinished (1,3).
%!test
%! G = digraph ([1 1 2], [2 3 3]);
%! E = dfsearch (G, 1, "edgetofinished");
%! assert (E, [1 3]);

## Same triangle: edgetodiscovered should be empty (no back edges).
%!test
%! G = digraph ([1 1 2], [2 3 3]);
%! E = dfsearch (G, 1, "edgetodiscovered");
%! assert (size (E), [0, 2]);

## No edge events on a 3-node chain -> 0x2 empty matrix.
%!test
%! G = digraph ([1 1], [2 3]);
%! E = dfsearch (G, 1, "edgetodiscovered");
%! assert (size (E), [0, 2]);

## 'allevents' returns a struct with fields Event, Node, Edge.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! T = dfsearch (G, 1, "allevents");
%! assert (isstruct (T));
%! assert (sort (fieldnames (T)), sort ({"Event"; "Node"; "Edge"}));

## 'allevents' on 3-cycle 1->2->3->1 produces the complete event log.
## Trace:
##   startnode 1; discovernode 1;
##   edgetonew (1,2); discovernode 2;
##   edgetonew (2,3); discovernode 3;
##   edgetodiscovered (3,1);
##   finishnode 3; finishnode 2; finishnode 1.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! T = dfsearch (G, 1, "allevents");
%! expected_Event = {"startnode"; "discovernode"; "edgetonew"; ...
%!                   "discovernode"; "edgetonew"; "discovernode"; ...
%!                   "edgetodiscovered"; "finishnode"; "finishnode"; ...
%!                   "finishnode"};
%! assert (T.Event, expected_Event);
%! assert (T.Node, [1; 1; 0; 2; 0; 3; 0; 3; 2; 1]);
%! assert (T.Edge, [0 0; 0 0; 1 2; 0 0; 2 3; 0 0; 3 1; 0 0; 0 0; 0 0]);

## 'allevents' on singleton: startnode, discovernode, finishnode.
%!test
%! G = digraph (1);
%! T = dfsearch (G, 1, "allevents");
%! assert (T.Event, {"startnode"; "discovernode"; "finishnode"});
%! assert (T.Node, [1; 1; 1]);
%! assert (T.Edge, [0 0; 0 0; 0 0]);

## Cellstr events selects a subset and returns struct preserving event order.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! T = dfsearch (G, 1, {"discovernode", "edgetonew"});
%! assert (isstruct (T));
%! assert (T.Event, {"discovernode"; "edgetonew"; "discovernode"; ...
%!                   "edgetonew"; "discovernode"});
%! assert (T.Node, [1; 0; 2; 0; 3]);
%! assert (T.Edge, [0 0; 1 2; 0 0; 2 3; 0 0]);

## Single-element cellstr still returns a struct (not a vector).
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! T = dfsearch (G, 1, {"discovernode"});
%! assert (isstruct (T));
%! assert (T.Event, {"discovernode"; "discovernode"; "discovernode"});
%! assert (T.Node, [1; 2; 3]);
%! assert (T.Edge, [0 0; 0 0; 0 0]);

## Cellstr with an event that never fires -> empty struct fields (right shape).
%!test
%! G = digraph ([1 1], [2 3]);  # tree; no back/cross edges
%! T = dfsearch (G, 1, {"edgetofinished"});
%! assert (isstruct (T));
%! assert (size (T.Event), [0, 1]);
%! assert (size (T.Node), [0, 1]);
%! assert (size (T.Edge), [0, 2]);

## Tie-break: edgetonew order follows ascending node index.
%!test
%! G = digraph ([1 1 1], [4 2 3]);
%! E = dfsearch (G, 1, "edgetonew");
%! assert (E, [1 2; 1 3; 1 4]);

## Tie-break: discovernode with multi fan-out (leaves) gives ascending order.
%!test
%! G = digraph ([1 1 1], [4 2 3]);
%! v = dfsearch (G, 1, "discovernode");
%! assert (v, [1; 2; 3; 4]);

## Reachability: events only include reachable nodes.
%!test
%! G = digraph ([1 2 4], [2 3 5]);
%! T = dfsearch (G, 1, "allevents");
%! nodes_seen = unique (T.Node(T.Node > 0));
%! assert (nodes_seen, [1; 2; 3]);

## Node-name source still yields numeric indices in the event log.
%!test
%! G = digraph ([1 2 3], [2 3 1], [], {"a", "b", "c"});
%! v = dfsearch (G, "a", "discovernode");
%! assert (v, [1; 2; 3]);

## Dot-notation dispatch with 3 args on digraph.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! v = G.dfsearch (1, "finishnode");
%! assert (v, [3; 2; 1]);

## 3-arg on undirected graph: 'finishnode' order on a path 1-2-3 is [3; 2; 1].
## DFS goes 1 -> 2 -> 3, then unwinds finishing 3, 2, 1 in that order.
%!test
%! G = graph ([1 2], [2 3]);
%! v = dfsearch (G, 1, "finishnode");
%! assert (v, [3; 2; 1]);

## 3-arg on undirected graph: back edge to parent in an undirected path fires
## edgetodiscovered.
## Trace on path 1-2-3 from 1:
##   At 2, edge (2,1) -> 1 on stack -> edgetodiscovered (2,1).
##   At 3, edge (3,2) -> 2 on stack -> edgetodiscovered (3,2).
%!test
%! G = graph ([1 2], [2 3]);
%! E = dfsearch (G, 1, "edgetodiscovered");
%! assert (E, [2 1; 3 2]);

## 3-arg on undirected graph: edgetofinished is empty for this path.
%!test
%! G = graph ([1 2], [2 3]);
%! E = dfsearch (G, 1, "edgetofinished");
%! assert (size (E), [0, 2]);

## 3-arg on undirected graph: 'allevents' returns struct.
%!test
%! G = graph ([1 2], [2 3]);
%! T = dfsearch (G, 1, "allevents");
%! assert (isstruct (T));
%! assert (sort (fieldnames (T)), sort ({"Event"; "Node"; "Edge"}));
%! ## Source node present in Event log.
%! assert (any (T.Node == 1));

## 3-arg: 'edgetonew' on undirected tree gives tree edges.
%!test
%! G = graph ([1 2], [2 3]);
%! E = dfsearch (G, 1, "edgetonew");
%! assert (E, [1 2; 2 3]);

## 3-arg: dot-notation dispatch on graph.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! v = G.dfsearch (1, "discovernode");
%! assert (v, [1; 2; 3]);

## 3-arg: return type for node events is a numeric column.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! v = dfsearch (G, 1, "discovernode");
%! assert (iscolumn (v));
%! assert (class (v), "double");

## 3-arg: return type for edge events is an Nx2 double matrix.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! E = dfsearch (G, 1, "edgetonew");
%! assert (size (E, 2), 2);
%! assert (class (E), "double");

## 3-arg: allevents preserves consistency with single-event queries.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! T = dfsearch (G, 1, "allevents");
%! v_disc = T.Node(strcmp (T.Event, "discovernode"));
%! assert (v_disc, dfsearch (G, 1, "discovernode"));
%! v_fin = T.Node(strcmp (T.Event, "finishnode"));
%! assert (v_fin, dfsearch (G, 1, "finishnode"));
%! E_new = T.Edge(strcmp (T.Event, "edgetonew"), :);
%! assert (E_new, dfsearch (G, 1, "edgetonew"));

## 3-arg on digraph: self-loop at source becomes edgetodiscovered.
## When processing node 1, edge (1,1) targets node 1 which is in the
## 'discovered' state (on the DFS stack, not yet finished).
%!test
%! G = digraph ([1 1 2], [1 2 3]);
%! T = dfsearch (G, 1, "allevents");
%! has_self_etd = false;
%! for i = 1:numel (T.Event)
%!   if (strcmp (T.Event{i}, "edgetodiscovered") ...
%!       && isequal (T.Edge(i, :), [1 1]))
%!     has_self_etd = true;
%!   endif
%! endfor
%! assert (has_self_etd);

## Forward edge: 1->2, 2->3, 1->3.  DFS trace from 1:
##   1 -> 2 -> 3 (finish 3, finish 2); back at 1, edge (1,3): 3 finished ->
##   edgetofinished (1,3).  This is the classic "forward edge" case.
%!test
%! G = digraph ([1 2 1], [2 3 3]);
%! E = dfsearch (G, 1, "edgetofinished");
%! assert (E, [1 3]);

## Cross edge: two sibling subtrees with a cross edge.
## 1->2, 1->3, 3->2.  DFS from 1 neighbours {2,3}:
##   1 -> 2 (leaf; finish 2); back at 1, edge (1,3): undiscovered -> recurse;
##   3 -> 2 (2 is finished -> edgetofinished (3,2)); finish 3; finish 1.
%!test
%! G = digraph ([1 1 3], [2 3 2]);
%! E = dfsearch (G, 1, "edgetofinished");
%! assert (E, [3 2]);

## Error: unknown event name as char.
%!error <unknown event|invalid event> ...
%!   dfsearch (digraph ([1 2], [2 3]), 1, "bogus")

## Error: unknown event name inside cellstr.
%!error <unknown event|invalid event> ...
%!   dfsearch (digraph ([1 2], [2 3]), 1, {"discovernode", "bogus"})

## Error: non-char non-cellstr events argument (numeric).
%!error <events must be|character|cell array of strings> ...
%!   dfsearch (digraph ([1 2], [2 3]), 1, 42)

## Error: non-char non-cellstr events argument (struct).
%!error <events must be|character|cell array of strings> ...
%!   dfsearch (digraph ([1 2], [2 3]), 1, struct ())

## Error: 4 arguments with a trailing non-Name-Value token fails because a
## Name-Value parser needs pairs; the unrecognized leading name triggers
## an "unknown option" error.
%!error <unknown option|Name-Value|unknown name> ...
%!   dfsearch (digraph ([1 2], [2 3]), 1, "discovernode", "extra")

## ------------------------------------------------------------------
## US-T04: 'Restart' and 'EdgeColors' Name-Value options
## ------------------------------------------------------------------

## 'Restart', false is explicit default and matches the no-option result.
%!test
%! G = digraph ([1 4], [2 5]);
%! T1 = dfsearch (G, 1, "allevents");
%! T2 = dfsearch (G, 1, "allevents", "Restart", false);
%! assert (T1, T2);

## 'Restart', true continues across disconnected digraph components.
## Digraph: 1->2 and 4->5; node 3 isolated.  N=5.
%!test
%! G = digraph ([1 4], [2 5]);
%! T = dfsearch (G, 1, "allevents", "Restart", true);
%! nodes_seen = unique (T.Node(T.Node > 0));
%! assert (nodes_seen, [1; 2; 3; 4; 5]);

## 'Restart', true with single-char 'discovernode' returns all nodes.
%!test
%! G = digraph ([1 4], [2 5]);
%! v = dfsearch (G, 1, "discovernode", "Restart", true);
%! assert (iscolumn (v));
%! assert (sort (v), [1; 2; 3; 4; 5]);
%! assert (v(1), 1);
%! assert (v(2), 2);
%! assert (v(3:5), [3; 4; 5]);

## 'Restart', true fires one 'startnode' event per restart.
%!test
%! G = digraph ([1 4], [2 5]);
%! T = dfsearch (G, 1, "allevents", "Restart", true);
%! starts = T.Node(strcmp (T.Event, "startnode"));
%! ## Three components reachable: starts at 1, 3, 4.
%! assert (starts, [1; 3; 4]);

## 'Restart', true emits edges only within each component.
%!test
%! G = digraph ([1 4], [2 5]);
%! E = dfsearch (G, 1, "edgetonew", "Restart", true);
%! assert (sort (E, 1), [1 2; 4 5]);

## Restart on undirected graph: full node cover.
%!test
%! G = graph ([1 4], [2 5]);  # components {1,2}, {3}, {4,5}
%! v = dfsearch (G, 1, "discovernode", "Restart", true);
%! assert (sort (v), [1; 2; 3; 4; 5]);

## 'EdgeColors', true adds EdgeColor field to struct output.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! T = dfsearch (G, 1, "allevents", "EdgeColors", true);
%! assert (isstruct (T));
%! assert (isfield (T, "EdgeColor"));
%! assert (iscell (T.EdgeColor));
%! assert (size (T.EdgeColor, 2), 1);
%! assert (numel (T.EdgeColor), numel (T.Event));

## 'EdgeColors': node events get empty-string tags.
%!test
%! G = digraph (1);
%! T = dfsearch (G, 1, "allevents", "EdgeColors", true);
%! assert (all (cellfun (@isempty, T.EdgeColor)));

## 'EdgeColors': tree edges tagged 'tree' on a 3-cycle.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! T = dfsearch (G, 1, "allevents", "EdgeColors", true);
%! tree_idx = strcmp (T.Event, "edgetonew");
%! tree_tags = T.EdgeColor(tree_idx);
%! assert (tree_tags, {"tree"; "tree"});

## 'EdgeColors': DFS back edge on 3-cycle -> 'back'.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! T = dfsearch (G, 1, "allevents", "EdgeColors", true);
%! idx_disc = strcmp (T.Event, "edgetodiscovered");
%! assert (T.EdgeColor(idx_disc), {"back"});

## 'EdgeColors': DFS forward edge on triangle 1->{2,3}, 2->3 -> 'forward'.
%! ## d[1]=1, d[2]=2, d[3]=3.  Edge (1,3) is edgetofinished, d[3] > d[1] ->
%! ## forward.
%!test
%! G = digraph ([1 1 2], [2 3 3]);
%! T = dfsearch (G, 1, "allevents", "EdgeColors", true);
%! idx_fin = strcmp (T.Event, "edgetofinished");
%! assert (T.EdgeColor(idx_fin), {"forward"});

## 'EdgeColors': DFS cross edge on 1->{2,3}, 3->2 -> 'cross'.
## d[1]=1, d[2]=2 (first child), d[3]=3.  Edge (3,2): d[2]=2 < d[3]=3 ->
## cross.
%!test
%! G = digraph ([1 1 3], [2 3 2]);
%! T = dfsearch (G, 1, "allevents", "EdgeColors", true);
%! idx_fin = strcmp (T.Event, "edgetofinished");
%! assert (T.EdgeColor(idx_fin), {"cross"});

## Both options combined.
%!test
%! G = digraph ([1 4], [2 5]);
%! T = dfsearch (G, 1, "allevents", "Restart", true, "EdgeColors", true);
%! assert (isfield (T, "EdgeColor"));
%! nodes_seen = unique (T.Node(T.Node > 0));
%! assert (nodes_seen, [1; 2; 3; 4; 5]);

## Both options in reverse order.
%!test
%! G = digraph ([1 4], [2 5]);
%! T = dfsearch (G, 1, "allevents", "EdgeColors", true, "Restart", true);
%! assert (isfield (T, "EdgeColor"));

## 'EdgeColors', true with cellstr events.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! T = dfsearch (G, 1, {"edgetonew", "edgetodiscovered"}, "EdgeColors", true);
%! assert (isfield (T, "EdgeColor"));

## 'EdgeColors', true with cellstr selecting only node events.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! T = dfsearch (G, 1, {"discovernode"}, "EdgeColors", true);
%! assert (isfield (T, "EdgeColor"));
%! assert (all (cellfun (@isempty, T.EdgeColor)));

## Dot-notation dispatch accepts Name-Value pairs on digraph.
%!test
%! G = digraph ([1 4], [2 5]);
%! v = G.dfsearch (1, "discovernode", "Restart", true);
%! assert (sort (v), [1; 2; 3; 4; 5]);

## Dot-notation dispatch accepts Name-Value pairs on graph.
%!test
%! G = graph ([1 4], [2 5]);
%! v = G.dfsearch (1, "discovernode", "Restart", true);
%! assert (sort (v), [1; 2; 3; 4; 5]);

## 'Restart' is case-insensitive on the name.
%!test
%! G = digraph ([1 4], [2 5]);
%! v = dfsearch (G, 1, "discovernode", "restart", true);
%! assert (sort (v), [1; 2; 3; 4; 5]);

## 'EdgeColors' is case-insensitive on the name.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! T = dfsearch (G, 1, "allevents", "edgecolors", true);
%! assert (isfield (T, "EdgeColor"));

## Error: EdgeColors=true on single-char *node* event is rejected.
%!error <EdgeColors requires|allevents|cell array> ...
%!   dfsearch (digraph ([1 2], [2 3]), 1, "discovernode", "EdgeColors", true)

## Error: unknown Name.
%!error <unknown option|unknown name> ...
%!   dfsearch (digraph ([1 2], [2 3]), 1, "allevents", "Bogus", true)

## Error: odd Name-Value arg count (missing value).
%!error <Name-Value|missing value|requires> ...
%!   dfsearch (digraph ([1 2], [2 3]), 1, "allevents", "Restart")

## Error: Restart value must be scalar logical.
%!error <Restart.*logical|Restart.*scalar|logical scalar> ...
%!   dfsearch (digraph ([1 2], [2 3]), 1, "allevents", "Restart", "yes")

## Error: EdgeColors value must be scalar logical.
%!error <EdgeColors.*logical|EdgeColors.*scalar|logical scalar> ...
%!   dfsearch (digraph ([1 2], [2 3]), 1, "allevents", "EdgeColors", "yes")

## Error: Name must be a char row vector.
%!error <Name|option name> ...
%!   dfsearch (digraph ([1 2], [2 3]), 1, "allevents", 7, true)
