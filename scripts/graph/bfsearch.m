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
## @deftypefn {} {@var{v} =} bfsearch (@var{G}, @var{s})
## Perform a breadth-first search of the graph @var{G} starting at node
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
## matches MATLAB's documented BFS traversal order and ensures the
## return value is deterministic.
##
## Self-loops are traversed trivially (the looped node is already
## visited at the moment the self-loop would be considered).  Parallel
## edges in a multigraph are collapsed for the purpose of neighbour
## enumeration -- BFS visits each neighbour at most once regardless of
## how many parallel edges connect it to the current node.
##
## @var{v} starts with @var{s} and contains only the nodes reachable
## from @var{s}.  Nodes in other connected components (or, for a
## digraph, nodes not reachable by following out-edges) are omitted.
## See @code{help bfsearch} and the @qcode{'Restart'} option of future
## revisions to include isolated components.
##
## @example
## @group
## G = graph ([1 2 3 3 4 5 5 6 7 7 8 9], ...
##            [2 3 2 4 5 6 9 7 8 9 7 4]);
## bfsearch (G, 1)
##    @result{} 1
##       2
##       3
##       4
##       5
##       9
##       6
##       7
##       8
## @end group
## @end example
##
## @seealso{graph, digraph, dfsearch, successors, predecessors, neighbors}
## @end deftypefn

function v = bfsearch (G, s)

  ## NOTE: When called with a graph or digraph object, Octave's classdef
  ## method dispatch runs the class-internal @code{bfsearch} method and
  ## this free-function body is not reached.  This file exists both as a
  ## canonical documentation target (so @code{help bfsearch} works
  ## outside the context of an instance) and as a fallback that gives a
  ## helpful error for non-graph inputs.

  if (nargin != 2)
    print_usage ();
  endif

  if (! isa (G, "graph") && ! isa (G, "digraph"))
    error ("Octave:invalid-input-arg", ...
           "bfsearch: G must be a graph or digraph object");
  endif

  ## Defensive delegation: if class dispatch ever skips past the free
  ## function (e.g. future subclassing edge cases) route back to the
  ## class method via dot notation, which is always class-dispatched.
  v = G.bfsearch (s);

endfunction


## ------------------------------------------------------------------
## BIST blocks
## ------------------------------------------------------------------

## -------------------- digraph tests --------------------

## Default single-node digraph: BFS from the sole node visits only that node.
%!test
%! G = digraph (1);
%! v = bfsearch (G, 1);
%! assert (v, 1);

## Empty edge set, source node in range: BFS returns just the source.
%!test
%! G = digraph (5);
%! v = bfsearch (G, 3);
%! assert (v, 3);

## Simple directed 3-cycle 1->2->3->1: BFS from 1 discovers [1; 2; 3].
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! v = bfsearch (G, 1);
%! assert (v, [1; 2; 3]);

## BFS from a different source on the same 3-cycle.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! v = bfsearch (G, 2);
%! assert (v, [2; 3; 1]);

## Tie-break: multiple unvisited out-neighbours visited in ascending index order.
%!test
%! G = digraph ([1 1 1], [4 2 3]);
%! v = bfsearch (G, 1);
%! assert (v, [1; 2; 3; 4]);

## BFS layers on a tree-like digraph.
##   1 -> {2, 3}; 2 -> {4, 5}; 3 -> {6, 7}
##   Expected BFS order: 1, 2, 3, 4, 5, 6, 7
%!test
%! G = digraph ([1 1 2 2 3 3], [2 3 4 5 6 7]);
%! v = bfsearch (G, 1);
%! assert (v, [1; 2; 3; 4; 5; 6; 7]);

## BFS tie-break + level ordering on a 7-node binary tree.
%!test
%! G = digraph ([1 1 2 2 3 3], [3 2 5 4 7 6]);  # unsorted input
%! v = bfsearch (G, 1);
%! assert (v, [1; 2; 3; 4; 5; 6; 7]);

## BFS returns a column vector.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! v = bfsearch (G, 1);
%! assert (size (v), [3, 1]);
%! assert (iscolumn (v));

## BFS result is class double.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! v = bfsearch (G, 1);
%! assert (class (v), "double");

## Node name input still returns numeric indices (MATLAB parity).
%!test
%! G = digraph ([1 2 3], [2 3 1], [], {"a", "b", "c"});
%! v = bfsearch (G, "b");
%! assert (class (v), "double");
%! assert (v, [2; 3; 1]);

## 1-element cellstr name input.
%!test
%! G = digraph ([1 2 3], [2 3 1], [], {"a", "b", "c"});
%! v = bfsearch (G, {"c"});
%! assert (v, [3; 1; 2]);

## Digraph where some nodes are unreachable from the source.
## Components: 1->2->3 (from 1) and 4->5 (disjoint).  BFS from 1 omits 4,5.
%!test
%! G = digraph ([1 2 4], [2 3 5]);
%! v = bfsearch (G, 1);
%! assert (v, [1; 2; 3]);

## Digraph with back edge: BFS from 1 on 1->2, 2->3, 3->1 still yields [1;2;3].
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! v = bfsearch (G, 1);
%! assert (v, [1; 2; 3]);

## Digraph: self-loop at source.  The looped node is already visited when
## the self-loop is encountered; result is unchanged.
%!test
%! G = digraph ([1 1 2], [1 2 3]);
%! v = bfsearch (G, 1);
%! assert (v, [1; 2; 3]);

## Weighted digraph: BFS ignores weights.
%!test
%! G = digraph ([1 2 3], [2 3 1], [7 5 9]);
%! v = bfsearch (G, 1);
%! assert (v, [1; 2; 3]);

## Siever-style 9-node digraph: deterministic BFS from node 1.
##   Edges: 1->2, 2->3, 3->2, 3->4, 4->5, 5->6, 5->9, 6->7, 7->8,
##          7->9, 8->7, 9->4
##   Expected BFS order from 1:  1, 2, 3, 4, 5, 6, 9, 7, 8
%!test
%! s = [1 2 3 3 4 5 5 6 7 7 8 9];
%! t = [2 3 2 4 5 6 9 7 8 9 7 4];
%! G = digraph (s, t);
%! v = bfsearch (G, 1);
%! assert (v, [1; 2; 3; 4; 5; 6; 9; 7; 8]);

## Multigraph digraph: parallel edges collapse to a single BFS neighbour.
%!test
%! G = digraph ([1 1 1 2], [2 2 3 3], "multigraph");
%! v = bfsearch (G, 1);
%! assert (v, [1; 2; 3]);

## Digraph value semantics: bfsearch does not mutate G.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! bfsearch (G, 1);
%! assert (numnodes (G), 3);
%! assert (numedges (G), 3);
%! assert (class (G), "digraph");

## Dot-notation dispatch works on digraph.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! v = G.bfsearch (1);
%! assert (v, [1; 2; 3]);

## -------------------- graph (undirected) tests --------------------

## Default single-node graph: BFS from the sole node.
%!test
%! G = graph (1);
%! v = bfsearch (G, 1);
%! assert (v, 1);

## Edgeless undirected graph: BFS returns only the source.
%!test
%! G = graph (5);
%! v = bfsearch (G, 3);
%! assert (v, 3);

## Simple undirected path 1--2--3: BFS from 1 gives [1; 2; 3].
%!test
%! G = graph ([1 2], [2 3]);
%! v = bfsearch (G, 1);
%! assert (v, [1; 2; 3]);

## BFS from middle node of a path: neighbours visited in ascending order.
%!test
%! G = graph ([1 2], [2 3]);
%! v = bfsearch (G, 2);
%! assert (v, [2; 1; 3]);

## Star graph: centre visits leaves in ascending order.
%!test
%! G = graph ([1 1 1 1], [2 3 4 5]);
%! v = bfsearch (G, 1);
%! assert (v, [1; 2; 3; 4; 5]);

## BFS from a leaf of a star graph.
%!test
%! G = graph ([1 1 1 1], [2 3 4 5]);
%! v = bfsearch (G, 3);
%! assert (v, [3; 1; 2; 4; 5]);

## Disconnected undirected graph: BFS only visits the connected component.
##   Components: {1,2,3} and {4,5}.  BFS from 1 omits 4,5.
%!test
%! G = graph ([1 2 4], [2 3 5]);
%! v = bfsearch (G, 1);
%! assert (v, [1; 2; 3]);

## Undirected cycle 1-2-3-4-1: BFS from 1 visits 2 and 4 first (ascending),
## then 3.
%!test
%! G = graph ([1 2 3 4], [2 3 4 1]);
%! v = bfsearch (G, 1);
%! assert (v, [1; 2; 4; 3]);

## Siever-style 9-node undirected graph: BFS from node 1.
##   Simple (deduplicated) undirected edge set:
##     {1-2, 2-3, 3-4, 4-5, 5-6, 5-9, 6-7, 7-8, 7-9, 4-9}
##   Neighbours of 1: {2};      BFS visits 2.
##   Neighbours of 2: {1, 3};   visits 3.
##   Neighbours of 3: {2, 4};   visits 4.
##   Neighbours of 4: {3, 5, 9}; visits 5, 9 (ascending).
##   Neighbours of 5: {4, 6, 9}; visits 6.
##   Neighbours of 9: {4, 5, 7}; visits 7.
##   Neighbours of 6: {5, 7};   none new.
##   Neighbours of 7: {6, 8, 9}; visits 8.
##   Expected: 1, 2, 3, 4, 5, 9, 6, 7, 8
%!test
%! s = [1 2 3 4 5 5 6 7 7 4];
%! t = [2 3 4 5 6 9 7 8 9 9];
%! G = graph (s, t);
%! v = bfsearch (G, 1);
%! assert (v, [1; 2; 3; 4; 5; 9; 6; 7; 8]);

## Undirected self-loop doesn't cause re-visit.
%!test
%! G = graph ([1 1 2], [1 2 3]);
%! v = bfsearch (G, 1);
%! assert (v, [1; 2; 3]);

## Weighted undirected graph: BFS ignores weights.
%!test
%! G = graph ([1 2 3], [2 3 1], [7 5 9]);
%! v = bfsearch (G, 1);
%! assert (v, [1; 2; 3]);

## Named undirected graph + string source.
%!test
%! G = graph ([1 2 3], [2 3 1], [], {"a", "b", "c"});
%! v = bfsearch (G, "b");
%! assert (v, [2; 1; 3]);

## Graph value semantics: bfsearch does not mutate G.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! bfsearch (G, 1);
%! assert (numnodes (G), 3);
%! assert (numedges (G), 3);
%! assert (class (G), "graph");

## Dot-notation dispatch works on graph.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! v = G.bfsearch (1);
%! assert (v, [1; 2; 4 - 1]);  # [1; 2; 3]

## BFS result is column vector for graph too.
%!test
%! G = graph ([1 2], [2 3]);
%! v = bfsearch (G, 1);
%! assert (size (v), [3, 1]);

## -------------------- error cases --------------------

## Error: non-graph first argument routes through the free-function guard.
%!error <G must be a graph or digraph> bfsearch (3, 1)
%!error <G must be a graph or digraph> bfsearch ("hello", 1)
%!error <G must be a graph or digraph> bfsearch (sparse (2, 2), 1)

## Error: nargin mismatch.
%!error <Invalid call> bfsearch ()
%!error <Invalid call> bfsearch (digraph (3))

## Error: numeric source out of range (too large).
%!error <invalid node index> bfsearch (digraph (3), 4)

## Error: numeric source out of range (zero).
%!error <invalid node index> bfsearch (digraph (3), 0)

## Error: numeric source non-integer.
%!error <invalid node index> bfsearch (digraph (3), 1.5)

## Error: non-existent node name.
%!error <not found> ...
%!   bfsearch (digraph ([1 2], [2 3], [], {"a","b","c"}), "z")

## Error: node name given but graph has no names.
%!error <no node names|not found> bfsearch (digraph (3), "foo")

## Error: non-scalar numeric source.
%!error <scalar> bfsearch (digraph (3), [1 2])

## Error: non-scalar numeric source on graph.
%!error <scalar> bfsearch (graph (3), [1 2])

## Error: multi-element cellstr source.
%!error <scalar> ...
%!   bfsearch (digraph ([1 2], [2 3], [], {"a","b","c"}), {"a","b"})

## Error: graph with no names + name source.
%!error <no node names|not found> bfsearch (graph (3), "foo")
