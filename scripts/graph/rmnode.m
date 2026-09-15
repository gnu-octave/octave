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
## @deftypefn {} {@var{H} =} rmnode (@var{G}, @var{nodes})
## Remove the specified nodes (and all of their incident edges) from the
## graph or digraph @var{G} and return the resulting graph @var{H}.
##
## @var{G} must be a @code{graph} or @code{digraph} object.  @var{nodes}
## identifies one or more nodes of @var{G} and may be given as:
##
## @table @asis
## @item a numeric index or numeric vector
## Positive integer node indices in @code{1:numnodes (@var{G})}.
## @item a character row vector
## Interpreted as a single node name; @var{G} must be a named graph.
## @item a cell array of character vectors
## Interpreted as a list of node names; every name must exist in
## @var{G}.  Shape is irrelevant; the list is flattened.
## @end table
##
## The removed nodes are deleted together with every edge that has at
## least one endpoint in the removed set.  Remaining nodes are reindexed
## compactly into @code{1:(numnodes (@var{G}) - k)}, where @var{k} is the
## number of removed nodes.  Node names, node-attribute columns, and
## edge-attribute columns are filtered to match.  For a multigraph, the
## parallel-edge storage is filtered the same way (duplicate edges on
## surviving endpoints are preserved).
##
## The returned graph @var{H} has the same class as @var{G}; the
## @qcode{'multigraph'}, weight, and node-name flags are preserved.
## Value semantics: @var{G} is not modified.
##
## @example
## @group
## G = digraph ([1 2 3 4], [2 3 4 1]);
## H = rmnode (G, 2);
## numnodes (H)                       # @result{} 3
## numedges (H)                       # @result{} 2
##
## G = graph ([1 2], [2 3], [], @{"a", "b", "c"@});
## H = rmnode (G, @{"a", "c"@});
## H.Nodes.Name                       # @result{} @{"b"@}
## numedges (H)                       # @result{} 0
## @end group
## @end example
##
## @seealso{graph, digraph, addnode, rmedge, addedge, numnodes, findnode}
## @end deftypefn

function H = rmnode (G, nodes)

  ## NOTE: When called with a graph or digraph first argument, Octave's
  ## classdef method dispatch runs the class-internal @code{rmnode}
  ## method and this free-function body is not reached.  This file
  ## exists both as a canonical documentation target (so @code{help
  ## rmnode} works outside the context of an instance) and as a fallback
  ## that gives a helpful error for non-graph inputs.

  if (nargin != 2)
    print_usage ();
  endif

  if (! isa (G, "graph") && ! isa (G, "digraph"))
    error ("Octave:invalid-input-arg", ...
           "rmnode: G must be a graph or digraph object");
  endif

  ## Defensive delegation through dot notation.
  H = G.rmnode (nodes);

endfunction


## ------------------------------------------------------------------
## BIST blocks
## ------------------------------------------------------------------

## ---------------- Basic removal: digraph ---------------------------

## Remove one node by numeric index from unnamed digraph.
%!test
%! G = digraph ([1 2 3 4], [2 3 4 1]);
%! H = rmnode (G, 2);
%! assert (numnodes (H), 3);
%! assert (numedges (H), 2);
%! ## Edges 1->2 and 2->3 touched node 2 and are gone.  Remaining edges
%! ## were 3->4 and 4->1 in original indices; after compaction nodes
%! ## (1, 3, 4) become (1, 2, 3), so edges become 2->3 and 3->1.
%! assert (H.Edges.EndNodes, [2 3; 3 1]);

## Remove one node by numeric index from unnamed graph.
%!test
%! G = graph ([1 2 3 4], [2 3 4 1]);
%! H = rmnode (G, 2);
%! assert (numnodes (H), 3);
%! ## 4-cycle edges are {1,2}, {2,3}, {3,4}, {1,4}.  Removing node 2
%! ## drops {1,2} and {2,3}; {3,4} and {1,4} survive.  After compaction
%! ## original nodes 1, 3, 4 become 1, 2, 3, so surviving edges are
%! ## {1,3} and {2,3} in lex (min,max) order.
%! assert (numedges (H), 2);
%! assert (H.Edges.EndNodes, [1 3; 2 3]);

## Remove multiple nodes at once (numeric vector).
%!test
%! G = digraph ([1 2 3 4 5], [2 3 4 5 1]);
%! H = rmnode (G, [2 4]);
%! assert (numnodes (H), 3);
%! ## Surviving nodes: 1, 3, 5 -> compacted to 1, 2, 3.  Surviving edge
%! ## is 5->1 (original) which becomes 3->1.
%! assert (H.Edges.EndNodes, [3 1]);

## Removing a single node returns a graph/digraph of the expected size.
%!test
%! G = digraph (5);
%! H = rmnode (G, 3);
%! assert (numnodes (H), 4);
%! assert (numedges (H), 0);

## Remove every node: result is the empty graph of the same class.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! H = rmnode (G, [1 2 3]);
%! assert (class (H), "digraph");
%! assert (numnodes (H), 0);
%! assert (numedges (H), 0);

## Remove zero nodes ([] input) is a no-op.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! H = rmnode (G, []);
%! assert (numnodes (H), numnodes (G));
%! assert (numedges (H), numedges (G));
%! assert (H.Edges.EndNodes, G.Edges.EndNodes);

## Remove zero nodes ({} input) is a no-op on a named graph.
%!test
%! G = digraph ([1 2], [2 3], [], {"a", "b", "c"});
%! H = rmnode (G, {});
%! assert (numnodes (H), 3);
%! assert (H.Nodes.Name, {"a"; "b"; "c"});

## ---------------- Name-based removal -------------------------------

## Remove one node by name from a named digraph.
%!test
%! G = digraph ([1 2 3], [2 3 1], [], {"a", "b", "c"});
%! H = rmnode (G, "b");
%! assert (numnodes (H), 2);
%! assert (H.Nodes.Name, {"a"; "c"});
%! ## Edges 1->2 and 2->3 touched b; only 3->1 survives.
%! assert (numedges (H), 1);
%! assert (H.Edges.EndNodes, [2 1]);

## Remove multiple nodes by cellstr.
%!test
%! G = digraph ([1 2 3], [2 3 1], [], {"a", "b", "c"});
%! H = rmnode (G, {"a", "c"});
%! assert (numnodes (H), 1);
%! assert (H.Nodes.Name, {"b"});
%! assert (numedges (H), 0);

## Remove every named node.
%!test
%! G = graph ([1 2], [2 3], [], {"a", "b", "c"});
%! H = rmnode (G, {"a", "b", "c"});
%! assert (numnodes (H), 0);
%! assert (numel (H.Nodes.Name), 0);

## Column-cell of names is accepted.
%!test
%! G = digraph ([1 2 3], [2 3 1], [], {"a", "b", "c"});
%! H = rmnode (G, {"a"; "b"});
%! assert (H.Nodes.Name, {"c"});

## ---------------- Value semantics ----------------------------------

## Original graph is unchanged after rmnode.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! H = rmnode (G, 2);
%! assert (numnodes (G), 3);
%! assert (numedges (G), 3);
%! assert (numnodes (H), 2);

## ---------------- Node-attribute filtering -------------------------

## Node-attribute columns follow surviving nodes.
%!test
%! NT.Name = {"a"; "b"; "c"};
%! NT.Size = [10; 20; 30];
%! ET.EndNodes = [1 2; 2 3];
%! G = digraph (ET, NT);
%! H = rmnode (G, "b");
%! assert (H.Nodes.Name, {"a"; "c"});
%! assert (H.Nodes.Size, [10; 30]);

## Node-attribute columns follow surviving nodes (graph class).
%!test
%! NT.Name = {"a"; "b"; "c"};
%! NT.Kind = {"x"; "y"; "z"};
%! ET.EndNodes = [1 2; 2 3];
%! G = graph (ET, NT);
%! H = rmnode (G, {"a", "c"});
%! assert (H.Nodes.Name, {"b"});
%! assert (H.Nodes.Kind, {"y"});

## ---------------- Edge-attribute filtering -------------------------

## Weighted digraph: Weight column is filtered to surviving edges.
%!test
%! G = digraph ([1 2 3 4], [2 3 4 1], [10 20 30 40]);
%! H = rmnode (G, 2);
%! ## Surviving original edges: 3->4 (w=30), 4->1 (w=40).  After
%! ## compaction: 2->3 (w=30), 3->1 (w=40).
%! assert (H.Edges.EndNodes, [2 3; 3 1]);
%! assert (H.Edges.Weight, [30; 40]);

## Unweighted digraph: Edges has no Weight field.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! H = rmnode (G, 1);
%! assert (! isfield (H.Edges, "Weight"));

## Extra edge-attribute columns survive filtering.
%!test
%! ET.EndNodes = [1 2; 2 3; 3 1];
%! ET.Weight = [10; 20; 30];
%! ET.Tag = {"x"; "y"; "z"};
%! G = digraph (ET);
%! H = rmnode (G, 2);
%! ## Surviving: 3->1 only -> new EndNodes = [2 1], Tag = {"z"}, W=30.
%! assert (H.Edges.EndNodes, [2 1]);
%! assert (H.Edges.Weight, 30);
%! assert (H.Edges.Tag, {"z"});

## ---------------- Graph class symmetry -----------------------------

## Graph adj_ stays symmetric after rmnode.
%!test
%! G = graph ([1 2 3 4], [2 3 4 1]);
%! H = rmnode (G, 3);
%! A = full (adjacency (H));
%! assert (A, A');

## Self-loops are removed if the node carrying them is removed.
%!test
%! G = digraph ([1 2 2 3], [2 2 3 1]);
%! ## Edges: 1->2, 2->2 (self-loop), 2->3, 3->1.
%! H = rmnode (G, 2);
%! ## All edges touch node 2 except 3->1.  So H has 1 edge.
%! assert (numnodes (H), 2);
%! assert (numedges (H), 1);
%! assert (H.Edges.EndNodes, [2 1]);

## Self-loop on a surviving node is preserved.
%!test
%! G = digraph ([1 2 3 3], [2 3 3 1]);
%! ## Edges: 1->2, 2->3, 3->3 (self-loop), 3->1.
%! H = rmnode (G, 2);
%! ## Survivors: 1 and 3 -> compacted to 1 and 2.
%! ## Surviving edges: 3->3 -> 2->2, 3->1 -> 2->1.
%! assert (numnodes (H), 2);
%! assert (numedges (H), 2);
%! assert (H.Edges.EndNodes, [2 1; 2 2]);

## ---------------- Multigraph path (digraph) ------------------------

## Multigraph: parallel edges on the removed node are dropped; the
## returned digraph is still multigraph-mode but has no duplicates left.
%!test
%! G = digraph ([1 1 2 3], [2 2 3 1], "multigraph");
%! ## Edges: 1->2 (x2), 2->3, 3->1.
%! H = rmnode (G, 2);
%! ## Survivors: nodes 1 and 3 -> compacted to 1 and 2.
%! ## Surviving edges: 3->1 -> 2->1.
%! assert (numnodes (H), 2);
%! assert (numedges (H), 1);
%! assert (H.Edges.EndNodes, [2 1]);
%! ## No duplicates remain, so ismultigraph is false even though the
%! ## storage mode is still multigraph (MATLAB parity).
%! assert (ismultigraph (H), false);

## Multigraph: parallel edges between two surviving nodes survive.
%!test
%! G = digraph ([1 1 1 2], [2 2 2 3], "multigraph");
%! ## Three parallel 1->2 edges plus 2->3.
%! H = rmnode (G, 3);
%! assert (numnodes (H), 2);
%! assert (numedges (H), 3);
%! assert (ismultigraph (H));
%! assert (H.Edges.EndNodes, [1 2; 1 2; 1 2]);

## Multigraph weighted: weights follow their edges.
%!test
%! G = digraph ([1 1 2], [2 2 3], [10 20 30], "multigraph");
%! H = rmnode (G, 3);
%! assert (numnodes (H), 2);
%! assert (numedges (H), 2);
%! assert (H.Edges.EndNodes, [1 2; 1 2]);
%! assert (sort (H.Edges.Weight), [10; 20]);

## ---------------- Class preservation -------------------------------

## Return class: digraph -> digraph.
%!test
%! G = digraph ([1 2], [2 3]);
%! H = rmnode (G, 1);
%! assert (isa (H, "digraph"));
%! assert (! isa (H, "graph"));

## Return class: graph -> graph.
%!test
%! G = graph ([1 2], [2 3]);
%! H = rmnode (G, 1);
%! assert (isa (H, "graph"));
%! assert (! isa (H, "digraph"));

## Return class: graph preserves weighted flag.
%!test
%! G = graph ([1 2 3], [2 3 1], [10 20 30]);
%! H = rmnode (G, 2);
%! assert (isfield (H.Edges, "Weight"));

## ---------------- Dot-notation dispatch ----------------------------

## G.rmnode(idx) works via classdef method.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! H = G.rmnode (2);
%! assert (numnodes (H), 2);

## G.rmnode(name) works via classdef method.
%!test
%! G = graph ([1 2], [2 3], [], {"a", "b", "c"});
%! H = G.rmnode ("b");
%! assert (H.Nodes.Name, {"a"; "c"});

## ---------------- Siever-style fixture -----------------------------

## Remove a middle node from a 9-node siever-style digraph.
%!test
%! s = [1 2 3 3 4 5 5 6 7 7 8 9];
%! t = [2 3 2 4 5 6 9 7 8 9 7 4];
%! G = digraph (s, t);
%! H = rmnode (G, 5);
%! ## 5 was involved in edges 4->5, 5->6, 5->9 -> 3 removed.
%! assert (numnodes (H), 8);
%! assert (numedges (H), numedges (G) - 3);

## ---------------- Error handling -----------------------------------

## Out-of-range numeric index.
%!error <invalid node index> ...
%! G = digraph ([1 2], [2 3]);
%! rmnode (G, 5);

## Zero index.
%!error <invalid node index> ...
%! G = digraph ([1 2], [2 3]);
%! rmnode (G, 0);

## Negative index.
%!error <invalid node index> ...
%! G = digraph ([1 2], [2 3]);
%! rmnode (G, -1);

## Non-integer index.
%!error <invalid node index> ...
%! G = digraph ([1 2], [2 3]);
%! rmnode (G, 1.5);

## Inf index.
%!error <invalid node index> ...
%! G = digraph ([1 2], [2 3]);
%! rmnode (G, Inf);

## Name not present.
%!error <not found> ...
%! G = digraph ([1 2], [2 3], [], {"a", "b", "c"});
%! rmnode (G, "z");

## Name given but graph is unnamed.
%!error <no node names> ...
%! G = digraph (3);
%! rmnode (G, "x");

## Unsupported type (logical).
%!error <numeric index array> ...
%! G = digraph (3);
%! rmnode (G, true);

## Non-graph first argument.
%!error <graph or digraph> ...
%! rmnode (42, 1);

## Non-graph first argument (string).
%!error <graph or digraph> ...
%! rmnode ("hello", 1);

## nargin < 2.
%!error <Invalid call> ...
%! rmnode (digraph ([1 2], [2 3]));
