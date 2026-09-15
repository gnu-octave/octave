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
## @deftypefn {} {@var{H} =} subgraph (@var{G}, @var{nodes})
## Return the subgraph of @var{G} induced by the node subset @var{nodes}.
##
## @var{G} must be a @code{graph} or @code{digraph} object.  @var{nodes}
## specifies which nodes of @var{G} are retained, and may be given as:
##
## @table @asis
## @item a numeric index or numeric vector
## Positive integer node indices in @code{1:numnodes (@var{G})}.  The
## entries must be unique.  The returned graph @var{H} has one node per
## entry of @var{nodes}, listed in the given order, so @code{nodes(i)}
## of @var{G} becomes node @code{i} of @var{H}.
## @item a character row vector
## Interpreted as a single node name; @var{G} must be a named graph.
## @item a cell array of character vectors
## Interpreted as a list of node names.  Every name must exist in
## @var{G}; names must be unique.  The order of @var{nodes} is preserved
## in @var{H}.
## @item a logical vector of length @code{numnodes (@var{G})}
## Select nodes where the mask is true.  The surviving nodes appear in
## their original order (i.e. the same order in which @code{find} would
## list them).
## @end table
##
## Only edges of @var{G} that have @emph{both} endpoints in @var{nodes}
## are retained in @var{H}.  Edge weights, node names, node-attribute
## columns, and edge-attribute columns are all carried over to @var{H}.
## For a multigraph digraph, every parallel edge between two surviving
## endpoints is preserved.
##
## The returned graph @var{H} has the same class as @var{G}; the
## @qcode{'multigraph'}, weight, and node-name flags are preserved.
## Value semantics: @var{G} is not modified.
##
## @example
## @group
## G = digraph ([1 2 3 4], [2 3 4 1]);
## H = subgraph (G, [1 2 3]);
## numnodes (H)                       # @result{} 3
## numedges (H)                       # @result{} 2
## H.Edges.EndNodes                   # @result{} [1 2; 2 3]
##
## G = graph ([1 2 3], [2 3 1], [], @{"a", "b", "c"@});
## H = subgraph (G, @{"a", "c"@});
## H.Nodes.Name                       # @result{} @{"a"; "c"@}
## numedges (H)                       # @result{} 1  (edge a-c)
## @end group
## @end example
##
## @seealso{graph, digraph, rmnode, reordernodes, numnodes, findnode}
## @end deftypefn

function H = subgraph (G, nodes)

  ## NOTE: When called with a graph or digraph first argument, Octave's
  ## classdef method dispatch runs the class-internal @code{subgraph}
  ## method and this free-function body is not reached.  This file
  ## exists both as a canonical documentation target (so @code{help
  ## subgraph} works outside the context of an instance) and as a
  ## fallback that gives a helpful error for non-graph inputs.

  if (nargin != 2)
    print_usage ();
  endif

  if (! isa (G, "graph") && ! isa (G, "digraph"))
    error ("Octave:invalid-input-arg", ...
           "subgraph: G must be a graph or digraph object");
  endif

  ## Defensive delegation through dot notation.
  H = G.subgraph (nodes);

endfunction


## ------------------------------------------------------------------
## BIST blocks
## ------------------------------------------------------------------

## ---------------- Basic induced subgraph: digraph ------------------

## Pick a contiguous prefix of an unnamed digraph.
%!test
%! G = digraph ([1 2 3 4], [2 3 4 1]);
%! H = subgraph (G, [1 2 3]);
%! assert (numnodes (H), 3);
%! ## Surviving original edges: 1->2, 2->3 (both endpoints in {1,2,3}).
%! assert (numedges (H), 2);
%! assert (H.Edges.EndNodes, [1 2; 2 3]);

## Pick a contiguous prefix of an unnamed graph.
%!test
%! G = graph ([1 2 3 4], [2 3 4 1]);
%! H = subgraph (G, [1 2 3]);
%! assert (numnodes (H), 3);
%! ## 4-cycle edges: {1,2}, {2,3}, {3,4}, {1,4}.  Both endpoints in
%! ## {1,2,3} for {1,2} and {2,3}.  Surviving edges: {1,2} and {2,3}.
%! assert (numedges (H), 2);
%! assert (H.Edges.EndNodes, [1 2; 2 3]);

## Pick a non-contiguous subset (numeric vector).
%!test
%! G = digraph ([1 2 3 4 5], [2 3 4 5 1]);
%! H = subgraph (G, [1 3 5]);
%! assert (numnodes (H), 3);
%! ## Surviving edges: 5->1 (both endpoints in set).
%! ## After compaction by keep order: old 1->new 1, old 3->new 2, old 5->new 3.
%! ## So 5->1 becomes 3->1.
%! assert (numedges (H), 1);
%! assert (H.Edges.EndNodes, [3 1]);

## Subgraph on one node.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! H = subgraph (G, 2);
%! assert (numnodes (H), 1);
%! assert (numedges (H), 0);

## Subgraph on all nodes is an identity (same edges, same order).
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! H = subgraph (G, [1 2 3]);
%! assert (numnodes (H), 3);
%! assert (numedges (H), 3);
%! assert (H.Edges.EndNodes, G.Edges.EndNodes);

## Subgraph on [] nodes is the empty graph of the same class.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! H = subgraph (G, []);
%! assert (class (H), "digraph");
%! assert (numnodes (H), 0);
%! assert (numedges (H), 0);

## Subgraph on {} nodes is the empty graph of the same class (named G).
%!test
%! G = digraph ([1 2], [2 3], [], {"a", "b", "c"});
%! H = subgraph (G, {});
%! assert (class (H), "digraph");
%! assert (numnodes (H), 0);
%! ## MATLAB parity: empty-named-graph Name is 0-by-1 cell.
%! assert (numel (H.Nodes.Name), 0);

## Subgraph reorders nodes: given order dictates new indexing.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! H = subgraph (G, [3 1 2]);
%! assert (numnodes (H), 3);
%! ## New mapping: new 1 = old 3, new 2 = old 1, new 3 = old 2.
%! ## Original edges (old): 1->2, 2->3, 3->1.
%! ## New edges: 2->3, 3->1, 1->2; lex-sorted by (src, dst): 1->2, 2->3, 3->1.
%! assert (numedges (H), 3);
%! assert (H.Edges.EndNodes, [1 2; 2 3; 3 1]);

## Subgraph reorders nodes (graph class): new edges still canonicalised
## as (min, max) pairs.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! H = subgraph (G, [3 1 2]);
%! ## New mapping: new 1 = old 3, new 2 = old 1, new 3 = old 2.
%! ## Edges (old): {1,2}, {2,3}, {1,3} -> new pairs (2,3), (3,1), (1,2)
%! ## canonicalised -> (2,3), (1,3), (1,2).  Lex sort -> (1,2), (1,3), (2,3).
%! assert (numnodes (H), 3);
%! assert (numedges (H), 3);
%! assert (H.Edges.EndNodes, [1 2; 1 3; 2 3]);

## ---------------- Name-based subgraph ------------------------------

## Pick named subset by cellstr.
%!test
%! G = digraph ([1 2 3], [2 3 1], [], {"a", "b", "c"});
%! H = subgraph (G, {"a", "c"});
%! assert (numnodes (H), 2);
%! assert (H.Nodes.Name, {"a"; "c"});
%! ## Only edge 3->1 (c->a) has both endpoints in {a, c}; in H a=1, c=2.
%! assert (numedges (H), 1);
%! assert (H.Edges.EndNodes, [2 1]);

## Single-name char row.
%!test
%! G = digraph ([1 2 3], [2 3 1], [], {"a", "b", "c"});
%! H = subgraph (G, "b");
%! assert (H.Nodes.Name, {"b"});
%! assert (numnodes (H), 1);
%! assert (numedges (H), 0);

## Cellstr order determines new node ordering (parity with numeric).
%!test
%! G = digraph ([1 2 3], [2 3 1], [], {"a", "b", "c"});
%! H = subgraph (G, {"c", "a", "b"});
%! assert (H.Nodes.Name, {"c"; "a"; "b"});
%! ## Same 3-cycle re-labelled: c=1, a=2, b=3; old c->a becomes 1->2,
%! ## a->b becomes 2->3, b->c becomes 3->1.
%! assert (H.Edges.EndNodes, [1 2; 2 3; 3 1]);

## Column-cell of names is accepted.
%!test
%! G = digraph ([1 2 3], [2 3 1], [], {"a", "b", "c"});
%! H = subgraph (G, {"a"; "c"});
%! assert (H.Nodes.Name, {"a"; "c"});

## ---------------- Logical mask form --------------------------------

## Logical row vector picks nodes where mask is true, preserving order.
%!test
%! G = digraph ([1 2 3 4], [2 3 4 1]);
%! H = subgraph (G, logical ([1 0 1 1]));
%! assert (numnodes (H), 3);
%! ## Surviving original nodes: 1, 3, 4 (mask-true positions, in order).
%! ## Surviving edges: 3->4, 4->1 -> compacted 2->3, 3->1.
%! assert (H.Edges.EndNodes, [2 3; 3 1]);

## Logical column vector equivalent.
%!test
%! G = digraph ([1 2 3 4], [2 3 4 1]);
%! H = subgraph (G, logical ([1; 0; 1; 1]));
%! assert (numnodes (H), 3);
%! assert (H.Edges.EndNodes, [2 3; 3 1]);

## All-false logical mask yields empty graph.
%!test
%! G = digraph ([1 2], [2 3]);
%! H = subgraph (G, false (1, 3));
%! assert (class (H), "digraph");
%! assert (numnodes (H), 0);

## All-true logical mask is identity.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! H = subgraph (G, true (1, 3));
%! assert (numnodes (H), 3);
%! assert (numedges (H), 3);
%! assert (H.Edges.EndNodes, G.Edges.EndNodes);

## ---------------- Value semantics ----------------------------------

## Original graph is unchanged after subgraph.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! H = subgraph (G, [1 2]);
%! assert (numnodes (G), 3);
%! assert (numedges (G), 3);
%! assert (numnodes (H), 2);

## ---------------- Node-attribute carryover -------------------------

## Node-attribute columns follow the keep order.
%!test
%! NT.Name = {"a"; "b"; "c"};
%! NT.Size = [10; 20; 30];
%! ET.EndNodes = [1 2; 2 3];
%! G = digraph (ET, NT);
%! H = subgraph (G, [3 1]);
%! assert (H.Nodes.Name, {"c"; "a"});
%! assert (H.Nodes.Size, [30; 10]);

## Node-attribute columns (graph class).
%!test
%! NT.Name = {"a"; "b"; "c"};
%! NT.Kind = {"x"; "y"; "z"};
%! ET.EndNodes = [1 2; 2 3];
%! G = graph (ET, NT);
%! H = subgraph (G, {"b", "c"});
%! assert (H.Nodes.Name, {"b"; "c"});
%! assert (H.Nodes.Kind, {"y"; "z"});

## ---------------- Edge-attribute carryover -------------------------

## Weighted digraph: Weight column follows surviving edges in new lex order.
%!test
%! G = digraph ([1 2 3 4], [2 3 4 1], [10 20 30 40]);
%! H = subgraph (G, [2 3 4]);
%! ## Surviving edges: old 2->3 (w=20), 3->4 (w=30).
%! ## After remap: new 2->3=old 2->3 -> new 1->2 (w=20),
##                 new 2->3=old 3->4 -> new 2->3 (w=30).
%! assert (H.Edges.EndNodes, [1 2; 2 3]);
%! assert (H.Edges.Weight, [20; 30]);

## Weighted graph: weights follow surviving edges.
%!test
%! G = graph ([1 2 3 4], [2 3 4 1], [10 20 30 40]);
%! H = subgraph (G, [1 2 3]);
%! ## 4-cycle edges: {1,2}=10, {2,3}=20, {3,4}=30, {1,4}=40.
%! ## Surviving (both endpoints in {1,2,3}): {1,2}=10, {2,3}=20.
%! ## After remap new 1,2,3 = old 1,2,3: {1,2}=10, {2,3}=20.
%! assert (H.Edges.EndNodes, [1 2; 2 3]);
%! assert (H.Edges.Weight, [10; 20]);

## Unweighted digraph: Edges has no Weight field.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! H = subgraph (G, [1 2 3]);
%! assert (! isfield (H.Edges, "Weight"));

## Extra edge-attribute columns survive and follow lex order.
%!test
%! ET.EndNodes = [1 2; 2 3; 3 1];
%! ET.Weight = [10; 20; 30];
%! ET.Tag = {"x"; "y"; "z"};
%! G = digraph (ET);
%! H = subgraph (G, [2 3]);
%! ## Surviving: 2->3 (w=20, Tag=y) only; new 2=old 3, new 1=old 2 ->
%! ## 1->2 (w=20, Tag=y).
%! assert (H.Edges.EndNodes, [1 2]);
%! assert (H.Edges.Weight, 20);
%! assert (H.Edges.Tag, {"y"});

## Extra edge-attribute columns with reorder (graph class).
%!test
%! ET.EndNodes = [1 2; 2 3; 1 3];
%! ET.Weight = [10; 20; 30];
%! ET.Tag = {"x"; "y"; "z"};
%! G = graph (ET);
%! ## G.Edges is stored in lex (min, max) order:
%! ##   row 1: (1,2) w=10 tag=x
%! ##   row 2: (1,3) w=30 tag=z
%! ##   row 3: (2,3) w=20 tag=y
%! H = subgraph (G, [3 1 2]);
%! ## idx_map: old 3->new 1, old 1->new 2, old 2->new 3.
%! ## Remap each stored edge:
%! ##   (1,2) -> (2,3)
%! ##   (1,3) -> (2,1) canon (1,2)
%! ##   (2,3) -> (3,1) canon (1,3)
%! ## Lex sort -> new row 1 (1,2) from stored row 2 (w=30, tag=z),
%! ##             new row 2 (1,3) from stored row 3 (w=20, tag=y),
%! ##             new row 3 (2,3) from stored row 1 (w=10, tag=x).
%! assert (H.Edges.EndNodes, [1 2; 1 3; 2 3]);
%! assert (H.Edges.Weight, [30; 20; 10]);
%! assert (H.Edges.Tag, {"z"; "y"; "x"});

## ---------------- Graph class symmetry -----------------------------

## Graph adj_ stays symmetric after subgraph.
%!test
%! G = graph ([1 2 3 4], [2 3 4 1]);
%! H = subgraph (G, [1 2 3]);
%! A = full (adjacency (H));
%! assert (A, A');

## Self-loops follow surviving nodes.
%!test
%! G = digraph ([1 2 2 3], [2 2 3 1]);
%! ## Edges: 1->2, 2->2 (self-loop), 2->3, 3->1.
%! H = subgraph (G, [2 3]);
%! ## Surviving (both endpoints in {2,3}): 2->2, 2->3.
%! ## After remap old 2,3 -> new 1,2: 1->1 (self-loop), 1->2.
%! assert (numnodes (H), 2);
%! assert (numedges (H), 2);
%! assert (H.Edges.EndNodes, [1 1; 1 2]);

## Self-loop on a dropped node is dropped.
%!test
%! G = digraph ([1 2 3 3], [2 3 3 1]);
%! ## Edges: 1->2, 2->3, 3->3 (self-loop), 3->1.
%! H = subgraph (G, [1 2]);
%! ## Surviving (both endpoints in {1,2}): only 1->2.
%! assert (numnodes (H), 2);
%! assert (numedges (H), 1);
%! assert (H.Edges.EndNodes, [1 2]);

## ---------------- Multigraph path (digraph) ------------------------

## Multigraph: parallel edges between surviving endpoints all survive.
%!test
%! G = digraph ([1 1 1 2], [2 2 2 3], "multigraph");
%! ## Three parallel 1->2 edges plus 2->3.
%! H = subgraph (G, [1 2]);
%! assert (numnodes (H), 2);
%! assert (numedges (H), 3);
%! assert (ismultigraph (H));
%! assert (H.Edges.EndNodes, [1 2; 1 2; 1 2]);

## Multigraph: parallel edges with dropped endpoint are removed.
%!test
%! G = digraph ([1 1 2 3], [2 2 3 1], "multigraph");
%! ## Edges: 1->2 (x2), 2->3, 3->1.
%! H = subgraph (G, [1 3]);
%! ## Surviving (both endpoints in {1,3}): 3->1 -> new 2->1.
%! assert (numnodes (H), 2);
%! assert (numedges (H), 1);
%! assert (H.Edges.EndNodes, [2 1]);
%! ## No duplicates remain, so ismultigraph is false even though the
%! ## storage mode is still multigraph (MATLAB parity with rmnode).
%! assert (ismultigraph (H), false);

## Multigraph weighted: weights follow their edges.
%!test
%! G = digraph ([1 1 2], [2 2 3], [10 20 30], "multigraph");
%! H = subgraph (G, [1 2]);
%! assert (numnodes (H), 2);
%! assert (numedges (H), 2);
%! assert (H.Edges.EndNodes, [1 2; 1 2]);
%! assert (sort (H.Edges.Weight), [10; 20]);

## Multigraph reorder: new lex order reflects the reindex.
%!test
%! G = digraph ([1 1 2], [2 2 3], [10 20 30], "multigraph");
%! ## Parallel 1->2 (x2), 2->3.
%! H = subgraph (G, [3 2 1]);
%! ## Remap: new 1=old 3, new 2=old 2, new 3=old 1.
%! ## Edges become 3->2 (x2, from old 1->2), 2->1 (from old 2->3).
%! ## Lex sort: (2,1), (3,2), (3,2).
%! assert (numedges (H), 3);
%! assert (H.Edges.EndNodes, [2 1; 3 2; 3 2]);
%! assert (ismultigraph (H));

## ---------------- Class preservation -------------------------------

## Return class: digraph -> digraph.
%!test
%! G = digraph ([1 2], [2 3]);
%! H = subgraph (G, [1 2]);
%! assert (isa (H, "digraph"));
%! assert (! isa (H, "graph"));

## Return class: graph -> graph.
%!test
%! G = graph ([1 2], [2 3]);
%! H = subgraph (G, [1 2]);
%! assert (isa (H, "graph"));
%! assert (! isa (H, "digraph"));

## Return class: graph preserves weighted flag.
%!test
%! G = graph ([1 2 3], [2 3 1], [10 20 30]);
%! H = subgraph (G, [1 2]);
%! assert (isfield (H.Edges, "Weight"));

## ---------------- Dot-notation dispatch ----------------------------

## G.subgraph(idx) works via classdef method.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! H = G.subgraph ([1 2]);
%! assert (numnodes (H), 2);

## G.subgraph(names) works via classdef method.
%!test
%! G = graph ([1 2], [2 3], [], {"a", "b", "c"});
%! H = G.subgraph ({"a", "b"});
%! assert (H.Nodes.Name, {"a"; "b"});

## ---------------- Siever-style fixture -----------------------------

## Pick a subset of a 9-node siever-style digraph.
%!test
%! s = [1 2 3 3 4 5 5 6 7 7 8 9];
%! t = [2 3 2 4 5 6 9 7 8 9 7 4];
%! G = digraph (s, t);
%! H = subgraph (G, [1 2 3 4]);
%! ## Surviving edges (both endpoints in {1,2,3,4}):
%! ## 1->2, 2->3, 3->2, 3->4.
%! assert (numnodes (H), 4);
%! assert (numedges (H), 4);
%! assert (H.Edges.EndNodes, [1 2; 2 3; 3 2; 3 4]);

## ---------------- Error handling -----------------------------------

## Out-of-range numeric index.
%!error <invalid node index> ...
%! G = digraph ([1 2], [2 3]);
%! subgraph (G, 5);

## Zero index.
%!error <invalid node index> ...
%! G = digraph ([1 2], [2 3]);
%! subgraph (G, 0);

## Negative index.
%!error <invalid node index> ...
%! G = digraph ([1 2], [2 3]);
%! subgraph (G, -1);

## Non-integer index.
%!error <invalid node index> ...
%! G = digraph ([1 2], [2 3]);
%! subgraph (G, 1.5);

## Inf index.
%!error <invalid node index> ...
%! G = digraph ([1 2], [2 3]);
%! subgraph (G, Inf);

## Duplicate numeric index.
%!error <unique> ...
%! G = digraph ([1 2 3], [2 3 1]);
%! subgraph (G, [1 1 2]);

## Duplicate name in cellstr.
%!error <unique> ...
%! G = digraph ([1 2], [2 3], [], {"a", "b", "c"});
%! subgraph (G, {"a", "a"});

## Name not present.
%!error <not found> ...
%! G = digraph ([1 2], [2 3], [], {"a", "b", "c"});
%! subgraph (G, "z");

## Name given but graph is unnamed.
%!error <no node names> ...
%! G = digraph (3);
%! subgraph (G, "x");

## Logical mask wrong length.
%!error <logical mask> ...
%! G = digraph (3);
%! subgraph (G, logical ([1 0 1 1]));

## Non-graph first argument.
%!error <graph or digraph> ...
%! subgraph (42, 1);

## Non-graph first argument (string).
%!error <graph or digraph> ...
%! subgraph ("hello", 1);

## nargin < 2.
%!error <Invalid call> ...
%! subgraph (digraph ([1 2], [2 3]));
