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
## @deftypefn {} {@var{H} =} reordernodes (@var{G}, @var{order})
## Permute the nodes of the graph or digraph @var{G} according to
## @var{order} and return the reordered graph @var{H}.
##
## @var{G} must be a @code{graph} or @code{digraph} object.
## @var{order} describes a permutation of the nodes of @var{G} and may
## be given as:
##
## @table @asis
## @item a numeric index vector
## A permutation of @code{1:numnodes (@var{G})}.  Every index in
## @code{1:numnodes (@var{G})} must appear exactly once; duplicates and
## omissions are rejected.
## @item a cell array of character vectors
## A permutation of the node names of @var{G}.  Each name in
## @code{@var{G}.Nodes.Name} must appear in @var{order} exactly once;
## names not present in @var{G} are rejected.  The graph must have
## node names.
## @end table
##
## Node @code{i} of the returned graph @var{H} is node
## @code{@var{order}(i)} of the input graph @var{G}.  Consequently the
## adjacency matrix of @var{H} is
## @code{adjacency (@var{G})(@var{order}, @var{order})}.  Node names,
## node-attribute columns, edge endpoints, edge weights, and
## edge-attribute columns are renumbered so that the reordered graph
## is structurally identical to @var{G} (i.e., isomorphic) -- only the
## node indexing changes.  For a multigraph, parallel edges are
## preserved with their weights and edge-attribute rows intact.
##
## The returned graph @var{H} has the same class as @var{G}; the
## @qcode{'multigraph'} flag, weight flag, and node-name presence are
## preserved.  Value semantics: @var{G} is not modified.
##
## @example
## @group
## G = digraph ([1 2 3], [2 3 1]);
## H = reordernodes (G, [3 1 2]);
## numnodes (H)                       # @result{} 3
## numedges (H)                       # @result{} 3
## H.Edges.EndNodes                   # @result{} [1 2; 2 3; 3 1]
##
## G = graph ([1 2], [2 3], [], @{"a", "b", "c"@});
## H = reordernodes (G, @{"c", "a", "b"@});
## H.Nodes.Name                       # @result{} @{"c"; "a"; "b"@}
## @end group
## @end example
##
## @seealso{graph, digraph, subgraph, addnode, rmnode, addedge, rmedge}
## @end deftypefn

function H = reordernodes (G, order)

  ## NOTE: When called with a graph or digraph first argument, Octave's
  ## classdef method dispatch runs the class-internal
  ## @code{reordernodes} method and this free-function body is not
  ## reached.  This file exists both as a canonical documentation
  ## target (so @code{help reordernodes} works outside the context of
  ## an instance) and as a fallback that gives a helpful error for
  ## non-graph inputs.

  if (nargin != 2)
    print_usage ();
  endif

  if (! isa (G, "graph") && ! isa (G, "digraph"))
    error ("Octave:invalid-input-arg", ...
           "reordernodes: G must be a graph or digraph object");
  endif

  ## Defensive delegation through dot notation.
  H = G.reordernodes (order);

endfunction


## ------------------------------------------------------------------
## BIST blocks
## ------------------------------------------------------------------

## ---------------- Basic permutation: digraph -----------------------

## Numeric perm on a 3-cycle digraph.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! H = reordernodes (G, [3 1 2]);
%! assert (numnodes (H), 3);
%! assert (numedges (H), 3);
%! ## Old edges 1->2, 2->3, 3->1 under perm = [3 1 2].
%! ## Inverse perm: inv(3)=1, inv(1)=2, inv(2)=3, so inv = [2 3 1].
%! ## 1->2 becomes inv(1)->inv(2) = 2->3.
%! ## 2->3 becomes inv(2)->inv(3) = 3->1.
%! ## 3->1 becomes inv(3)->inv(1) = 1->2.
%! ## Sorted lex: [1 2; 2 3; 3 1].
%! assert (H.Edges.EndNodes, [1 2; 2 3; 3 1]);

## Numeric perm on a 3-cycle graph.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! H = reordernodes (G, [3 1 2]);
%! assert (numnodes (H), 3);
%! assert (numedges (H), 3);
%! ## Old edges {1,2}, {2,3}, {1,3}.  Under perm=[3 1 2] (inv=[2 3 1]):
%! ## {1,2} -> {inv(1),inv(2)} = {2,3}.
%! ## {2,3} -> {inv(2),inv(3)} = {3,1} -> canonical (1,3).
%! ## {1,3} -> {inv(1),inv(3)} = {2,1} -> canonical (1,2).
%! ## Sorted lex (min, max): [1 2; 1 3; 2 3].
%! assert (H.Edges.EndNodes, [1 2; 1 3; 2 3]);

## Identity permutation is a no-op.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! H = reordernodes (G, [1 2 3]);
%! assert (H.Edges.EndNodes, G.Edges.EndNodes);
%! assert (numnodes (H), numnodes (G));
%! assert (numedges (H), numedges (G));

## Reverse permutation on graph.
%!test
%! G = graph ([1 2], [2 3]);
%! H = reordernodes (G, [3 2 1]);
%! ## Inv of [3 2 1] is [3 2 1].  Edges {1,2}, {2,3} become
%! ## {3,2} -> {2,3} and {2,1} -> {1,2}.  Lex-sorted: [1 2; 2 3].
%! assert (H.Edges.EndNodes, [1 2; 2 3]);
%! assert (numnodes (H), 3);

## Column-vector perm is accepted.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! H = reordernodes (G, [3; 1; 2]);
%! assert (H.Edges.EndNodes, [1 2; 2 3; 3 1]);

## ---------------- Name-based permutation ---------------------------

## Cellstr perm on named digraph.
%!test
%! G = digraph ([1 2 3], [2 3 1], [], {"a", "b", "c"});
%! H = reordernodes (G, {"c", "a", "b"});
%! assert (H.Nodes.Name, {"c"; "a"; "b"});
%! ## Node 'c' is new 1, 'a' is new 2, 'b' is new 3.
%! ## Old edges a->b, b->c, c->a under new indexing: 2->3, 3->1, 1->2.
%! ## Lex-sorted: [1 2; 2 3; 3 1].
%! assert (H.Edges.EndNodes, [1 2; 2 3; 3 1]);

## Cellstr perm on named graph.
%!test
%! G = graph ([1 2], [2 3], [], {"a", "b", "c"});
%! H = reordernodes (G, {"c", "b", "a"});
%! assert (H.Nodes.Name, {"c"; "b"; "a"});
%! ## Old edges {a,b}={1,2}, {b,c}={2,3}.  Under new labels:
%! ## a is new 3, b is new 2, c is new 1.
%! ## {a,b} = {3,2} -> sorted (2,3); {b,c} = {2,1} -> sorted (1,2).
%! assert (H.Edges.EndNodes, [1 2; 2 3]);

## Column-cell of names is accepted.
%!test
%! G = digraph ([1 2 3], [2 3 1], [], {"a", "b", "c"});
%! H = reordernodes (G, {"c"; "a"; "b"});
%! assert (H.Nodes.Name, {"c"; "a"; "b"});

## ---------------- Weighted edges -----------------------------------

## Weighted digraph: weights follow edges after reorder.
%!test
%! G = digraph ([1 2 3], [2 3 1], [10 20 30]);
%! H = reordernodes (G, [3 1 2]);
%! ## Old edges (with weights): 1->2 (10), 2->3 (20), 3->1 (30).
%! ## Remapped: 2->3 (10), 3->1 (20), 1->2 (30).  Sorted lex:
%! ## 1->2 (30), 2->3 (10), 3->1 (20).
%! assert (H.Edges.EndNodes, [1 2; 2 3; 3 1]);
%! assert (H.Edges.Weight, [30; 10; 20]);

## Weighted graph: weights follow edges after reorder.
%!test
%! G = graph ([1 2 3], [2 3 1], [10 20 30]);
%! H = reordernodes (G, [3 1 2]);
%! ## Old edges {1,2}=10, {2,3}=20, {1,3}=30.  Inv=[2 3 1].
%! ## {1,2} -> {2,3} (10), {2,3} -> {3,1} = {1,3} (20),
##   ## {1,3} -> {2,1} = {1,2} (30).
%! ## Sorted (min, max): [1 2] w=30, [1 3] w=20, [2 3] w=10.
%! assert (H.Edges.EndNodes, [1 2; 1 3; 2 3]);
%! assert (H.Edges.Weight, [30; 20; 10]);

## Unweighted digraph: no Weight field after reorder.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! H = reordernodes (G, [3 1 2]);
%! assert (! isfield (H.Edges, "Weight"));

## ---------------- Edge-attribute filtering -------------------------

## Extra edge-attribute columns follow edges (digraph).
%!test
%! ET.EndNodes = [1 2; 2 3; 3 1];
%! ET.Weight = [10; 20; 30];
%! ET.Tag = {"x"; "y"; "z"};
%! G = digraph (ET);
%! H = reordernodes (G, [3 1 2]);
%! ## Sort order: new edges remapped and sorted lex -> original edge
%! ## 3->1 becomes 1->2 (first); 1->2 becomes 2->3 (second);
%! ## 2->3 becomes 3->1 (third).  So Tag permutes as {"z";"x";"y"}.
%! assert (H.Edges.EndNodes, [1 2; 2 3; 3 1]);
%! assert (H.Edges.Weight, [30; 10; 20]);
%! assert (H.Edges.Tag, {"z"; "x"; "y"});

## Extra edge-attribute columns follow edges (graph).
%!test
%! ET.EndNodes = [1 2; 2 3; 1 3];
%! ET.Weight = [10; 20; 30];
%! ET.Tag = {"x"; "y"; "z"};
%! G = graph (ET);
%! H = reordernodes (G, [3 1 2]);
%! ## Orig {1,2} -> {2,3} (x, 10); {2,3} -> {3,1} = {1,3} (y, 20);
%! ## {1,3} -> {2,1} = {1,2} (z, 30).  Sorted lex:
%! ## [1 2] w=30 t=z; [1 3] w=20 t=y; [2 3] w=10 t=x.
%! assert (H.Edges.EndNodes, [1 2; 1 3; 2 3]);
%! assert (H.Edges.Weight, [30; 20; 10]);
%! assert (H.Edges.Tag, {"z"; "y"; "x"});

## ---------------- Node-attribute filtering -------------------------

## Node-attribute columns follow nodes (digraph).
%!test
%! NT.Name = {"a"; "b"; "c"};
%! NT.Size = [10; 20; 30];
%! ET.EndNodes = [1 2; 2 3];
%! G = digraph (ET, NT);
%! H = reordernodes (G, [3 1 2]);
%! assert (H.Nodes.Name, {"c"; "a"; "b"});
%! assert (H.Nodes.Size, [30; 10; 20]);

## Node-attribute columns follow nodes (graph).
%!test
%! NT.Name = {"a"; "b"; "c"};
%! NT.Kind = {"x"; "y"; "z"};
%! ET.EndNodes = [1 2; 2 3];
%! G = graph (ET, NT);
%! H = reordernodes (G, {"c", "a", "b"});
%! assert (H.Nodes.Name, {"c"; "a"; "b"});
%! assert (H.Nodes.Kind, {"z"; "x"; "y"});

## ---------------- Value semantics ----------------------------------

## Original graph is unchanged after reordernodes.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! H = reordernodes (G, [3 1 2]);
%! assert (numnodes (G), 3);
%! assert (numedges (G), 3);
%! assert (G.Edges.EndNodes, [1 2; 2 3; 3 1]);

## ---------------- Class preservation -------------------------------

## Return class: digraph -> digraph.
%!test
%! G = digraph ([1 2], [2 3]);
%! H = reordernodes (G, [2 1 3]);
%! assert (isa (H, "digraph"));
%! assert (! isa (H, "graph"));

## Return class: graph -> graph.
%!test
%! G = graph ([1 2], [2 3]);
%! H = reordernodes (G, [2 1 3]);
%! assert (isa (H, "graph"));
%! assert (! isa (H, "digraph"));

## ---------------- Self-loops ---------------------------------------

## Self-loop in digraph: preserved on reordered node.
%!test
%! G = digraph ([1 2 2 3], [2 2 3 1]);
%! ## Edges: 1->2, 2->2 (self), 2->3, 3->1.
%! H = reordernodes (G, [3 1 2]);
%! ## Inv = [2 3 1].  Edges remap:
%! ## 1->2 -> 2->3; 2->2 -> 3->3 (self-loop on node 3);
%! ## 2->3 -> 3->1; 3->1 -> 1->2.
%! ## Sorted: [1 2; 2 3; 3 1; 3 3].
%! assert (numnodes (H), 3);
%! assert (numedges (H), 4);
%! assert (H.Edges.EndNodes, [1 2; 2 3; 3 1; 3 3]);

## Self-loop in graph: preserved.
%!test
%! G = graph ([1 2 3], [1 3 3]);
%! ## Edges: {1,1} self, {2,3}, {3,3} self.
%! H = reordernodes (G, [3 2 1]);
%! ## Inv = [3 2 1].  Edges remap:
%! ## {1,1} -> {3,3} (self on new 3);
%! ## {2,3} -> {2,1} -> (1,2);
%! ## {3,3} -> {1,1} (self on new 1).
%! ## Sorted: [1 1; 1 2; 3 3].
%! assert (numnodes (H), 3);
%! assert (numedges (H), 3);
%! assert (H.Edges.EndNodes, [1 1; 1 2; 3 3]);

## ---------------- Multigraph path (digraph) ------------------------

## Multigraph with parallel edges: edges preserved under reorder.
%!test
%! G = digraph ([1 1 2], [2 2 3], "multigraph");
%! H = reordernodes (G, [3 1 2]);
%! ## Inv = [2 3 1].  Edges: 1->2 (x2), 2->3.
%! ## Remapped: 2->3 (x2), 3->1.  Sorted lex: 2->3, 2->3, 3->1.
%! assert (numnodes (H), 3);
%! assert (numedges (H), 3);
%! assert (ismultigraph (H), true);
%! assert (H.Edges.EndNodes, [2 3; 2 3; 3 1]);

## Multigraph weighted: weights follow parallel edges.
%!test
%! G = digraph ([1 1 2], [2 2 3], [10 20 30], "multigraph");
%! H = reordernodes (G, [3 1 2]);
%! ## Inv = [2 3 1].  Old edges: 1->2 w=10, 1->2 w=20, 2->3 w=30.
%! ## Remapped: 2->3 w=10, 2->3 w=20, 3->1 w=30.
%! ## Sorted lex (stable): 2->3 w=10, 2->3 w=20, 3->1 w=30.
%! assert (H.Edges.EndNodes, [2 3; 2 3; 3 1]);
%! assert (H.Edges.Weight, [10; 20; 30]);

## ---------------- Empty graph --------------------------------------

## Empty digraph with empty perm is a no-op.
%!test
%! G = digraph ();
%! H = reordernodes (G, []);
%! assert (numnodes (H), 0);
%! assert (numedges (H), 0);

## Empty graph with empty perm is a no-op.
%!test
%! G = graph ();
%! H = reordernodes (G, []);
%! assert (numnodes (H), 0);
%! assert (numedges (H), 0);

## Single-node digraph with perm = [1] is a no-op.
%!test
%! G = digraph (1);
%! H = reordernodes (G, 1);
%! assert (numnodes (H), 1);
%! assert (numedges (H), 0);

## ---------------- Dot-notation dispatch ----------------------------

## G.reordernodes(perm) works via classdef method (digraph).
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! H = G.reordernodes ([3 1 2]);
%! assert (numnodes (H), 3);
%! assert (H.Edges.EndNodes, [1 2; 2 3; 3 1]);

## G.reordernodes(names) works via classdef method (graph).
%!test
%! G = graph ([1 2], [2 3], [], {"a", "b", "c"});
%! H = G.reordernodes ({"b", "c", "a"});
%! assert (H.Nodes.Name, {"b"; "c"; "a"});

## ---------------- Structural equivalence (acceptance) --------------

## adj(H)(i, j) == adj(G)(perm(i), perm(j)) for all i, j (digraph).
%!test
%! G = digraph ([1 2 3 3 4 5], [2 3 2 4 5 1], [11 12 13 14 15 16]);
%! perm = [5 2 4 1 3];
%! H = reordernodes (G, perm);
%! A_old = full (adjacency (G, "weighted"));
%! A_new = full (adjacency (H, "weighted"));
%! assert (A_new, A_old(perm, perm));

## adj(H)(i, j) == adj(G)(perm(i), perm(j)) for all i, j (graph).
%!test
%! G = graph ([1 2 3 4 5], [2 3 4 5 1], [10 20 30 40 50]);
%! perm = [5 3 1 4 2];
%! H = reordernodes (G, perm);
%! A_old = full (adjacency (G, "weighted"));
%! A_new = full (adjacency (H, "weighted"));
%! assert (A_new, A_old(perm, perm));

## ---------------- Siever-style fixture -----------------------------

## 9-node siever-style digraph reversed.
%!test
%! s = [1 2 3 3 4 5 5 6 7 7 8 9];
%! t = [2 3 2 4 5 6 9 7 8 9 7 4];
%! G = digraph (s, t);
%! perm = 9:-1:1;
%! H = reordernodes (G, perm);
%! assert (numnodes (H), 9);
%! assert (numedges (H), 12);
%! A_old = full (adjacency (G));
%! A_new = full (adjacency (H));
%! assert (A_new, A_old(perm, perm));

## ---------------- Error handling -----------------------------------

## Not a permutation: missing index (duplicate).
%!error <permutation> ...
%! G = digraph ([1 2], [2 3]);
%! reordernodes (G, [1 2 2]);

## Not a permutation: duplicate index.
%!error <permutation> ...
%! G = digraph ([1 2], [2 3]);
%! reordernodes (G, [1 1 2]);

## Wrong length: too short.
%!error <permutation> ...
%! G = digraph ([1 2], [2 3]);
%! reordernodes (G, [1 2]);

## Wrong length: too long.  The out-of-range index is caught first,
## before the permutation-length check -- that is reasonable since the
## index was out of 1:numnodes anyway.
%!error <invalid node index> ...
%! G = digraph ([1 2], [2 3]);
%! reordernodes (G, [1 2 3 4]);

## Out-of-range numeric index.
%!error <invalid node index> ...
%! G = digraph ([1 2], [2 3]);
%! reordernodes (G, [1 2 4]);

## Zero index.
%!error <invalid node index> ...
%! G = digraph ([1 2], [2 3]);
%! reordernodes (G, [0 1 2]);

## Negative index.
%!error <invalid node index> ...
%! G = digraph ([1 2], [2 3]);
%! reordernodes (G, [-1 2 3]);

## Non-integer index.
%!error <invalid node index> ...
%! G = digraph ([1 2], [2 3]);
%! reordernodes (G, [1.5 2 3]);

## Inf index.
%!error <invalid node index> ...
%! G = digraph ([1 2], [2 3]);
%! reordernodes (G, [Inf 2 3]);

## NaN index.
%!error <invalid node index> ...
%! G = digraph ([1 2], [2 3]);
%! reordernodes (G, [NaN 2 3]);

## Name not present.
%!error <not found> ...
%! G = digraph ([1 2], [2 3], [], {"a", "b", "c"});
%! reordernodes (G, {"a", "b", "z"});

## Cellstr perm missing a node name.
%!error <permutation> ...
%! G = digraph ([1 2], [2 3], [], {"a", "b", "c"});
%! reordernodes (G, {"a", "b"});

## Cellstr perm with a duplicate name.
%!error <permutation> ...
%! G = digraph ([1 2], [2 3], [], {"a", "b", "c"});
%! reordernodes (G, {"a", "b", "a"});

## Name given but graph is unnamed.
%!error <no node names> ...
%! G = digraph (3);
%! reordernodes (G, {"a", "b", "c"});

## Unsupported type (logical).
%!error <numeric index array> ...
%! G = digraph (3);
%! reordernodes (G, [true false true]);

## Non-graph first argument.
%!error <graph or digraph> ...
%! reordernodes (42, 1);

## Non-graph first argument (string).
%!error <graph or digraph> ...
%! reordernodes ("hello", 1);

## nargin < 2.
%!error <Invalid call> ...
%! reordernodes (digraph ([1 2], [2 3]));
