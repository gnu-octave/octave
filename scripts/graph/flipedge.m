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
## @deftypefn  {} {@var{H} =} flipedge (@var{G})
## @deftypefnx {} {@var{H} =} flipedge (@var{G}, @var{edgeIdx})
## Reverse the direction of edges in the directed graph @var{G} and
## return the resulting digraph @var{H}.
##
## @var{G} must be a @code{digraph} object; @code{flipedge} is not
## defined for an undirected @code{graph}.  Two call forms are
## supported:
##
## @table @asis
## @item @code{flipedge (@var{G})}
## Reverse @emph{every} edge of @var{G}.  The adjacency matrix of
## @var{H} is the transpose of the adjacency matrix of @var{G}, so
## @code{adjacency (flipedge (@var{G})) == adjacency (@var{G}).'}.
## @item @code{flipedge (@var{G}, @var{edgeIdx})}
## Reverse only the edges at the positions given by @var{edgeIdx}, a
## numeric array of positive integer indices in
## @code{1:numedges (@var{G})}.  Duplicate indices are silently
## deduplicated (each edge is reversed at most once).  It is an error
## for the resulting graph to contain duplicate directed edges unless
## @var{G} is a multigraph.
## @end table
##
## The returned graph @var{H} has the same class as @var{G} (always
## @code{digraph}); the @qcode{'multigraph'} flag, weight flag, node
## count, node names, and node-attribute columns are preserved.
## Self-loops are unaffected by flipping.  Edge weights and
## edge-attribute columns follow their edges in the reversed graph,
## so @code{H.Edges.Weight} and any extra edge-attribute columns
## are reordered to match the new lexicographic edge order.
## Value semantics: @var{G} is not modified.
##
## @example
## @group
## G = digraph ([1 2 3], [2 3 1]);
## H = flipedge (G);
## H.Edges.EndNodes                   # @result{} [1 3; 2 1; 3 2]
##
## G = digraph ([1 2 3 4], [2 3 4 1], [10 20 30 40]);
## H = flipedge (G, [1 3]);
## ## Edges 1 (1->2) and 3 (3->4) are reversed to 2->1 and 4->3.
## numedges (H)                       # @result{} 4
## @end group
## @end example
##
## @seealso{digraph, rmedge, addedge, reordernodes, subgraph}
## @end deftypefn

function H = flipedge (G, varargin)

  ## NOTE: When called with a digraph first argument, Octave's classdef
  ## method dispatch runs the class-internal @code{flipedge} method
  ## and this free-function body is not reached.  This file exists both
  ## as a canonical documentation target (so @code{help flipedge} works
  ## outside the context of an instance) and as a fallback that gives a
  ## helpful error for non-digraph inputs, including the undirected
  ## @code{graph} class for which @code{flipedge} is undefined.

  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif

  if (isa (G, "graph") && ! isa (G, "digraph"))
    error ("Octave:invalid-input-arg", ...
           ["flipedge: G must be a digraph object; flipedge is not ", ...
            "defined for the undirected graph class"]);
  endif

  if (! isa (G, "digraph"))
    error ("Octave:invalid-input-arg", ...
           "flipedge: G must be a digraph object");
  endif

  ## Defensive delegation through dot notation.
  H = G.flipedge (varargin{:});

endfunction


## ------------------------------------------------------------------
## BIST blocks
## ------------------------------------------------------------------

## ---------------- Form 1: flipedge(G) all edges --------------------

## 3-cycle: reversing a 1->2->3->1 cycle gives 1->3->2->1.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! H = flipedge (G);
%! assert (numnodes (H), 3);
%! assert (numedges (H), 3);
%! ## Old edges 1->2, 2->3, 3->1 reversed: 2->1, 3->2, 1->3.
%! ## In lex order: [1 3; 2 1; 3 2].
%! assert (H.Edges.EndNodes, [1 3; 2 1; 3 2]);

## Path graph: 1->2->3 becomes 3->2->1.
%!test
%! G = digraph ([1 2], [2 3]);
%! H = flipedge (G);
%! assert (numnodes (H), 3);
%! assert (numedges (H), 2);
%! ## Reversed edges: 2->1, 3->2.  Sorted lex: [2 1; 3 2].
%! assert (H.Edges.EndNodes, [2 1; 3 2]);

## Adjacency matrix of flipedge(G) equals transpose of adjacency of G.
%!test
%! G = digraph ([1 2 3 3 4 5], [2 3 2 4 5 1]);
%! H = flipedge (G);
%! A_old = full (adjacency (G));
%! A_new = full (adjacency (H));
%! assert (A_new, A_old.');

## Weighted adjacency matrix of flipedge(G) equals transpose.
%!test
%! G = digraph ([1 2 3], [2 3 1], [10 20 30]);
%! H = flipedge (G);
%! A_old = full (adjacency (G, "weighted"));
%! A_new = full (adjacency (H, "weighted"));
%! assert (A_new, A_old.');

## Weighted flipedge: weights follow edges to the new lex positions.
%!test
%! G = digraph ([1 2 3], [2 3 1], [10 20 30]);
%! H = flipedge (G);
%! ## Old edges in lex order: (1,2,10), (2,3,20), (3,1,30).
%! ## Reversed: (2,1,10), (3,2,20), (1,3,30).
%! ## Sorted new lex (1,3), (2,1), (3,2): weights [30; 10; 20].
%! assert (H.Edges.EndNodes, [1 3; 2 1; 3 2]);
%! assert (H.Edges.Weight, [30; 10; 20]);

## Self-loop is unaffected by flipping.
%!test
%! G = digraph ([1 2 3], [1 3 2]);
%! ## Edges: 1->1 (self), 2->3, 3->2.
%! H = flipedge (G);
%! ## Reversed: 1->1 (self, unchanged), 3->2, 2->3.
%! ## Sorted lex: [1 1; 2 3; 3 2].
%! assert (H.Edges.EndNodes, [1 1; 2 3; 3 2]);
%! assert (numedges (H), 3);

## Double flip is identity.
%!test
%! G = digraph ([1 2 3 4], [2 3 4 1], [10 20 30 40]);
%! H = flipedge (flipedge (G));
%! assert (H.Edges.EndNodes, G.Edges.EndNodes);
%! assert (H.Edges.Weight, G.Edges.Weight);

## Unweighted digraph: no Weight field after flipedge.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! H = flipedge (G);
%! assert (! isfield (H.Edges, "Weight"));

## Empty digraph: flipedge is a no-op.
%!test
%! G = digraph ();
%! H = flipedge (G);
%! assert (numnodes (H), 0);
%! assert (numedges (H), 0);

## Edgeless digraph with isolated nodes: no edges, no change.
%!test
%! G = digraph (5);
%! H = flipedge (G);
%! assert (numnodes (H), 5);
%! assert (numedges (H), 0);

## ---------------- Form 1: named digraph ----------------------------

## Named digraph: node names and attributes preserved, edges reversed.
%!test
%! G = digraph ([1 2 3], [2 3 1], [], {"a", "b", "c"});
%! H = flipedge (G);
%! assert (H.Nodes.Name, {"a"; "b"; "c"});
%! ## Edges were a->b, b->c, c->a; reversed: b->a, c->b, a->c.
%! ## Indices sorted lex: [1 3; 2 1; 3 2].
%! assert (H.Edges.EndNodes, [1 3; 2 1; 3 2]);

## ---------------- Form 1: multigraph -------------------------------

## Multigraph: parallel edges reversed together.
%!test
%! G = digraph ([1 1 2], [2 2 3], "multigraph");
%! H = flipedge (G);
%! assert (ismultigraph (H));
%! assert (numedges (H), 3);
%! ## Reversed edges: 2->1 (x2), 3->2.  Sorted lex: [2 1; 2 1; 3 2].
%! assert (H.Edges.EndNodes, [2 1; 2 1; 3 2]);

## Weighted multigraph: weights follow parallel edges after flip.
%!test
%! G = digraph ([1 1 2], [2 2 3], [10 20 30], "multigraph");
%! H = flipedge (G);
%! assert (H.Edges.EndNodes, [2 1; 2 1; 3 2]);
%! assert (H.Edges.Weight, [10; 20; 30]);

## ---------------- Form 1: edge attributes --------------------------

## Extra edge-attribute Tag column follows edges after flip.
%!test
%! ET.EndNodes = [1 2; 2 3; 3 1];
%! ET.Weight = [10; 20; 30];
%! ET.Tag = {"x"; "y"; "z"};
%! G = digraph (ET);
%! H = flipedge (G);
%! ## Reversed edges in new lex: (1,3)=z, (2,1)=x, (3,2)=y.
%! assert (H.Edges.EndNodes, [1 3; 2 1; 3 2]);
%! assert (H.Edges.Weight, [30; 10; 20]);
%! assert (H.Edges.Tag, {"z"; "x"; "y"});

## Node-attribute columns are unchanged under flipedge.
%!test
%! NT.Name = {"a"; "b"; "c"};
%! NT.Size = [10; 20; 30];
%! ET.EndNodes = [1 2; 2 3];
%! G = digraph (ET, NT);
%! H = flipedge (G);
%! assert (H.Nodes.Name, {"a"; "b"; "c"});
%! assert (H.Nodes.Size, [10; 20; 30]);

## ---------------- Form 2: flipedge(G, edgeIdx) ---------------------

## Scalar edgeIdx on simple digraph.
%!test
%! G = digraph ([1 2 3 4], [2 3 4 1]);
%! ## Edges in lex order: 1->2, 2->3, 3->4, 4->1.
%! H = flipedge (G, 1);
%! ## Reverse only the first edge: 2->1 replaces 1->2.
%! ## New edges in lex: 2->1, 2->3, 3->4, 4->1.
%! assert (numedges (H), 4);
%! assert (H.Edges.EndNodes, [2 1; 2 3; 3 4; 4 1]);

## Vector edgeIdx on simple digraph.
%!test
%! G = digraph ([1 2 3 4], [2 3 4 1], [10 20 30 40]);
%! H = flipedge (G, [1 3]);
%! ## Reverse edges 1 (1->2, w=10) and 3 (3->4, w=30):
%! ## new edges 2->1 (w=10), 4->3 (w=30), plus unchanged 2->3 (w=20)
%! ## and 4->1 (w=40).  Sorted lex:
%! ## (2,1)=10, (2,3)=20, (4,1)=40, (4,3)=30.
%! assert (H.Edges.EndNodes, [2 1; 2 3; 4 1; 4 3]);
%! assert (H.Edges.Weight, [10; 20; 40; 30]);

## Empty edgeIdx is a no-op.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! H = flipedge (G, []);
%! assert (H.Edges.EndNodes, G.Edges.EndNodes);
%! assert (numedges (H), numedges (G));

## Duplicate indices are silently deduplicated.
%!test
%! G = digraph ([1 2 3 4], [2 3 4 1]);
%! H1 = flipedge (G, [1 1 3]);
%! H2 = flipedge (G, [1 3]);
%! assert (H1.Edges.EndNodes, H2.Edges.EndNodes);

## Column-vector edgeIdx is accepted.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! H = flipedge (G, [1; 3]);
%! ## Reverse edges 1 (1->2) and 3 (3->1): 2->1 and 1->3.
%! ## Plus unchanged 2->3.  Sorted lex: [1 3; 2 1; 2 3].
%! assert (H.Edges.EndNodes, [1 3; 2 1; 2 3]);

## Reverse every edge (index = all edges) equals flipedge(G).
%!test
%! G = digraph ([1 2 3 4], [2 3 4 1], [10 20 30 40]);
%! H1 = flipedge (G, 1:numedges (G));
%! H2 = flipedge (G);
%! assert (H1.Edges.EndNodes, H2.Edges.EndNodes);
%! assert (H1.Edges.Weight, H2.Edges.Weight);

## Self-loop: flipping a self-loop edge is a no-op on that edge.
%!test
%! G = digraph ([1 2 3], [1 3 2]);
%! ## Edges in lex order: 1->1 (self, index 1), 2->3 (index 2),
%! ## 3->2 (index 3).
%! H = flipedge (G, 1);
%! ## Reversing the self-loop is a no-op; graph unchanged.
%! assert (H.Edges.EndNodes, G.Edges.EndNodes);

## Flipping the same edge twice (tracking its new index) is identity.
## After flipping index 1 (1->2 -> 2->1), the new lex-order edge 1 is
## still (2,1); flipping it again restores (1,2).
%!test
%! G = digraph ([1 2 3], [2 3 1], [10 20 30]);
%! H = flipedge (G, 1);
%! ## After first flip, edges are (2,1,10), (2,3,20), (3,1,30) - the
%! ## new lex-order puts (2,1) at index 1 still.
%! assert (H.Edges.EndNodes, [2 1; 2 3; 3 1]);
%! H2 = flipedge (H, 1);
%! ## Flip it back: edges are (1,2), (2,3), (3,1) = original.
%! assert (H2.Edges.EndNodes, G.Edges.EndNodes);
%! assert (H2.Edges.Weight, G.Edges.Weight);

## ---------------- Form 2: multigraph -------------------------------

## Multigraph: flip one of two parallel edges.  After flip the
## parallel pair is broken so ismultigraph becomes false, but the
## underlying multigraph storage mode is preserved.
%!test
%! G = digraph ([1 1 2], [2 2 3], "multigraph");
%! ## Edges: two parallel 1->2 and one 2->3 (indices 1, 2, 3).
%! H = flipedge (G, 1);
%! ## Flip edge index 1 (1->2 parallel): becomes 2->1.
%! ## Remaining: 1->2 (index 2 orig), 2->3.
%! ## Sorted lex: (1,2), (2,1), (2,3).
%! assert (numedges (H), 3);
%! assert (H.Edges.EndNodes, [1 2; 2 1; 2 3]);
%! ## ismultigraph reports actual duplicates; none remain after flip.
%! assert (ismultigraph (H), false);

## Multigraph: flipping all parallel edges preserves the multigraph flag.
%!test
%! G = digraph ([1 1 2], [2 2 3], [10 20 30], "multigraph");
%! H = flipedge (G, [1 2]);
%! ## Flip both parallel 1->2 edges (weights 10 and 20): both become 2->1.
%! ## Plus unchanged 2->3 (w=30).
%! ## Sorted lex: (2,1) x2, (2,3).
%! assert (H.Edges.EndNodes, [2 1; 2 1; 2 3]);
%! assert (H.Edges.Weight, [10; 20; 30]);
%! assert (ismultigraph (H));

## Multigraph: extra edge-attribute columns follow flipped edges.
%!test
%! G = digraph ([1 1 2], [2 2 3], [10 20 30], "multigraph");
%! H = flipedge (G, 1);
%! ## Edges after flip sorted lex: (1,2) w=20, (2,1) w=10, (2,3) w=30.
%! assert (H.Edges.EndNodes, [1 2; 2 1; 2 3]);
%! assert (H.Edges.Weight, [20; 10; 30]);

## ---------------- Form 2: edge attributes --------------------------

## Extra edge-attribute column permuted to match new lex order.
%!test
%! ET.EndNodes = [1 2; 2 3; 3 1];
%! ET.Weight = [10; 20; 30];
%! ET.Tag = {"x"; "y"; "z"};
%! G = digraph (ET);
%! H = flipedge (G, [1 3]);
%! ## Flip edges 1 (1->2, x, 10) and 3 (3->1, z, 30):
%! ## new edges 2->1 (x,10), 2->3 (y,20), 1->3 (z,30).
%! ## Sorted lex: (1,3)=z/30, (2,1)=x/10, (2,3)=y/20.
%! assert (H.Edges.EndNodes, [1 3; 2 1; 2 3]);
%! assert (H.Edges.Weight, [30; 10; 20]);
%! assert (H.Edges.Tag, {"z"; "x"; "y"});

## ---------------- Form 2: simple duplicate detection ---------------

## Simple digraph: flipedge must error if it would create a duplicate
## edge with an already-present reverse-orientation edge.
%!error <duplicate> ...
%! G = digraph ([1 2], [2 1]);
%! ## Both 1->2 and 2->1 already exist.  Flipping edge 1 (1->2 -> 2->1)
%! ## would duplicate the existing 2->1 edge.
%! flipedge (G, 1);

## ---------------- Value semantics ----------------------------------

## Original digraph is unchanged after flipedge.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! H = flipedge (G);
%! assert (G.Edges.EndNodes, [1 2; 2 3; 3 1]);
%! assert (numedges (G), 3);

## Original digraph is unchanged after indexed flipedge.
%!test
%! G = digraph ([1 2 3 4], [2 3 4 1], [10 20 30 40]);
%! H = flipedge (G, [1 3]);
%! assert (G.Edges.EndNodes, [1 2; 2 3; 3 4; 4 1]);
%! assert (G.Edges.Weight, [10; 20; 30; 40]);

## ---------------- Class preservation -------------------------------

## flipedge of digraph returns digraph.
%!test
%! G = digraph ([1 2], [2 3]);
%! H = flipedge (G);
%! assert (isa (H, "digraph"));
%! assert (! isa (H, "graph"));

## flipedge of weighted digraph preserves weighted flag.
%!test
%! G = digraph ([1 2], [2 3], [5 7]);
%! H = flipedge (G);
%! assert (isfield (H.Edges, "Weight"));
%! ## Old edges (1,2,5), (2,3,7) -> flipped (2,1,5), (3,2,7).
%! ## Sorted lex: (2,1,5), (3,2,7).
%! assert (H.Edges.EndNodes, [2 1; 3 2]);
%! assert (H.Edges.Weight, [5; 7]);

## ---------------- Dot-notation dispatch ----------------------------

## G.flipedge() works via classdef method.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! H = G.flipedge ();
%! assert (H.Edges.EndNodes, [1 3; 2 1; 3 2]);

## G.flipedge(idx) works via classdef method.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! H = G.flipedge (2);
%! ## Flip edge 2 (2->3 -> 3->2): new edges 1->2, 3->2, 3->1.
%! ## Sorted lex: [1 2; 3 1; 3 2].
%! assert (H.Edges.EndNodes, [1 2; 3 1; 3 2]);

## ---------------- Siever-style fixture -----------------------------

## 9-node digraph: flipedge matches adjacency transpose.
%!test
%! s = [1 2 3 3 4 5 5 6 7 7 8 9];
%! t = [2 3 2 4 5 6 9 7 8 9 7 4];
%! G = digraph (s, t);
%! H = flipedge (G);
%! assert (numnodes (H), 9);
%! assert (numedges (H), 12);
%! A_old = full (adjacency (G));
%! A_new = full (adjacency (H));
%! assert (A_new, A_old.');

## ---------------- Error handling: graph class ----------------------

## Calling flipedge on an undirected graph errors.
%!error <not defined for the undirected graph|must be a digraph> ...
%! G = graph ([1 2 3], [2 3 1]);
%! flipedge (G);

## Calling flipedge on a weighted graph errors.
%!error <not defined for the undirected graph|must be a digraph> ...
%! G = graph ([1 2], [2 3], [10 20]);
%! flipedge (G, 1);

## ---------------- Error handling: other ----------------------------

## Non-graph first argument.
%!error <must be a digraph> ...
%! flipedge (42);

## Non-graph first argument (string).
%!error <must be a digraph> ...
%! flipedge ("hello");

## nargin < 1.
%!error ...
%! flipedge ();

## Too many arguments.
%!error ...
%! G = digraph ([1 2], [2 3]);
%! flipedge (G, 1, 2);

## Form 2: edge index out-of-range.
%!error <invalid edge|out of range> ...
%! G = digraph ([1 2], [2 3]);
%! flipedge (G, 99);

## Form 2: edge index zero.
%!error <invalid edge|out of range> ...
%! G = digraph ([1 2], [2 3]);
%! flipedge (G, 0);

## Form 2: edge index negative.
%!error <invalid edge|out of range> ...
%! G = digraph ([1 2], [2 3]);
%! flipedge (G, -1);

## Form 2: edge index non-integer.
%!error <invalid edge|out of range> ...
%! G = digraph ([1 2], [2 3]);
%! flipedge (G, 1.5);

## Form 2: edge index Inf.
%!error <invalid edge|out of range> ...
%! G = digraph ([1 2], [2 3]);
%! flipedge (G, Inf);

## Form 2: edge index NaN.
%!error <invalid edge|out of range> ...
%! G = digraph ([1 2], [2 3]);
%! flipedge (G, NaN);

## Form 2: unsupported type (logical).
%!error <numeric|must be> ...
%! G = digraph (3);
%! flipedge (G, true);

## Form 2: unsupported type (cellstr).
%!error <numeric|must be> ...
%! G = digraph (3);
%! flipedge (G, {"a"});
