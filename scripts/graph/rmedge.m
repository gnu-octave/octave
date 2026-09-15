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
## @deftypefn  {} {@var{H} =} rmedge (@var{G}, @var{s}, @var{t})
## @deftypefnx {} {@var{H} =} rmedge (@var{G}, @var{edgeIdx})
## Remove the specified edges from the graph or digraph @var{G} and
## return the resulting graph @var{H}.
##
## @var{G} must be a @code{graph} or @code{digraph} object.  There are
## two supported call forms:
##
## @table @asis
## @item @code{rmedge (@var{G}, @var{s}, @var{t})}
## Remove every edge whose endpoints are @code{(@var{s}(i), @var{t}(i))}
## for some @code{i}.  @var{s} and @var{t} must have the same number of
## elements and may be numeric node indices, a char row vector (single
## node name), or a cell array of strings.  For a @code{digraph} the
## ordered pair @code{(@var{s}, @var{t})} is matched; for a
## @code{graph} the pair is matched in either orientation.  For a
## multigraph @code{digraph}, @emph{all} parallel edges between each
## @code{(@var{s}(i), @var{t}(i))} pair are removed.  It is an error
## if any requested edge is not present in @var{G}.
##
## @item @code{rmedge (@var{G}, @var{edgeIdx})}
## Remove the edges identified by @var{edgeIdx}, which must be a
## numeric array of positive integer indices in
## @code{1:numedges (@var{G})}.  Duplicate indices are silently
## ignored (the edge is removed only once).
## @end table
##
## The returned graph @var{H} has the same class as @var{G}; the
## @qcode{'multigraph'}, weight, and node-name flags are preserved.
## Node count and node-attribute columns are unchanged; edge-attribute
## columns are filtered to match the surviving edges.  Value semantics:
## @var{G} is not modified.
##
## @example
## @group
## G = digraph ([1 2 3 4], [2 3 4 1]);
## H = rmedge (G, 1, 2);
## numedges (H)                       # @result{} 3
##
## G = graph ([1 2 3], [2 3 1], [10 20 30]);
## H = rmedge (G, [1 3]);
## H.Edges.Weight                     # @result{} 20
## @end group
## @end example
##
## @seealso{graph, digraph, addedge, rmnode, addnode, numedges, findedge}
## @end deftypefn

function H = rmedge (G, varargin)

  ## NOTE: When called with a graph or digraph first argument, Octave's
  ## classdef method dispatch runs the class-internal @code{rmedge}
  ## method and this free-function body is not reached.  This file
  ## exists both as a canonical documentation target (so @code{help
  ## rmedge} works outside the context of an instance) and as a fallback
  ## that gives a helpful error for non-graph inputs.

  if (nargin < 2 || nargin > 3)
    print_usage ();
  endif

  if (! isa (G, "graph") && ! isa (G, "digraph"))
    error ("Octave:invalid-input-arg", ...
           "rmedge: G must be a graph or digraph object");
  endif

  ## Defensive delegation through dot notation.
  H = G.rmedge (varargin{:});

endfunction


## ------------------------------------------------------------------
## BIST blocks
## ------------------------------------------------------------------

## ---------------- Form 1: rmedge(G, s, t) -------------------------

## digraph: remove a single edge by numeric endpoints.
%!test
%! G = digraph ([1 2 3 4], [2 3 4 1]);
%! H = rmedge (G, 1, 2);
%! assert (numedges (H), 3);
%! assert (numnodes (H), 4);
%! ## Edge 1->2 was removed; remaining are 2->3, 3->4, 4->1.
%! assert (H.Edges.EndNodes, [2 3; 3 4; 4 1]);

## graph: remove a single edge by numeric endpoints.
%!test
%! G = graph ([1 2 3 4], [2 3 4 1]);
%! H = rmedge (G, 1, 2);
%! assert (numedges (H), 3);
%! assert (numnodes (H), 4);
%! ## Undirected 4-cycle edges {1,2}, {2,3}, {3,4}, {1,4} ->
%! ## remove {1,2} yields the other three in lex (min,max) order.
%! assert (H.Edges.EndNodes, [1 4; 2 3; 3 4]);

## graph: rmedge matches reversed orientation too (undirected).
%!test
%! G = graph ([1 2 3 4], [2 3 4 1]);
%! H = rmedge (G, 2, 1);
%! ## Exactly the same effect as rmedge(G, 1, 2).
%! assert (numedges (H), 3);
%! assert (H.Edges.EndNodes, [1 4; 2 3; 3 4]);

## digraph: remove multiple edges via vector endpoints.
%!test
%! G = digraph ([1 2 3 4 5], [2 3 4 5 1]);
%! H = rmedge (G, [1 3], [2 4]);
%! assert (numedges (H), 3);
%! assert (H.Edges.EndNodes, [2 3; 4 5; 5 1]);

## graph: remove multiple edges via vector endpoints.
%!test
%! G = graph ([1 2 3 4 5], [2 3 4 5 1]);
%! H = rmedge (G, [1 3], [2 4]);
%! assert (numedges (H), 3);
%! assert (H.Edges.EndNodes, [1 5; 2 3; 4 5]);

## digraph: column-vector endpoints also accepted.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! H = rmedge (G, [1; 3], [2; 1]);
%! assert (numedges (H), 1);
%! assert (H.Edges.EndNodes, [2 3]);

## Self-loop can be removed.
%!test
%! G = digraph ([1 2 3], [1 3 1]);
%! ## Edges: 1->1 (self-loop), 2->3, 3->1.
%! H = rmedge (G, 1, 1);
%! assert (numedges (H), 2);
%! assert (H.Edges.EndNodes, [2 3; 3 1]);

## graph: self-loop can be removed via (s, s).
%!test
%! G = graph ([1 1 2], [1 2 3]);
%! ## Undirected self-loop at 1 plus edges {1,2} and {2,3}.
%! H = rmedge (G, 1, 1);
%! assert (numedges (H), 2);
%! assert (H.Edges.EndNodes, [1 2; 2 3]);

## Value semantics: original graph is unchanged.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! H = rmedge (G, 1, 2);
%! assert (numedges (G), 3);
%! assert (numedges (H), 2);

## ---------------- Form 1: name-based endpoints ---------------------

## digraph: remove edge by char-row name endpoints.
%!test
%! G = digraph ([1 2 3], [2 3 1], [], {"a", "b", "c"});
%! H = rmedge (G, "a", "b");
%! assert (numedges (H), 2);
%! assert (H.Edges.EndNodes, [2 3; 3 1]);
%! ## Node set and names are preserved; only the edge was removed.
%! assert (H.Nodes.Name, {"a"; "b"; "c"});

## graph: remove edge by cellstr name endpoints.
%!test
%! G = graph ([1 2 3], [2 3 1], [], {"a", "b", "c"});
%! H = rmedge (G, {"a"}, {"b"});
%! assert (numedges (H), 2);
%! assert (H.Nodes.Name, {"a"; "b"; "c"});

## digraph: vector of cellstr endpoints.
%!test
%! G = digraph ([1 2 3], [2 3 1], [], {"a", "b", "c"});
%! H = rmedge (G, {"a", "c"}, {"b", "a"});
%! assert (numedges (H), 1);
%! assert (H.Edges.EndNodes, [2 3]);

## graph: mixed char+cellstr - cellstr on both sides.
%!test
%! G = graph ([1 2 3], [2 3 1], [], {"a", "b", "c"});
%! H = rmedge (G, {"c"}, {"a"});
%! ## Edge was {1,3} in storage (min,max); should be gone.
%! assert (numedges (H), 2);

## ---------------- Form 1: weighted preservation --------------------

## Weighted digraph: weights of surviving edges are preserved.
%!test
%! G = digraph ([1 2 3], [2 3 1], [10 20 30]);
%! H = rmedge (G, 1, 2);
%! assert (H.Edges.EndNodes, [2 3; 3 1]);
%! assert (H.Edges.Weight, [20; 30]);

## Weighted graph: removing an edge keeps remaining weights.
%!test
%! G = graph ([1 2 3], [2 3 1], [10 20 30]);
%! H = rmedge (G, 3, 1);
%! ## Surviving: {1,2} w=10 and {2,3} w=20.
%! assert (numedges (H), 2);
%! assert (H.Edges.EndNodes, [1 2; 2 3]);
%! assert (H.Edges.Weight, [10; 20]);

## Unweighted digraph has no Weight field after rmedge.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! H = rmedge (G, 1, 2);
%! assert (! isfield (H.Edges, "Weight"));

## ---------------- Form 1: multigraph parallel edges ----------------

## Multigraph: removing (s,t) removes ALL parallel edges between them.
%!test
%! G = digraph ([1 1 1 2], [2 2 2 3], "multigraph");
%! H = rmedge (G, 1, 2);
%! assert (numedges (H), 1);
%! assert (H.Edges.EndNodes, [2 3]);
%! ## Parallel edges were all removed; ismultigraph is now false
%! ## (storage is still multigraph but no duplicates remain).
%! assert (ismultigraph (H), false);

## Multigraph with some parallel edges surviving after rmedge(1,2).
%!test
%! G = digraph ([1 1 2 2 2], [2 2 3 3 3], "multigraph");
%! H = rmedge (G, 1, 2);
%! ## Three parallel 2->3 edges remain.
%! assert (numedges (H), 3);
%! assert (ismultigraph (H));
%! assert (H.Edges.EndNodes, [2 3; 2 3; 2 3]);

## Multigraph weighted: parallel weights removed together.
%!test
%! G = digraph ([1 1 2], [2 2 3], [10 20 30], "multigraph");
%! H = rmedge (G, 1, 2);
%! assert (numedges (H), 1);
%! assert (H.Edges.EndNodes, [2 3]);
%! assert (H.Edges.Weight, 30);

## ---------------- Form 2: rmedge(G, edgeIdx) -----------------------

## digraph: remove edge by scalar index.
%!test
%! G = digraph ([1 2 3 4], [2 3 4 1]);
%! H = rmedge (G, 2);
%! ## Edges in G: 1->2 (1), 2->3 (2), 3->4 (3), 4->1 (4).
%! assert (numedges (H), 3);
%! assert (H.Edges.EndNodes, [1 2; 3 4; 4 1]);

## graph: remove edge by scalar index.
%!test
%! G = graph ([1 2 3 4], [2 3 4 1]);
%! H = rmedge (G, 1);
%! ## Graph edges in lex order: {1,2}, {1,4}, {2,3}, {3,4}.  Remove
%! ## index 1 ({1,2}).
%! assert (numedges (H), 3);
%! assert (H.Edges.EndNodes, [1 4; 2 3; 3 4]);

## digraph: remove multiple edges by index vector.
%!test
%! G = digraph ([1 2 3 4 5], [2 3 4 5 1]);
%! H = rmedge (G, [1 3]);
%! ## Original edges 1->2 (1), 2->3 (2), 3->4 (3), 4->5 (4), 5->1 (5).
%! ## Remove indices 1 and 3: survivors are 2->3, 4->5, 5->1.
%! assert (numedges (H), 3);
%! assert (H.Edges.EndNodes, [2 3; 4 5; 5 1]);

## graph: remove multiple edges by index vector.
%!test
%! G = graph ([1 2 3 4 5], [2 3 4 5 1]);
%! H = rmedge (G, [2 4]);
%! ## Graph edges in lex order: {1,2},{1,5},{2,3},{3,4},{4,5}.
%! ## Remove indices 2 and 4: survivors are {1,2},{2,3},{4,5}.
%! assert (numedges (H), 3);
%! assert (H.Edges.EndNodes, [1 2; 2 3; 4 5]);

## Empty index list: no-op.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! H = rmedge (G, []);
%! assert (numedges (H), numedges (G));
%! assert (H.Edges.EndNodes, G.Edges.EndNodes);

## Duplicate indices are silently deduplicated.
%!test
%! G = digraph ([1 2 3 4], [2 3 4 1]);
%! H = rmedge (G, [2 2 3]);
%! assert (numedges (H), 2);
%! assert (H.Edges.EndNodes, [1 2; 4 1]);

## Remove all edges: result is an edgeless graph with the same nodes.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! H = rmedge (G, [1 2 3]);
%! assert (numedges (H), 0);
%! assert (numnodes (H), numnodes (G));

## ---------------- Form 2: column vector input ----------------------

## Column-vector edgeIdx input works.
%!test
%! G = digraph ([1 2 3 4], [2 3 4 1]);
%! H = rmedge (G, [1; 4]);
%! assert (numedges (H), 2);
%! assert (H.Edges.EndNodes, [2 3; 3 4]);

## ---------------- Weight preservation under index form -------------

## Weighted digraph edges survive with weights intact.
%!test
%! G = digraph ([1 2 3 4], [2 3 4 1], [10 20 30 40]);
%! H = rmedge (G, [1 3]);
%! ## Survivors: 2->3 (w=20), 4->1 (w=40).
%! assert (H.Edges.EndNodes, [2 3; 4 1]);
%! assert (H.Edges.Weight, [20; 40]);

## ---------------- Edge-attribute filtering -------------------------

## Edge-attribute Tag column is filtered to surviving edges.
%!test
%! ET.EndNodes = [1 2; 2 3; 3 1];
%! ET.Weight = [10; 20; 30];
%! ET.Tag = {"x"; "y"; "z"};
%! G = digraph (ET);
%! H = rmedge (G, 2);
%! ## Survivors: 1->2 (w=10, Tag=x), 3->1 (w=30, Tag=z).
%! assert (H.Edges.EndNodes, [1 2; 3 1]);
%! assert (H.Edges.Weight, [10; 30]);
%! assert (H.Edges.Tag, {"x"; "z"});

## Edge-attribute filtering (graph class).
%!test
%! ET.EndNodes = [1 2; 2 3; 1 3];
%! ET.Weight = [10; 20; 30];
%! ET.Tag = {"x"; "y"; "z"};
%! G = graph (ET);
%! H = rmedge (G, 1, 2);
%! ## Edge (1,2) was index 1; survivors are indices 2 and 3.
%! assert (H.Edges.EndNodes, [1 3; 2 3]);
%! assert (sort (H.Edges.Weight), [20; 30]);
%! assert (sort (H.Edges.Tag), {"y"; "z"});

## ---------------- Class preservation -------------------------------

## Return class: digraph -> digraph.
%!test
%! G = digraph ([1 2], [2 3]);
%! H = rmedge (G, 1);
%! assert (isa (H, "digraph"));
%! assert (! isa (H, "graph"));

## Return class: graph -> graph.
%!test
%! G = graph ([1 2], [2 3]);
%! H = rmedge (G, 1);
%! assert (isa (H, "graph"));
%! assert (! isa (H, "digraph"));

## ---------------- Dot-notation dispatch ----------------------------

## G.rmedge(idx) works via classdef method.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! H = G.rmedge (2);
%! assert (numedges (H), 2);

## G.rmedge(s, t) works via classdef method.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! H = G.rmedge (1, 2);
%! assert (numedges (H), 2);

## ---------------- Error handling -----------------------------------

## nargin < 2.
%!error <Invalid call> ...
%! rmedge (digraph ([1 2], [2 3]));

## Non-graph first argument.
%!error <graph or digraph> ...
%! rmedge (42, 1);

## Non-graph first argument (string).
%!error <graph or digraph> ...
%! rmedge ("hello", 1);

## Form 1: (s, t) length mismatch.
%!error <same length|same number> ...
%! G = digraph ([1 2 3], [2 3 1]);
%! rmedge (G, [1 2], 3);

## Form 1: numeric endpoint out of range.
%!error <invalid node index|not found> ...
%! G = digraph ([1 2], [2 3]);
%! rmedge (G, 1, 99);

## Form 1: numeric endpoint zero.
%!error <invalid node index|not found> ...
%! G = digraph ([1 2], [2 3]);
%! rmedge (G, 0, 1);

## Form 1: numeric endpoint non-integer.
%!error <invalid node index|not found> ...
%! G = digraph ([1 2], [2 3]);
%! rmedge (G, 1.5, 2);

## Form 1: non-existent edge errors.
%!error <no such edge|not an edge|does not exist> ...
%! G = digraph ([1 2], [2 3]);
%! rmedge (G, 1, 3);

## Form 1: node name not present.
%!error <not found> ...
%! G = digraph ([1 2], [2 3], [], {"a", "b", "c"});
%! rmedge (G, "a", "z");

## Form 1: name given but graph is unnamed.
%!error <no node names|not found> ...
%! G = digraph (3);
%! rmedge (G, "x", "y");

## Form 2: edge index out-of-range.
%!error <invalid edge|out of range> ...
%! G = digraph ([1 2], [2 3]);
%! rmedge (G, 99);

## Form 2: edge index zero.
%!error <invalid edge|out of range> ...
%! G = digraph ([1 2], [2 3]);
%! rmedge (G, 0);

## Form 2: edge index negative.
%!error <invalid edge|out of range> ...
%! G = digraph ([1 2], [2 3]);
%! rmedge (G, -1);

## Form 2: edge index non-integer.
%!error <invalid edge|out of range> ...
%! G = digraph ([1 2], [2 3]);
%! rmedge (G, 1.5);

## Form 2: edge index Inf.
%!error <invalid edge|out of range> ...
%! G = digraph ([1 2], [2 3]);
%! rmedge (G, Inf);

## Form 1: unsupported type for s.
%!error <numeric|char|cell|must be> ...
%! G = digraph (3);
%! rmedge (G, true, false);

## Form 2: unsupported type.
%!error <numeric|must be> ...
%! G = digraph (3);
%! rmedge (G, true);

## Too many arguments.
%!error ...
%! G = digraph ([1 2], [2 3]);
%! rmedge (G, 1, 2, 3);
