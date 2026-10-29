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
## @deftypefn  {} {@var{H} =} addedge (@var{G}, @var{s}, @var{t})
## @deftypefnx {} {@var{H} =} addedge (@var{G}, @var{s}, @var{t}, @var{w})
## @deftypefnx {} {@var{H} =} addedge (@var{G}, @var{EdgeTable})
## Append edges to the graph or digraph @var{G} and return the new
## graph @var{H}.
##
## @var{G} must be a @code{graph} or @code{digraph} object.  The
## remaining arguments select the variant:
##
## @table @asis
## @item @var{s}, @var{t}
## Same-length endpoint vectors.  Each entry
## @code{(@var{s}(i), @var{t}(i))} specifies a new edge from
## @var{s}(i) to @var{t}(i) (for a @code{digraph}) or the undirected
## edge between them (for a @code{graph}).  Endpoints may be numeric
## node indices or node names (char row vector, scalar string, or
## cellstr).  Endpoints that refer to node names not already present
## in @var{G} cause new nodes to be appended to the graph before the
## edges are added.  Numeric endpoints that exceed
## @code{numnodes (@var{G})} likewise extend the node set.
##
## @item @var{w}
## Optional weight vector (or scalar) of length @code{numel (@var{s})}.
## When @var{G} already has edge weights (i.e., @code{G.Edges} has a
## @code{Weight} column), @var{w} gives the weights of the newly added
## edges.  When @var{G} is unweighted but has no edges yet, supplying
## @var{w} promotes @var{G} to a weighted graph; supplying @var{w} to
## an unweighted graph that already has edges is an error.  When
## @var{w} is omitted and @var{G} is weighted, the new edges receive
## weight @code{1}.
##
## @item @var{EdgeTable}
## A scalar struct (MATLAB table surrogate) with an @code{EndNodes}
## field (an @code{m}-by-2 numeric matrix of indices or cellstr of
## names) and an optional @code{Weight} field.  The same duplicate-edge
## rules as the @var{s}, @var{t}, @var{w} form apply.  Extra
## columns are reserved for a future story and currently cause an
## error.  Edge-attribute columns already stored on @var{G} (from a
## prior @code{digraph (ET, @dots{})} constructor) are extended with
## type-appropriate default rows so the resulting graph has consistent
## per-edge columns.
## @end table
##
## For a plain @code{graph} or a @code{digraph} without the
## @qcode{'multigraph'} flag, adding an edge that already exists is
## an error (parallel edges require the @qcode{'multigraph'} flag on
## @code{digraph}).  For a multigraph @code{digraph}, parallel edges
## are appended to the edge list in the order supplied.
##
## The graph class @var{G} is preserved (a @code{graph} returns a
## @code{graph}; a @code{digraph} returns a @code{digraph}).
##
## @example
## @group
## G = digraph ([1 2 3], [2 3 1]);
## H = addedge (G, 3, 2);
## numedges (H)                       # @result{} 4
##
## G = graph ([1 2], [2 3], [], @{"a", "b", "c"@});
## H = addedge (G, "c", "d");
## H.Nodes.Name                       # @result{} @{"a"; "b"; "c"; "d"@}
##
## ET.EndNodes = [1 4; 2 4];
## ET.Weight   = [7; 8];
## G = digraph ([1 2 3], [2 3 1], [10 20 30]);
## H = addedge (G, ET);
## @end group
## @end example
##
## @seealso{graph, digraph, addnode, rmnode, rmedge, numedges, findedge}
## @end deftypefn

function H = addedge (G, varargin)

  ## NOTE: When called with a graph or digraph object, Octave's classdef
  ## method dispatch runs the class-internal @code{addedge} method and
  ## this free-function body is not reached.  This file exists both as a
  ## canonical documentation target (so @code{help addedge} works outside
  ## the context of an instance) and as a fallback that gives a helpful
  ## error for non-graph inputs.

  if (nargin < 2 || nargin > 4)
    print_usage ();
  endif

  if (! isa (G, "graph") && ! isa (G, "digraph"))
    error ("Octave:invalid-input-arg", ...
           "addedge: G must be a graph or digraph object");
  endif

  ## Defensive delegation: classdef method dispatch should intercept any
  ## call with a graph/digraph first arg, but route through dot notation
  ## to be safe.
  H = G.addedge (varargin{:});

endfunction


## ------------------------------------------------------------------
## BIST blocks
## ------------------------------------------------------------------

## ---------------- Form 1: addedge(G, s, t) ------------------------

## digraph: append a single directed edge (numeric endpoints).
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! H = addedge (G, 3, 2);
%! assert (numedges (H), 4);
%! assert (numnodes (H), 3);
%! assert (ismember ([3, 2], H.Edges.EndNodes, "rows"));

## graph: append a single undirected edge (numeric endpoints).
%!test
%! G = graph ([1 2], [2 3]);
%! H = addedge (G, 1, 3);
%! assert (numedges (H), 3);
%! assert (numnodes (H), 3);
%! ## Undirected lookup works either way.
%! assert (findedge (H, 1, 3) > 0);
%! assert (findedge (H, 3, 1) > 0);

## digraph: append multiple edges via vector endpoints.
%!test
%! G = digraph (4);
%! H = addedge (G, [1 2 3], [2 3 4]);
%! assert (numedges (H), 3);
%! assert (H.Edges.EndNodes, [1 2; 2 3; 3 4]);

## graph: append multiple edges via vector endpoints.
%!test
%! G = graph (4);
%! H = addedge (G, [1 2 3], [2 3 4]);
%! assert (numedges (H), 3);
%! assert (H.Edges.EndNodes, [1 2; 2 3; 3 4]);

## Value semantics: original G is unchanged.
%!test
%! G = digraph ([1 2], [2 3]);
%! H = addedge (G, 3, 1);
%! assert (numedges (G), 2);
%! assert (numedges (H), 3);

## Adding a self-loop is accepted.
%!test
%! G = digraph (3);
%! H = addedge (G, 2, 2);
%! assert (numedges (H), 1);
%! assert (H.Edges.EndNodes, [2 2]);

## Unweighted graph: addedge(G, s, t) keeps G unweighted (no Weight).
%!test
%! G = digraph ([1 2], [2 3]);
%! H = addedge (G, 3, 1);
%! assert (! isfield (H.Edges, "Weight"));

## Weighted graph: addedge(G, s, t) defaults new edge weights to 1.
%!test
%! G = digraph ([1 2], [2 3], [10 20]);
%! H = addedge (G, 3, 1);
%! assert (numedges (H), 3);
%! ## Weight column present.
%! assert (isfield (H.Edges, "Weight"));
%! ## New edge (3, 1) has weight 1.
%! idx = findedge (H, 3, 1);
%! assert (H.Edges.Weight(idx), 1);

## ---------------- Form 1b: string / cellstr endpoints -------------

## String endpoints on a named graph resolve to existing indices.
%!test
%! G = digraph ([1 2], [2 3], [], {"a", "b", "c"});
%! H = addedge (G, "c", "a");
%! assert (numedges (H), 3);
%! assert (numnodes (H), 3);

## Cellstr endpoints on a named graph (no collision with existing).
%!test
%! G2 = graph ([1], [2], [], {"a", "b", "c"});  # only a-b present
%! H2 = addedge (G2, {"a"; "b"}, {"c"; "c"});   # add a-c and b-c
%! assert (numedges (H2), 3);
%! assert (findedge (H2, "a", "c") > 0);
%! assert (findedge (H2, "b", "c") > 0);

## Auto-add missing node name: named digraph.
%!test
%! G = digraph ([1 2], [2 3], [], {"a", "b", "c"});
%! H = addedge (G, "c", "d");
%! assert (numnodes (H), 4);
%! assert (H.Nodes.Name, {"a"; "b"; "c"; "d"});
%! assert (numedges (H), 3);

## Auto-add two missing names at once.
%!test
%! G = digraph ([1 2], [2 3], [], {"a", "b", "c"});
%! H = addedge (G, {"d"; "e"}, {"a"; "a"});
%! assert (numnodes (H), 5);
%! assert (H.Nodes.Name, {"a"; "b"; "c"; "d"; "e"});
%! assert (numedges (H), 4);

## Auto-add missing name on named graph (undirected).
%!test
%! G = graph ([1 2], [2 3], [], {"a", "b", "c"});
%! H = addedge (G, "c", "d");
%! assert (numnodes (H), 4);
%! assert (H.Nodes.Name, {"a"; "b"; "c"; "d"});
%! assert (numedges (H), 3);

## Auto-extend numeric endpoint beyond numnodes(G): unnamed graph.
%!test
%! G = digraph (3);
%! H = addedge (G, 2, 5);
%! assert (numnodes (H), 5);
%! assert (numedges (H), 1);

## Auto-extend numeric endpoint on a named graph: appends "NodeK" names.
%!test
%! G = digraph ([1 2], [2 3], [], {"a", "b", "c"});
%! H = addedge (G, 3, 5);
%! assert (numnodes (H), 5);
%! assert (numedges (H), 3);
%! assert (numel (H.Nodes.Name), 5);
%! ## First 3 names preserved.
%! assert (H.Nodes.Name(1:3), {"a"; "b"; "c"});
%! ## New auto-names follow the "Node%d" pattern.
%! assert (all (strncmp (H.Nodes.Name(4:5), "Node", 4)));

## ---------------- Form 2: addedge(G, s, t, w) ---------------------

## Weighted digraph: append weighted edge.
%!test
%! G = digraph ([1 2], [2 3], [10 20]);
%! H = addedge (G, 3, 1, 30);
%! assert (numedges (H), 3);
%! idx = findedge (H, 3, 1);
%! assert (H.Edges.Weight(idx), 30);

## Weighted graph: append multiple weighted edges.
%!test
%! G = graph ([1 2], [2 3], [10 20]);
%! H = addedge (G, [1 1], [3 4], [5 7]);
%! assert (numedges (H), 4);
%! assert (numnodes (H), 4);
%! ## All edges are present.
%! assert (findedge (H, 1, 3) > 0);
%! assert (findedge (H, 1, 4) > 0);

## Scalar weight broadcasts over vector endpoints.
%!test
%! G = digraph (4);
%! H = addedge (G, [1 2], [3 4], 7);
%! assert (isfield (H.Edges, "Weight"));
%! assert (sort (H.Edges.Weight), [7; 7]);

## Empty unweighted G + weighted addedge promotes to weighted.
%!test
%! G = digraph (3);   # empty, unweighted
%! H = addedge (G, 1, 2, 5);
%! assert (isfield (H.Edges, "Weight"));
%! assert (H.Edges.Weight, 5);

## ---------------- Form 3: addedge(G, EdgeTable) ------------------

## EdgeTable with EndNodes only.
%!test
%! G = digraph (4);
%! ET.EndNodes = [1 2; 2 3];
%! H = addedge (G, ET);
%! assert (numedges (H), 2);
%! assert (H.Edges.EndNodes, [1 2; 2 3]);

## EdgeTable with Weight.
%!test
%! G = digraph ([1 2], [2 3], [10 20]);
%! ET.EndNodes = [3 1];
%! ET.Weight   = 30;
%! H = addedge (G, ET);
%! assert (numedges (H), 3);
%! idx = findedge (H, 3, 1);
%! assert (H.Edges.Weight(idx), 30);

## EdgeTable with cellstr EndNodes resolves against names.
%!test
%! G = digraph ([1 2], [2 3], [], {"a", "b", "c"});
%! ET.EndNodes = {"c", "a"};
%! H = addedge (G, ET);
%! assert (numedges (H), 3);

## EdgeTable with cellstr EndNodes introduces new named nodes.
%!test
%! G = graph ([1 2], [2 3], [], {"a", "b", "c"});
%! ET.EndNodes = {"c", "d"};
%! H = addedge (G, ET);
%! assert (numnodes (H), 4);
%! assert (H.Nodes.Name, {"a"; "b"; "c"; "d"});

## Edge-attribute columns on G are extended with default rows for new edges.
%!test
%! ET.EndNodes = [1 2; 2 3];
%! ET.Weight = [10; 20];
%! ET.Kind = {"a"; "b"};
%! G = digraph (ET);
%! H = addedge (G, 3, 1, 30);
%! assert (numedges (H), 3);
%! assert (H.Edges.Weight, [10; 20; 30]);
%! ## Old Kind values preserved; new edge gets default empty string.
%! assert (H.Edges.Kind, {"a"; "b"; ""});

## ---------------- Multigraph path --------------------------------

## Multigraph digraph: addedge appends to mg storage.
%!test
%! G = digraph ([1 2], [2 3], "multigraph");
%! H = addedge (G, 1, 2);   # parallel edge allowed
%! assert (numedges (H), 3);
%! assert (ismultigraph (H), true);

## Multigraph: addedge with scalar weight on weighted mg digraph.
%!test
%! G = digraph ([1 2], [2 3], [10 20], "multigraph");
%! H = addedge (G, 2, 3, 99);
%! assert (numedges (H), 3);
%! ## Find all edges (2, 3); verify one has weight 20 and one 99.
%! idx = find (H.Edges.EndNodes(:,1) == 2 & H.Edges.EndNodes(:,2) == 3);
%! assert (numel (idx), 2);
%! assert (sort (H.Edges.Weight(idx)), [20; 99]);

## ---------------- Class preservation -----------------------------

## Return class: digraph -> digraph.
%!test
%! G = digraph ([1 2], [2 3]);
%! H = addedge (G, 3, 1);
%! assert (isa (H, "digraph"));
%! assert (! isa (H, "graph"));

## Return class: graph -> graph.
%!test
%! G = graph ([1 2], [2 3]);
%! H = addedge (G, 1, 3);
%! assert (isa (H, "graph"));
%! assert (! isa (H, "digraph"));

## ---------------- Dot-notation dispatch --------------------------

## G.addedge(s, t) works via classdef method.
%!test
%! G = digraph ([1 2], [2 3]);
%! H = G.addedge (3, 1);
%! assert (numedges (H), 3);

## G.addedge(ET) works via classdef method.
%!test
%! G = digraph (4);
%! ET.EndNodes = [1 2; 3 4];
%! H = G.addedge (ET);
%! assert (numedges (H), 2);

## ---------------- Error handling ---------------------------------

## Duplicate edge on simple digraph is rejected.
%!error <duplicate|parallel> ...
%! G = digraph ([1 2], [2 3]);
%! addedge (G, 1, 2);

## Duplicate edge on simple graph is rejected.
%!error <duplicate|parallel> ...
%! G = graph ([1 2], [2 3]);
%! addedge (G, 2, 1);

## Length mismatch between s and t.
%!error <same length|length> ...
%! addedge (digraph (3), [1 2], [3]);

## Weight vector length mismatch.
%!error <length|numel> ...
%! addedge (digraph ([1 2], [2 3], [10 20]), [3 1], [1 3], [1 2 3]);

## Weighted w applied to unweighted non-empty G errors.
%!error <unweighted|weighted> ...
%! addedge (digraph ([1 2], [2 3]), 3, 1, 5);

## Non-graph input is an error.
%!error <graph or digraph> ...
%! addedge (42, 1, 2);

## Non-graph input (string).
%!error <graph or digraph> ...
%! addedge ("hello", 1, 2);

## nargin < 2 triggers print_usage.
%!error <Invalid call> ...
%! addedge (digraph ());

## EdgeTable must have EndNodes.
%!error <EndNodes> ...
%! ET.Weight = 5;
%! addedge (digraph (3), ET);

## EdgeTable EndNodes must have 2 columns.
%!error <two columns|column> ...
%! ET.EndNodes = [1 2 3];
%! addedge (digraph (4), ET);

## Missing string endpoint on an unnamed graph is an error.
%!error ...
%! addedge (digraph (3), "a", "b");

## Non-integer numeric endpoint errors.
%!error <positive integer|integer> ...
%! addedge (digraph (3), 1.5, 2);
