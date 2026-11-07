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
## @deftypefn  {} {@var{H} =} addnode (@var{G}, @var{N})
## @deftypefnx {} {@var{H} =} addnode (@var{G}, @var{NodeNames})
## @deftypefnx {} {@var{H} =} addnode (@var{G}, @var{NodeTable})
## Append nodes to the graph or digraph @var{G} and return the new graph
## @var{H}.
##
## @var{G} must be a @code{graph} or @code{digraph} object.  The second
## argument selects the variant:
##
## @table @asis
## @item @var{N} -- a non-negative integer scalar
## Append @var{N} nodes with no incident edges.  If @var{G} already has
## node names, new nodes are auto-named @qcode{"NodeK"}, where @var{K}
## is chosen to avoid collisions with existing names (starting from
## @code{numnodes (@var{G}) + 1}).  If @var{G} has no node names, the
## result remains unnamed.
##
## @item @var{NodeNames} -- a cell array of strings or a single string
## Append nodes with the given names.  Names must be unique among
## themselves and must not collide with any existing node name.  If
## @var{G} has no node names prior to the call, its existing nodes are
## auto-named @qcode{"NodeK"} so that the returned graph is fully
## named.
##
## @item @var{NodeTable} -- a scalar struct (MATLAB table surrogate)
## Append nodes whose attributes are taken from @var{NodeTable}.  An
## optional @code{Name} field (cellstr) provides the new node names.
## Other fields contribute extra node-attribute columns whose length
## must match the row count of @code{Name} (or of each other when
## @code{Name} is absent).  Extra columns already present on @var{G}
## that are missing from @var{NodeTable} are extended with default
## values (@code{0} for numeric, @code{""} for cellstr, @code{false}
## for logical).
## @end table
##
## The graph class @var{G} is preserved (a @code{graph} returns a
## @code{graph}; a @code{digraph} returns a @code{digraph}).  The
## adjacency of existing nodes is unchanged; only the node set grows.
##
## @example
## @group
## G = digraph ([1 2 3], [2 3 1]);
## H = addnode (G, 2);
## numnodes (H)                       # @result{} 5
## numedges (H)                       # @result{} 3
##
## G = graph ([1 2], [2 3], [], @{"a", "b", "c"@});
## H = addnode (G, @{"d", "e"@});
## H.Nodes.Name                       # @result{} @{"a"; "b"; "c"; "d"; "e"@}
##
## NT.Name = @{"x"; "y"@};
## NT.Size = [10; 20];
## H = addnode (G, NT);
## @end group
## @end example
##
## @seealso{graph, digraph, addedge, rmnode, rmedge, numnodes, findnode}
## @end deftypefn

function H = addnode (G, newnodes)

  ## NOTE: When called with a graph or digraph object, Octave's classdef
  ## method dispatch runs the class-internal @code{addnode} method and
  ## this free-function body is not reached.  This file exists both as a
  ## canonical documentation target (so @code{help addnode} works outside
  ## the context of an instance) and as a fallback that gives a helpful
  ## error for non-graph inputs.

  if (nargin != 2)
    print_usage ();
  endif

  if (! isa (G, "graph") && ! isa (G, "digraph"))
    error ("Octave:invalid-input-arg", ...
           "addnode: G must be a graph or digraph object");
  endif

  ## Defensive delegation: classdef method dispatch should intercept any
  ## call with a graph/digraph first arg, but route through dot notation
  ## to be safe.
  H = G.addnode (newnodes);

endfunction


## ------------------------------------------------------------------
## BIST blocks
## ------------------------------------------------------------------

## ---------------- Form 1: addnode(G, N) ---------------------------

## digraph: add 2 unnamed nodes to an unnamed digraph.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! H = addnode (G, 2);
%! assert (numnodes (H), 5);
%! assert (numedges (H), 3);
%! assert (numel (H.Nodes.Name), 0);

## graph: add 2 unnamed nodes to an unnamed graph.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! H = addnode (G, 2);
%! assert (numnodes (H), 5);
%! assert (numedges (H), 3);
%! assert (numel (H.Nodes.Name), 0);

## addnode(G, 0) is a no-op on an unnamed graph.
%!test
%! G = digraph ([1 2], [2 3]);
%! H = addnode (G, 0);
%! assert (numnodes (H), numnodes (G));
%! assert (numedges (H), numedges (G));
%! assert (isequal (H.Edges.EndNodes, G.Edges.EndNodes));

## addnode(G, 0) is a no-op on a named graph.
%!test
%! G = digraph ([1 2], [2 3], [], {"a", "b", "c"});
%! H = addnode (G, 0);
%! assert (numnodes (H), 3);
%! assert (H.Nodes.Name, {"a"; "b"; "c"});

## addnode on an empty digraph: starts at 0, grows to N (unnamed).
%!test
%! G = digraph ();
%! H = addnode (G, 4);
%! assert (numnodes (H), 4);
%! assert (numedges (H), 0);
%! assert (numel (H.Nodes.Name), 0);

## Value semantics: original G is unchanged.
%!test
%! G = digraph ([1 2], [2 3]);
%! H = addnode (G, 3);
%! assert (numnodes (G), 3);  # G unchanged
%! assert (numnodes (H), 6);

## Named digraph: addnode(G, N) appends auto-names "NodeK".
%!test
%! G = digraph ([1 2], [2 3], [], {"a", "b", "c"});
%! H = addnode (G, 2);
%! assert (numnodes (H), 5);
%! assert (H.Nodes.Name, {"a"; "b"; "c"; "Node4"; "Node5"});

## Named graph: addnode(G, N) appends auto-names "NodeK".
%!test
%! G = graph ([1 2], [2 3], [], {"a", "b", "c"});
%! H = addnode (G, 3);
%! assert (numnodes (H), 6);
%! assert (H.Nodes.Name, {"a"; "b"; "c"; "Node4"; "Node5"; "Node6"});

## Auto-name collision: NodeK is bumped until unused.
%!test
%! G = digraph ([1 2], [2 3], [], {"Node4", "Node5", "c"});
%! H = addnode (G, 2);
%! ## Existing names: Node4, Node5, c.  New nodes need unused NodeK.
%! ## Start from numnodes+1 = 4; Node4 taken, try Node6 (next unused);
%! ## actually we increment until we find a free slot for each new
%! ## node.  The exact choice is Node6, Node7 (skipping Node4, Node5).
%! assert (numel (H.Nodes.Name), 5);
%! assert (H.Nodes.Name(1:3), {"Node4"; "Node5"; "c"});
%! assert (! ismember (H.Nodes.Name(4), H.Nodes.Name(1:3)));
%! assert (! ismember (H.Nodes.Name(5), H.Nodes.Name(1:4)));
%! assert (all (strncmp (H.Nodes.Name(4:5), "Node", 4)));

## ---------------- Form 2: addnode(G, cellstr) ----------------------

## Append 2 named nodes to a named digraph.
%!test
%! G = digraph ([1 2], [2 3], [], {"a", "b", "c"});
%! H = addnode (G, {"d", "e"});
%! assert (numnodes (H), 5);
%! assert (H.Nodes.Name, {"a"; "b"; "c"; "d"; "e"});
%! assert (numedges (H), 2);

## Append 2 named nodes to a named graph.
%!test
%! G = graph ([1 2], [2 3], [], {"a", "b", "c"});
%! H = addnode (G, {"d", "e"});
%! assert (numnodes (H), 5);
%! assert (H.Nodes.Name, {"a"; "b"; "c"; "d"; "e"});
%! assert (numedges (H), 2);

## Single-string form: addnode(G, "name") adds one named node.
%!test
%! G = digraph ([1 2], [2 3], [], {"a", "b", "c"});
%! H = addnode (G, "d");
%! assert (numnodes (H), 4);
%! assert (H.Nodes.Name, {"a"; "b"; "c"; "d"});

## Append to an unnamed graph: existing nodes get auto-names.
%!test
%! G = digraph ([1 2], [2 3]);  # 3 nodes, unnamed
%! H = addnode (G, {"d", "e"});
%! assert (numnodes (H), 5);
%! assert (H.Nodes.Name, {"Node1"; "Node2"; "Node3"; "d"; "e"});

## Append to an empty graph.
%!test
%! G = digraph ();
%! H = addnode (G, {"a", "b"});
%! assert (numnodes (H), 2);
%! assert (H.Nodes.Name, {"a"; "b"});
%! assert (numedges (H), 0);

## Column-cell input is accepted.
%!test
%! G = digraph ([1 2], [2 3], [], {"a", "b", "c"});
%! H = addnode (G, {"d"; "e"});
%! assert (H.Nodes.Name, {"a"; "b"; "c"; "d"; "e"});

## Edges unchanged after addnode with names.
%!test
%! G = digraph ([1 2 3], [2 3 1], [10 20 30]);
%! H = addnode (G, {"d", "e"});
%! assert (H.Edges.EndNodes, [1 2; 2 3; 3 1]);
%! assert (H.Edges.Weight, [10; 20; 30]);

## ---------------- Form 3: addnode(G, NodeTable) -------------------

## NodeTable with Name column: becomes new node names.
%!test
%! G = digraph ([1 2], [2 3], [], {"a", "b", "c"});
%! NT.Name = {"d"; "e"};
%! H = addnode (G, NT);
%! assert (numnodes (H), 5);
%! assert (H.Nodes.Name, {"a"; "b"; "c"; "d"; "e"});

## NodeTable with extra columns: stored as new node_attrs.
%!test
%! G = digraph ([1 2], [2 3], [], {"a", "b", "c"});
%! NT.Name = {"d"; "e"};
%! NT.Size = [40; 50];
%! H = addnode (G, NT);
%! assert (numnodes (H), 5);
%! assert (H.Nodes.Name, {"a"; "b"; "c"; "d"; "e"});
%! ## G had no Size column; existing nodes get default 0.
%! assert (H.Nodes.Size, [0; 0; 0; 40; 50]);

## NodeTable must agree with existing node_attrs columns when present.
## G has Size column via EdgeTable/NodeTable constructor; addnode
## NT.Size continues it.
%!test
%! ET.EndNodes = [1 2; 2 3];
%! NT0.Name = {"a"; "b"; "c"};
%! NT0.Size = [10; 20; 30];
%! G = digraph (ET, NT0);
%! NT.Name = {"d"; "e"};
%! NT.Size = [40; 50];
%! H = addnode (G, NT);
%! assert (H.Nodes.Name, {"a"; "b"; "c"; "d"; "e"});
%! assert (H.Nodes.Size, [10; 20; 30; 40; 50]);

## NodeTable without Name on a named graph: auto-names new nodes.
%!test
%! G = digraph ([1 2], [2 3], [], {"a", "b", "c"});
%! NT.Size = [40; 50];
%! H = addnode (G, NT);
%! assert (numnodes (H), 5);
%! assert (H.Nodes.Name, {"a"; "b"; "c"; "Node4"; "Node5"});
%! assert (H.Nodes.Size, [0; 0; 0; 40; 50]);

## NodeTable on graph class.
%!test
%! G = graph ([1 2], [2 3], [], {"a", "b", "c"});
%! NT.Name = {"d"; "e"};
%! H = addnode (G, NT);
%! assert (numnodes (H), 5);
%! assert (H.Nodes.Name, {"a"; "b"; "c"; "d"; "e"});

## NodeTable with cellstr extra column.
%!test
%! G = digraph ([1 2], [2 3], [], {"a", "b", "c"});
%! NT.Name = {"d"; "e"};
%! NT.Kind = {"leaf"; "root"};
%! H = addnode (G, NT);
%! assert (H.Nodes.Kind, {""; ""; ""; "leaf"; "root"});

## ---------------- Multigraph path ---------------------------------

## Multigraph digraph: addnode preserves edge storage.
%!test
%! G = digraph ([1 1 2], [2 2 3], "multigraph");
%! H = addnode (G, 2);
%! assert (numnodes (H), 5);
%! assert (numedges (H), 3);
%! assert (ismultigraph (H), true);

## Multigraph addnode with names.
%!test
%! G = digraph ([1 1 2], [2 2 3], [], {"a", "b", "c"}, "multigraph");
%! H = addnode (G, {"d", "e"});
%! assert (numnodes (H), 5);
%! assert (H.Nodes.Name, {"a"; "b"; "c"; "d"; "e"});
%! assert (ismultigraph (H), true);

## ---------------- Class preservation -------------------------------

## Return class: digraph -> digraph.
%!test
%! G = digraph ([1 2], [2 3]);
%! H = addnode (G, 1);
%! assert (isa (H, "digraph"));
%! assert (! isa (H, "graph"));

## Return class: graph -> graph.
%!test
%! G = graph ([1 2], [2 3]);
%! H = addnode (G, 1);
%! assert (isa (H, "graph"));
%! assert (! isa (H, "digraph"));

## ---------------- Adjacency round-trip -----------------------------

## The adjacency submatrix for original indices is unchanged.
%!test
%! G = digraph ([1 2 3], [2 3 1], [10 20 30]);
%! A0 = full (adjacency (G, "weighted"));
%! H = addnode (G, 2);
%! A1 = full (adjacency (H, "weighted"));
%! assert (size (A1), [5, 5]);
%! assert (A1(1:3, 1:3), A0);
%! assert (A1(4:5, :), zeros (2, 5));
%! assert (A1(:, 4:5), zeros (5, 2));

## ---------------- Dot-notation dispatch ----------------------------

## G.addnode(N) works via classdef method.
%!test
%! G = digraph ([1 2], [2 3]);
%! H = G.addnode (2);
%! assert (numnodes (H), 5);

## G.addnode(cellstr) works via classdef method.
%!test
%! G = graph ([1 2], [2 3], [], {"a", "b", "c"});
%! H = G.addnode ({"d", "e"});
%! assert (H.Nodes.Name, {"a"; "b"; "c"; "d"; "e"});

## ---------------- Error handling -----------------------------------

## Duplicate new names among themselves.
%!error <duplicate> ...
%! G = digraph ([1 2], [2 3], [], {"a", "b", "c"});
%! addnode (G, {"d", "d"});

## Duplicate: new name already exists in G.
%!error <already exists|duplicate> ...
%! G = digraph ([1 2], [2 3], [], {"a", "b", "c"});
%! addnode (G, {"b", "d"});

## Negative N is an error.
%!error <non-negative integer> ...
%! addnode (digraph (), -1);

## Non-integer N is an error.
%!error <non-negative integer> ...
%! addnode (digraph (), 1.5);

## Non-scalar numeric N is an error (must be scalar).
%!error <non-negative integer|cellstr> ...
%! addnode (digraph (), [1 2 3]);

## Non-cellstr cell input is an error.
%!error <cell array of strings|cellstr> ...
%! addnode (digraph (), {1, 2});

## NodeTable Name must be cellstr.
%!error <Name must be> ...
%! G = digraph ();
%! NT.Name = [1; 2];
%! addnode (G, NT);

## NodeTable columns must match in length.
%!error <length|rows> ...
%! G = digraph ();
%! NT.Name = {"a"; "b"};
%! NT.Size = [1; 2; 3];
%! addnode (G, NT);

## Non-graph input is an error.
%!error <graph or digraph> ...
%! addnode (42, 2);

## Non-graph input (string).
%!error <graph or digraph> ...
%! addnode ("hello", 2);

## nargin < 2 triggers print_usage.
%!error <Invalid call> ...
%! addnode (digraph ());

## Unsupported second argument type (logical).
%!error ...
%! addnode (digraph (), true);
