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
## @deftypefn  {} {@var{eid} =} inedges (@var{G}, @var{nodeID})
## @deftypefnx {} {[@var{eid}, @var{nid}] =} inedges (@var{G}, @var{nodeID})
## Return the indices of the in-edges of @var{nodeID} in the digraph
## @var{G}.
##
## @var{G} must be a @code{digraph} object.  @code{inedges} is not
## defined on the undirected @code{graph} class because an undirected
## edge has no orientation; on a @code{graph} every incident edge is
## both incoming and outgoing.  See the related @code{neighbors} and
## @code{degree} methods on @code{graph}.
##
## @var{nodeID} is a @emph{scalar} node identifier:
##
## @itemize
## @item
## A positive integer scalar in the range @code{1:numnodes (@var{G})} is
## treated as a 1-based node index.
## @item
## A character row vector (or a 1-element cell array of strings) is
## treated as a node name, which must appear in
## @code{@var{G}.Nodes.Name}.
## @end itemize
##
## @var{eid} is a numeric column vector of the edge indices (1-based,
## referring to row positions in @code{@var{G}.Edges}) for every edge
## whose destination endpoint is @var{nodeID}.  An isolated node (no
## in-edges) yields an empty column of shape @code{[0 1]}.  For a
## @code{multigraph} digraph (see @code{ismultigraph}) parallel edges
## arriving at @var{nodeID} contribute one entry per copy.
##
## When two output arguments are requested, @var{nid} is also returned
## as a column of the @emph{source} node identifiers of those edges
## (the tails of the in-edges).  The type of @var{nid} matches the
## type of @var{nodeID}: a numeric input yields a numeric column, while
## a name input yields a column cell array of character vectors.
##
## @example
## @group
## G = digraph ([1 1 2 3], [2 3 3 1]);
## inedges (G, 3)
##    @result{} 2
##       3
## [eid, nid] = inedges (G, 3);
## eid                                    # @result{} [2; 3]
## nid                                    # @result{} [1; 2]
## @end group
## @end example
##
## @seealso{digraph, outedges, predecessors, neighbors, findedge, indegree}
## @end deftypefn

function [eid, nid] = inedges (G, nodeID)

  ## NOTE: When called with a digraph object, Octave's classdef method
  ## dispatch runs the class-internal @code{inedges} method and this
  ## free-function body is not reached.  This file exists both as a
  ## canonical documentation target (so @code{help inedges} works
  ## outside the context of an instance) and as a fallback that gives a
  ## helpful error for non-digraph inputs.

  if (nargin != 2)
    print_usage ();
  endif

  if (! isa (G, "digraph"))
    error ("Octave:invalid-input-arg", ...
           "inedges: G must be a digraph object");
  endif

  ## Defensive delegation: if class dispatch ever skips past the free
  ## function (e.g. future subclassing edge cases) route back to the
  ## class method via dot notation, which is always class-dispatched.
  if (nargout <= 1)
    eid = G.inedges (nodeID);
  else
    [eid, nid] = G.inedges (nodeID);
  endif

endfunction


## ------------------------------------------------------------------
## BIST blocks
## ------------------------------------------------------------------

## ---------------- Single-output form (eid only) -------------------

## Empty in-edges set on isolated node of an N-node edgeless digraph.
%!test
%! G = digraph (3);
%! eid = inedges (G, 1);
%! assert (eid, zeros (0, 1));

## Single in-edge.
%!test
%! G = digraph (1, 2);
%! eid = inedges (G, 2);
%! assert (eid, 1);

## Single in-edge: result is a column vector.
%!test
%! G = digraph (1, 2);
%! eid = inedges (G, 2);
%! assert (size (eid), [1, 1]);
%! assert (class (eid), "double");

## Multiple in-edges returned in increasing edge-index order.
%!test
%! G = digraph ([1 2 3], [4 4 4]);
%! eid = inedges (G, 4);
%! assert (eid, [1; 2; 3]);

## In-edges of node from sources scattered across edge list.
%!test
%! G = digraph ([1 2 3 4], [3 1 1 1]);
%! eid = inedges (G, 1);
%! ## Lex edges: 1->3, 2->1, 3->1, 4->1.
%! assert (eid, [2; 3; 4]);

## Column-vector shape is enforced even for a single result.
%!test
%! G = digraph ([1 2], [3 3]);
%! eid = inedges (G, 3);
%! assert (size (eid), [2, 1]);

## Node with no in-edges in a populated digraph.
%!test
%! G = digraph ([1 2], [2 3]);
%! eid = inedges (G, 1);
%! assert (eid, zeros (0, 1));

## Self-loop: the self-loop edge is included in inedges of the node.
%!test
%! G = digraph ([1 2], [1 3]);
%! eid = inedges (G, 1);
%! assert (eid, 1);

## Numeric input returns numeric output (class double).
%!test
%! G = digraph ([1 2], [2 3]);
%! eid = inedges (G, 2);
%! assert (class (eid), "double");

## Siever-style fixture (9 nodes, 12 edges) -- in-edges of node 9.
%!test
%! G = digraph ([1 2 3 3 4 5 5 6 7 7 8 9], ...
%!              [2 3 2 4 5 6 9 7 8 9 7 4]);
%! eid = inedges (G, 9);
%! ## Lex edge order:
%! ##   1->2, 2->3, 3->2, 3->4, 4->5, 5->6, 5->9, 6->7,
%! ##   7->8, 7->9, 8->7, 9->4
%! ## Edges with dst==9 are at positions 7 and 10 (5->9 and 7->9).
%! assert (eid, [7; 10]);

## Weighted digraph: inedges ignores weights, only structure matters.
%!test
%! G = digraph ([1 2 3], [4 4 4], [0.5 0.25 0.75]);
%! eid = inedges (G, 4);
%! assert (eid, [1; 2; 3]);

## Adjacency-matrix constructor round-trip.
%!test
%! A = sparse ([0 1 1; 0 0 1; 1 0 0]);
%! G = digraph (A);
%! ## Lex edges: 1->2, 1->3, 2->3, 3->1.
%! assert (inedges (G, 1), 4);
%! assert (inedges (G, 2), 1);
%! assert (inedges (G, 3), [2; 3]);

## N-node edgeless digraph: every node has empty in-edges.
%!test
%! G = digraph (4);
%! for ii = 1:4
%!   assert (inedges (G, ii), zeros (0, 1));
%! endfor

## ---------------- Two-output form (eid, nid) ----------------------

## Two-output form returns sources as numeric column.
%!test
%! G = digraph ([1 2 3], [4 4 4]);
%! [eid, nid] = inedges (G, 4);
%! assert (eid, [1; 2; 3]);
%! assert (nid, [1; 2; 3]);
%! assert (class (nid), "double");
%! assert (size (nid), [3, 1]);

## Two-output form: isolated node yields empty columns.
%!test
%! G = digraph (3);
%! [eid, nid] = inedges (G, 2);
%! assert (eid, zeros (0, 1));
%! assert (nid, zeros (0, 1));

## Two-output form: self-loop's nid is the node itself.
%!test
%! G = digraph ([1 1 2], [1 2 3]);
%! [eid, nid] = inedges (G, 1);
%! assert (eid, 1);
%! assert (nid, 1);

## Two-output form: round-trip with findedge.
%!test
%! G = digraph ([1 1 2 3], [2 3 3 1]);
%! [eid, nid] = inedges (G, 3);
%! ## findedge(G, eid) should give back the endpoint pairs.
%! E = findedge (G, eid);
%! assert (E(:, 1), nid);
%! assert (E(:, 2), [3; 3]);

## ---------------- Named digraph -----------------------------------

## Named digraph + string node name -> nid is a cellstr.
%!test
%! G = digraph ([1 2 3], [3 3 3], [], {"alpha", "beta", "gamma"});
%! [eid, nid] = inedges (G, "gamma");
%! assert (eid, [1; 2; 3]);
%! assert (iscellstr (nid));
%! assert (nid, {"alpha"; "beta"; "gamma"});

## Named digraph + 1-element cellstr name -> nid is a cellstr.
%!test
%! G = digraph ([1 2], [3 3], [], {"alpha", "beta", "gamma"});
%! [eid, nid] = inedges (G, {"gamma"});
%! assert (iscellstr (nid));
%! assert (nid, {"alpha"; "beta"});

## Named digraph + numeric index -> nid stays numeric (matches input).
%!test
%! G = digraph ([1 2], [3 3], [], {"alpha", "beta", "gamma"});
%! [eid, nid] = inedges (G, 3);
%! assert (eid, [1; 2]);
%! assert (class (nid), "double");
%! assert (nid, [1; 2]);

## Named digraph + node-name input + isolated node -> empty cellstr.
%!test
%! G = digraph ([1 1], [2 3], [], {"a", "b", "c", "d"});
%! [eid, nid] = inedges (G, "d");
%! assert (eid, zeros (0, 1));
%! assert (iscell (nid));
%! assert (size (nid), [0, 1]);

## Single-output form on named digraph + name input still returns
## numeric edge indices (eid is always numeric -- only nid varies).
%!test
%! G = digraph ([1 2], [3 3], [], {"alpha", "beta", "gamma"});
%! eid = inedges (G, "gamma");
%! assert (class (eid), "double");
%! assert (eid, [1; 2]);

## ---------------- Multigraph --------------------------------------

## Multigraph: parallel edges produce duplicate edge index entries.
%!test
%! G = digraph ([1 1 2 2], [3 3 3 3], "multigraph");
%! eid = inedges (G, 3);
%! ## Four in-edges at lex-sorted multigraph positions 1, 2, 3, 4.
%! assert (eid, [1; 2; 3; 4]);

## Multigraph two-output form: nid has duplicates for parallel edges.
%!test
%! G = digraph ([1 1 2 2], [3 3 3 3], "multigraph");
%! [eid, nid] = inedges (G, 3);
%! assert (eid, [1; 2; 3; 4]);
%! assert (nid, [1; 1; 2; 2]);

## ---------------- Edge index consistency --------------------------

## findedge round-trip: inedges indices map back to (src, dst).
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! eid = inedges (G, 2);
%! E = findedge (G, eid);
%! assert (E, [1 2]);

## inedges and outedges partition the incident edges of any node.
%!test
%! G = digraph ([1 2 3 3 4], [2 3 1 4 5]);
%! all_edges = sort (unique ([inedges(G, 3); outedges(G, 3)]));
%! ## Edges incident to node 3: 2->3, 3->1, 3->4.
%! ## Lex order: (1,2), (2,3), (3,1), (3,4), (4,5) -> indices 2, 3, 4.
%! assert (all_edges, [2; 3; 4]);

## ---------------- Errors ------------------------------------------

## Error: node index out of range (too large).
%!error <invalid node index> inedges (digraph (3), 4)

## Error: node index out of range (zero).
%!error <invalid node index> inedges (digraph (3), 0)

## Error: non-integer node index.
%!error <invalid node index> inedges (digraph (3), 1.5)

## Error: non-existent node name.
%!error <not found> ...
%!   inedges (digraph ([1 2], [2 3], [], {"a","b","c"}), "z")

## Error: node name given but digraph has no names.
%!error <no node names|not found> inedges (digraph (3), "foo")

## Error: non-scalar numeric nodeID.
%!error <scalar> inedges (digraph (3), [1 2])

## Error: multi-element cellstr nodeID.
%!error <scalar> ...
%!   inedges (digraph ([1 2], [2 3], [], {"a","b","c"}), {"a","b"})

## Error: non-graph first argument routes through the free-function guard.
%!error <G must be a digraph> inedges (3, 1)
%!error <G must be a digraph> inedges ("hello", 1)
%!error <G must be a digraph> inedges (graph ([1 2], [2 3]), 1)

## Error: nargin mismatch.
%!error <Invalid call> inedges ()
%!error <Invalid call> inedges (digraph (3))
