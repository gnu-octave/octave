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
## @deftypefn  {} {@var{s} =} successors (@var{G}, @var{nodeID})
## Return the nodes that are destinations of out-edges from @var{nodeID}
## in the digraph @var{G}.
##
## @var{G} must be a @code{digraph} object.  @code{successors} is not
## defined on the undirected @code{graph} class because an undirected
## neighbour has no direction; use @code{neighbors} there instead.
##
## @var{nodeID} is a @emph{scalar} node identifier:
##
## @itemize
## @item
## A positive integer scalar in the range @code{1:numnodes (@var{G})} is
## treated as a 1-based node index.  In that case @var{s} is a numeric
## column vector of node indices.
## @item
## A character row vector (or a 1-element cell array of strings) is
## treated as a node name, which must appear in @code{@var{G}.Nodes.Name}.
## In that case @var{s} is a column cell array of character vectors
## containing the names of the successor nodes.
## @end itemize
##
## The return type always matches the input type.  An isolated node (no
## out-edges) yields an empty column (shape @code{[0 1]}).  For a
## multigraph, parallel edges from @var{nodeID} to the same destination
## contribute one entry each, so duplicate destinations are possible.
##
## @example
## @group
## G = digraph ([1 1 2 3], [2 3 3 1]);
## successors (G, 1)
##    @result{} 2
##       3
## successors (G, 2)
##    @result{} 3
## @end group
## @end example
##
## @seealso{digraph, predecessors, neighbors, outedges, indegree, outdegree}
## @end deftypefn

function s = successors (G, nodeID)

  ## NOTE: When called with a digraph object, Octave's classdef method
  ## dispatch runs the class-internal @code{successors} method and this
  ## free-function body is not reached.  This file exists both as a
  ## canonical documentation target (so @code{help successors} works
  ## outside the context of an instance) and as a fallback that gives a
  ## helpful error for non-digraph inputs.

  if (nargin != 2)
    print_usage ();
  endif

  if (! isa (G, "digraph"))
    error ("Octave:invalid-input-arg", ...
           "successors: G must be a digraph object");
  endif

  ## Defensive delegation: if class dispatch ever skips past the free
  ## function (e.g. future subclassing edge cases) route back to the
  ## class method via dot notation, which is always class-dispatched.
  s = G.successors (nodeID);

endfunction


## ------------------------------------------------------------------
## BIST blocks
## ------------------------------------------------------------------

## Empty successors set (isolated node).
%!test
%! G = digraph (3);
%! s = successors (G, 1);
%! assert (s, zeros (0, 1));

## Single successor.
%!test
%! G = digraph ([1], [2]);
%! s = successors (G, 1);
%! assert (s, 2);

## Multiple successors returned in increasing index order.
%!test
%! G = digraph ([1 1 1], [3 2 4]);
%! s = successors (G, 1);
%! assert (s, [2; 3; 4]);

## Column-vector shape.
%!test
%! G = digraph ([1 1], [2 3]);
%! s = successors (G, 1);
%! assert (size (s), [2, 1]);

## Numeric input returns numeric output (class double).
%!test
%! G = digraph ([1 2], [2 3]);
%! s = successors (G, 1);
%! assert (class (s), "double");

## Node with no out-edges on a populated digraph.
%!test
%! G = digraph ([1 2], [2 3]);
%! s = successors (G, 3);
%! assert (s, zeros (0, 1));

## Self-loop: node is its own successor.
%!test
%! G = digraph ([1 2], [1 3]);
%! s = successors (G, 1);
%! assert (s, 1);

## Siever-style fixture (9 nodes, 12 edges) — successors of node 3.
%!test
%! G = digraph ([1 2 3 3 4 5 5 6 7 7 8 9], ...
%!              [2 3 2 4 5 6 9 7 8 9 7 4]);
%! s = successors (G, 3);
%! assert (s, [2; 4]);

## Weighted digraph: successors ignores weights (only endpoints).
%!test
%! G = digraph ([1 1 1], [2 3 4], [0.5 0.25 0.75]);
%! s = successors (G, 1);
%! assert (s, [2; 3; 4]);

## Named digraph + string node name -> cellstr result.
%!test
%! G = digraph ([1 1 2], [2 3 3], [], {"alpha", "beta", "gamma"});
%! s = successors (G, "alpha");
%! assert (iscellstr (s));
%! assert (s, {"beta"; "gamma"});

## Named digraph + 1-element cellstr node name -> cellstr result.
%!test
%! G = digraph ([1 1 2], [2 3 3], [], {"alpha", "beta", "gamma"});
%! s = successors (G, {"alpha"});
%! assert (iscellstr (s));
%! assert (s, {"beta"; "gamma"});

## Named digraph + numeric index -> numeric result (type matches input).
%!test
%! G = digraph ([1 1 2], [2 3 3], [], {"alpha", "beta", "gamma"});
%! s = successors (G, 1);
%! assert (class (s), "double");
%! assert (s, [2; 3]);

## Named digraph + node-name input + isolated node -> empty cellstr column.
%!test
%! G = digraph ([1 1], [2 3], [], {"a", "b", "c", "d"});
%! s = successors (G, "d");
%! assert (iscell (s));
%! assert (size (s), [0, 1]);

## Multigraph: parallel edges produce duplicate destinations.
%!test
%! G = digraph ([1 1 1 2], [2 2 3 3], "multigraph");
%! s = successors (G, 1);
%! assert (s, [2; 2; 3]);

## Adjacency-matrix constructor round-trip.
%!test
%! A = sparse ([0 1 1; 0 0 1; 1 0 0]);
%! G = digraph (A);
%! assert (successors (G, 1), [2; 3]);
%! assert (successors (G, 2), 3);
%! assert (successors (G, 3), 1);

## N-node edgeless digraph: every node has no successors.
%!test
%! G = digraph (4);
%! for ii = 1:4
%!   assert (successors (G, ii), zeros (0, 1));
%! endfor

## Error: node index out of range (too large).
%!error <invalid node index> successors (digraph (3), 4)

## Error: node index out of range (zero).
%!error <invalid node index> successors (digraph (3), 0)

## Error: non-integer node index.
%!error <invalid node index> successors (digraph (3), 1.5)

## Error: non-existent node name.
%!error <not found> ...
%!   successors (digraph ([1 2], [2 3], [], {"a","b","c"}), "z")

## Error: node name given but digraph has no names.
%!error <no node names|not found> successors (digraph (3), "foo")

## Error: non-scalar numeric nodeID.
%!error <scalar> successors (digraph (3), [1 2])

## Error: multi-element cellstr nodeID.
%!error <scalar> ...
%!   successors (digraph ([1 2], [2 3], [], {"a","b","c"}), {"a","b"})

## Error: non-graph first argument routes through the free-function guard.
%!error <G must be a digraph> successors (3, 1)
%!error <G must be a digraph> successors ("hello", 1)
%!error <G must be a digraph> successors (graph ([1 2], [2 3]), 1)

## Error: nargin mismatch.
%!error <Invalid call> successors ()
%!error <Invalid call> successors (digraph (3))
