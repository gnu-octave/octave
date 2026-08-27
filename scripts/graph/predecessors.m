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
## @deftypefn  {} {@var{p} =} predecessors (@var{G}, @var{nodeID})
## Return the nodes that are sources of in-edges into @var{nodeID}
## in the digraph @var{G}.
##
## @var{G} must be a @code{digraph} object.  @code{predecessors} is not
## defined on the undirected @code{graph} class because an undirected
## neighbour has no direction; use @code{neighbors} there instead.
##
## @var{nodeID} is a @emph{scalar} node identifier:
##
## @itemize
## @item
## A positive integer scalar in the range @code{1:numnodes (@var{G})} is
## treated as a 1-based node index.  In that case @var{p} is a numeric
## column vector of node indices.
## @item
## A character row vector (or a 1-element cell array of strings) is
## treated as a node name, which must appear in @code{@var{G}.Nodes.Name}.
## In that case @var{p} is a column cell array of character vectors
## containing the names of the predecessor nodes.
## @end itemize
##
## The return type always matches the input type.  A source node (no
## in-edges) yields an empty column (shape @code{[0 1]}).  For a
## multigraph, parallel edges from the same source into @var{nodeID}
## contribute one entry each, so duplicate sources are possible.
##
## @example
## @group
## G = digraph ([1 1 2 3], [2 3 3 1]);
## predecessors (G, 3)
##    @result{} 1
##       2
## predecessors (G, 1)
##    @result{} 3
## @end group
## @end example
##
## @seealso{digraph, successors, neighbors, inedges, indegree, outdegree}
## @end deftypefn

function p = predecessors (G, nodeID)

  ## NOTE: When called with a digraph object, Octave's classdef method
  ## dispatch runs the class-internal @code{predecessors} method and
  ## this free-function body is not reached.  This file exists both as
  ## a canonical documentation target (so @code{help predecessors}
  ## works outside the context of an instance) and as a fallback that
  ## gives a helpful error for non-digraph inputs.

  if (nargin != 2)
    print_usage ();
  endif

  if (! isa (G, "digraph"))
    error ("Octave:invalid-input-arg", ...
           "predecessors: G must be a digraph object");
  endif

  ## Defensive delegation: if class dispatch ever skips past the free
  ## function (e.g. future subclassing edge cases) route back to the
  ## class method via dot notation, which is always class-dispatched.
  p = G.predecessors (nodeID);

endfunction


## ------------------------------------------------------------------
## BIST blocks
## ------------------------------------------------------------------

## Empty predecessors set (source node).
%!test
%! G = digraph (3);
%! p = predecessors (G, 1);
%! assert (p, zeros (0, 1));

## Single predecessor.
%!test
%! G = digraph ([1], [2]);
%! p = predecessors (G, 2);
%! assert (p, 1);

## Multiple predecessors returned in increasing index order.
%!test
%! G = digraph ([2 3 4], [1 1 1]);
%! p = predecessors (G, 1);
%! assert (p, [2; 3; 4]);

## Column-vector shape.
%!test
%! G = digraph ([1 2], [3 3]);
%! p = predecessors (G, 3);
%! assert (size (p), [2, 1]);

## Numeric input returns numeric output (class double).
%!test
%! G = digraph ([1 2], [2 3]);
%! p = predecessors (G, 3);
%! assert (class (p), "double");

## Source node (no in-edges) on a populated digraph.
%!test
%! G = digraph ([1 2], [2 3]);
%! p = predecessors (G, 1);
%! assert (p, zeros (0, 1));

## Self-loop: node is its own predecessor.
%!test
%! G = digraph ([1 2], [1 3]);
%! p = predecessors (G, 1);
%! assert (p, 1);

## Siever-style fixture (9 nodes, 12 edges) — predecessors of node 4.
%!test
%! G = digraph ([1 2 3 3 4 5 5 6 7 7 8 9], ...
%!              [2 3 2 4 5 6 9 7 8 9 7 4]);
%! p = predecessors (G, 4);
%! assert (p, [3; 9]);

## Weighted digraph: predecessors ignores weights (only endpoints).
%!test
%! G = digraph ([1 2 3], [4 4 4], [0.5 0.25 0.75]);
%! p = predecessors (G, 4);
%! assert (p, [1; 2; 3]);

## Named digraph + string node name -> cellstr result.
%!test
%! G = digraph ([1 2 1], [3 3 2], [], {"alpha", "beta", "gamma"});
%! p = predecessors (G, "gamma");
%! assert (iscellstr (p));
%! assert (p, {"alpha"; "beta"});

## Named digraph + 1-element cellstr node name -> cellstr result.
%!test
%! G = digraph ([1 2 1], [3 3 2], [], {"alpha", "beta", "gamma"});
%! p = predecessors (G, {"gamma"});
%! assert (iscellstr (p));
%! assert (p, {"alpha"; "beta"});

## Named digraph + numeric index -> numeric result (type matches input).
%!test
%! G = digraph ([1 2 1], [3 3 2], [], {"alpha", "beta", "gamma"});
%! p = predecessors (G, 3);
%! assert (class (p), "double");
%! assert (p, [1; 2]);

## Named digraph + node-name input + source node -> empty cellstr column.
%!test
%! G = digraph ([1 1], [2 3], [], {"a", "b", "c", "d"});
%! p = predecessors (G, "a");
%! assert (iscell (p));
%! assert (size (p), [0, 1]);

## Multigraph: parallel edges produce duplicate sources.
%!test
%! G = digraph ([1 1 2 2], [3 3 3 3], "multigraph");
%! p = predecessors (G, 3);
%! assert (p, [1; 1; 2; 2]);

## Adjacency-matrix constructor round-trip.
%!test
%! A = sparse ([0 1 1; 0 0 1; 1 0 0]);
%! G = digraph (A);
%! assert (predecessors (G, 1), 3);
%! assert (predecessors (G, 2), 1);
%! assert (predecessors (G, 3), [1; 2]);

## N-node edgeless digraph: every node has no predecessors.
%!test
%! G = digraph (4);
%! for ii = 1:4
%!   assert (predecessors (G, ii), zeros (0, 1));
%! endfor

## Error: node index out of range (too large).
%!error <invalid node index> predecessors (digraph (3), 4)

## Error: node index out of range (zero).
%!error <invalid node index> predecessors (digraph (3), 0)

## Error: non-integer node index.
%!error <invalid node index> predecessors (digraph (3), 1.5)

## Error: non-existent node name.
%!error <not found> ...
%!   predecessors (digraph ([1 2], [2 3], [], {"a","b","c"}), "z")

## Error: node name given but digraph has no names.
%!error <no node names|not found> predecessors (digraph (3), "foo")

## Error: non-scalar numeric nodeID.
%!error <scalar> predecessors (digraph (3), [1 2])

## Error: multi-element cellstr nodeID.
%!error <scalar> ...
%!   predecessors (digraph ([1 2], [2 3], [], {"a","b","c"}), {"a","b"})

## Error: non-graph first argument routes through the free-function guard.
%!error <G must be a digraph> predecessors (3, 1)
%!error <G must be a digraph> predecessors ("hello", 1)
%!error <G must be a digraph> predecessors (graph ([1 2], [2 3]), 1)

## Error: nargin mismatch.
%!error <Invalid call> predecessors ()
%!error <Invalid call> predecessors (digraph (3))
