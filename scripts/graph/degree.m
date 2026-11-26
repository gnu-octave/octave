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
## @deftypefn  {} {@var{d} =} degree (@var{G})
## @deftypefnx {} {@var{d} =} degree (@var{G}, @var{nodeIDs})
## Return the degrees of nodes in the undirected graph @var{G}.
##
## @var{G} must be a @code{graph} object.  @code{degree} is not defined
## on the directed @code{digraph} class; use @code{indegree},
## @code{outdegree}, or their sum there instead.
##
## With one argument, @code{degree (@var{G})} returns a column vector of
## length @code{numnodes (@var{G})} where element @math{i} is the number
## of edge-ends incident to node @math{i}.  Each non-self-loop edge
## contributes one to the degree of each of its endpoints.  Each
## self-loop contributes @strong{two} to the degree of the looped node
## (MATLAB convention).
##
## With two arguments, @code{degree (@var{G}, @var{nodeIDs})} returns
## the degrees of only the specified nodes.  @var{nodeIDs} may be:
##
## @itemize
## @item
## a numeric array of positive integer node indices in the range
## @code{1:numnodes (@var{G})};
## @item
## a character row vector, interpreted as a single node name;
## @item
## a cell array of character vectors, interpreted as node names.
## @end itemize
##
## When @var{nodeIDs} is supplied the shape of @var{d} matches the
## shape of @var{nodeIDs} (a scalar stays scalar, a row stays row, a
## column stays column).  A character row vector is treated as a single
## node name and yields a scalar result.
##
## @example
## @group
## G = graph ([1 1 2 3], [2 3 3 4]);
## degree (G)                  # @result{}  2
##                             ##           2
##                             ##           3
##                             ##           1
## degree (G, 3)               # @result{}  3
## degree (G, [1 4])           # @result{}  2  1
## @end group
## @end example
##
## @seealso{graph, numnodes, numedges, neighbors, indegree, outdegree}
## @end deftypefn

function d = degree (G, nodeIDs)

  ## NOTE: When called with a graph object, Octave's classdef method
  ## dispatch runs the class-internal @code{degree} method and this
  ## free-function body is not reached.  This file exists both as a
  ## canonical documentation target (so @code{help degree} works
  ## outside the context of an instance) and as a fallback that gives a
  ## helpful error for non-graph inputs.

  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif

  if (! isa (G, "graph"))
    error ("Octave:invalid-input-arg", ...
           "degree: G must be a graph object");
  endif

  ## Defensive delegation: if class dispatch ever skips past the free
  ## function (e.g. future subclassing edge cases) route back to the
  ## class method via dot notation, which is always class-dispatched.
  if (nargin == 1)
    d = G.degree ();
  else
    d = G.degree (nodeIDs);
  endif

endfunction


## ------------------------------------------------------------------
## BIST blocks
## ------------------------------------------------------------------

## Empty graph returns a 0-by-1 column.
%!test
%! G = graph ();
%! d = degree (G);
%! assert (d, zeros (0, 1));

## Edgeless N-node graph returns zeros(N, 1).
%!test
%! G = graph (5);
%! d = degree (G);
%! assert (d, zeros (5, 1));

## Simple graph: correct degrees for every node.
%!test
%! G = graph ([1 1 2 3], [2 3 3 4]);
%! assert (degree (G), [2; 2; 3; 1]);

## No-arg result is a column vector.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! d = degree (G);
%! assert (size (d), [3, 1]);

## No-arg result is class double.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! assert (class (degree (G)), "double");

## Scalar numeric nodeID yields a scalar result.
%!test
%! G = graph ([1 1 2 3], [2 3 3 4]);
%! d = degree (G, 3);
%! assert (d, 3);
%! assert (isscalar (d));

## Row-vector nodeIDs yield row-vector result (shape preserved).
%!test
%! G = graph ([1 1 2 3], [2 3 3 4]);
%! d = degree (G, [1 4]);
%! assert (size (d), [1, 2]);
%! assert (d, [2 1]);

## Column-vector nodeIDs yield column-vector result (shape preserved).
%!test
%! G = graph ([1 1 2 3], [2 3 3 4]);
%! d = degree (G, [1; 4]);
%! assert (size (d), [2, 1]);
%! assert (d, [2; 1]);

## 2-D numeric nodeIDs array preserves shape.
%!test
%! G = graph ([1 1 2 3], [2 3 3 4]);
%! d = degree (G, [1 2; 3 4]);
%! assert (size (d), [2, 2]);
%! assert (d, [2 2; 3 1]);

## Empty numeric nodeIDs yields empty result of same shape.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! d = degree (G, []);
%! assert (isempty (d));

## Self-loop: contributes 2 to the degree of the looped node (MATLAB
## convention).
%!test
%! G = graph ([1 2 3], [1 3 3]);
%! d = degree (G);
%! assert (d, [2; 1; 3]);

## Single self-loop in isolation: degree of looped node is 2, others 0.
%!test
%! G = graph ([1], [1]);
%! assert (degree (G), 2);

## Self-loop in 3-node graph: node 2 has self-loop (deg 2), others 0.
%!test
%! G = graph ([2], [2], [], 3);
%! assert (degree (G), [0; 2; 0]);

## Self-loop mixed with regular edges: self-loop at 2 (deg +2), plus
## edge (1,2) (deg +1 each), plus edge (2,3) (deg +1 each) -> node 2
## has degree 4.
%!test
%! G = graph ([1 2 2], [2 2 3]);
%! assert (degree (G), [1; 4; 1]);

## Return type is class double for subset form.
%!test
%! G = graph ([1 2], [2 3]);
%! assert (class (degree (G, 2)), "double");

## Path graph: endpoints degree 1, middle degrees 2.
%!test
%! G = graph ([1 2 3 4], [2 3 4 5]);
%! assert (degree (G), [1; 2; 2; 2; 1]);

## Triangle: every node degree 2.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! assert (degree (G), [2; 2; 2]);

## Complete graph K4: every node degree 3.
%!test
%! G = graph ([1 1 1 2 2 3], [2 3 4 3 4 4]);
%! assert (degree (G), [3; 3; 3; 3]);

## Star graph: centre degree high, leaves degree 1.
%!test
%! s = ones (1, 5);
%! t = 2:6;
%! G = graph (s, t);
%! assert (degree (G), [5; 1; 1; 1; 1; 1]);

## Weighted graph: weights are ignored, only edge counts matter.
%!test
%! G = graph ([1 2 3], [2 3 1], [0.5 0.25 0.75]);
%! assert (degree (G), [2; 2; 2]);

## Weighted self-loop: weight ignored, self-loop still contributes 2.
%!test
%! G = graph ([1 2], [1 2], [3.5 7.0]);
%! assert (degree (G), [2; 2]);

## Isolated trailing nodes (N form).
%!test
%! G = graph ([1 2], [2 3], [], 5);
%! assert (degree (G), [1; 2; 1; 0; 0]);

## Named graph + scalar char-row nodeID -> scalar numeric result.
%!test
%! G = graph ([1 1 2], [2 3 3], [], {"alpha", "beta", "gamma"});
%! d = degree (G, "gamma");
%! assert (d, 2);
%! assert (isscalar (d));

## Named graph + 1-element cellstr -> scalar result.
%!test
%! G = graph ([1 1 2], [2 3 3], [], {"alpha", "beta", "gamma"});
%! d = degree (G, {"gamma"});
%! assert (d, 2);

## Named graph + row cellstr preserves row shape.
%!test
%! G = graph ([1 1 2], [2 3 3], [], {"alpha", "beta", "gamma"});
%! d = degree (G, {"alpha", "beta", "gamma"});
%! assert (size (d), [1, 3]);
%! assert (d, [2 2 2]);

## Named graph + column cellstr preserves column shape.
%!test
%! G = graph ([1 1 2], [2 3 3], [], {"alpha", "beta", "gamma"});
%! d = degree (G, {"alpha"; "gamma"});
%! assert (size (d), [2, 1]);
%! assert (d, [2; 2]);

## Named graph + numeric index -> numeric result.
%!test
%! G = graph ([1 1 2], [2 3 3], [], {"alpha", "beta", "gamma"});
%! assert (degree (G, 3), 2);

## Adjacency-matrix constructor round-trip.
%!test
%! A = [0 1 1; 1 0 1; 1 1 0];
%! G = graph (A);
%! assert (degree (G), [2; 2; 2]);

## Adjacency with self-loop on diagonal -> diagonal contributes 2.
%!test
%! A = [1 1 0; 1 0 1; 0 1 0];
%! G = graph (A);
%! ## Node 1: self-loop (+2) + edge to 2 (+1) = 3
%! ## Node 2: edges to 1, 3 = 2
%! ## Node 3: edge to 2 = 1
%! assert (degree (G), [3; 2; 1]);

## Siever-style 9-node fixture as undirected (12 edges -> 24 edge-ends):
%!test
%! s = [1 2 3 3 4 5 5 6 7 7 8 9];
%! t = [2 3 2 4 5 6 9 7 8 9 7 4];
%! ## remove the duplicate (3,2)/(2,3) and similar pairs for the
%! ## undirected interpretation -- list only the distinct unordered
%! ## pairs.
%! pairs = unique (sort ([s.', t.'], 2), "rows");
%! G = graph (pairs(:, 1), pairs(:, 2));
%! ## Verify degree sum = 2 * numedges.
%! d = degree (G);
%! assert (sum (d), 2 * numedges (G));

## Sum of degrees equals twice the edge count (handshake lemma).
%!test
%! G = graph ([1 2 3 4], [2 3 4 1]);
%! assert (sum (degree (G)), 2 * numedges (G));

## Multiple self-loops on different nodes.
%!test
%! G = graph ([1 2 3 4], [1 2 3 4]);
%! assert (degree (G), [2; 2; 2; 2]);

## Error: node index out of range (too large).
%!error <invalid node index> degree (graph (3), 4)

## Error: node index out of range (zero).
%!error <invalid node index> degree (graph (3), 0)

## Error: non-integer node index.
%!error <invalid node index> degree (graph (3), 1.5)

## Error: vector with an out-of-range index.
%!error <invalid node index> degree (graph (3), [1 2 4])

## Error: non-existent node name.
%!error <not found> ...
%!   degree (graph ([1 2], [2 3], [], {"a","b","c"}), "z")

## Error: node name given but graph has no names.
%!error <no node names|not found> degree (graph (3), "foo")

## Error: non-graph first argument routes through the free-function guard.
%!error <G must be a graph> degree (3)
%!error <G must be a graph> degree ("hello")
%!error <G must be a graph> degree (digraph ([1 2], [2 3]))
%!error <G must be a graph> degree (digraph (3), 1)

## Error: nargin mismatch.
%!error <Invalid call> degree ()
%!error <too many inputs|Invalid call> degree (graph (3), 1, 2)
