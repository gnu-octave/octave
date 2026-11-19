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
## @deftypefn  {} {@var{d} =} indegree (@var{G})
## @deftypefnx {} {@var{d} =} indegree (@var{G}, @var{nodeIDs})
## Return the in-degrees of nodes in the digraph @var{G}.
##
## @var{G} must be a @code{digraph} object.  @code{indegree} is not
## defined on the undirected @code{graph} class; use @code{degree}
## there instead.
##
## With one argument, @code{indegree (@var{G})} returns a column vector
## of length @code{numnodes (@var{G})} where element @math{i} is the
## number of edges ending at node @math{i}.
##
## With two arguments, @code{indegree (@var{G}, @var{nodeIDs})} returns
## the in-degrees of only the specified nodes.  @var{nodeIDs} may be:
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
## Self-loops contribute one to the in-degree of the looped node.  For
## a multigraph, each parallel edge is counted individually, so
## @code{indegree} reflects the true edge count.
##
## @example
## @group
## G = digraph ([1 2 3 1], [2 3 1 3]);
## indegree (G)                # @result{}  1
##                             ##           1
##                             ##           2
## indegree (G, 3)             # @result{}  2
## indegree (G, [1 3])         # @result{}  1  2
## @end group
## @end example
##
## @seealso{digraph, outdegree, degree, successors, predecessors, neighbors}
## @end deftypefn

function d = indegree (G, nodeIDs)

  ## NOTE: When called with a digraph object, Octave's classdef method
  ## dispatch runs the class-internal @code{indegree} method and this
  ## free-function body is not reached.  This file exists both as a
  ## canonical documentation target (so @code{help indegree} works
  ## outside the context of an instance) and as a fallback that gives a
  ## helpful error for non-digraph inputs.

  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif

  if (! isa (G, "digraph"))
    error ("Octave:invalid-input-arg", ...
           "indegree: G must be a digraph object");
  endif

  ## Defensive delegation: if class dispatch ever skips past the free
  ## function (e.g. future subclassing edge cases) route back to the
  ## class method via dot notation, which is always class-dispatched.
  if (nargin == 1)
    d = G.indegree ();
  else
    d = G.indegree (nodeIDs);
  endif

endfunction


## ------------------------------------------------------------------
## BIST blocks
## ------------------------------------------------------------------

## Empty digraph returns a 0-by-1 column.
%!test
%! G = digraph ();
%! d = indegree (G);
%! assert (d, zeros (0, 1));

## Edgeless N-node digraph returns zeros(N, 1).
%!test
%! G = digraph (5);
%! d = indegree (G);
%! assert (d, zeros (5, 1));

## Simple digraph: correct in-degrees for every node.
%!test
%! G = digraph ([1 2 3 1], [2 3 1 3]);
%! assert (indegree (G), [1; 1; 2]);

## No-arg result is a column vector.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! d = indegree (G);
%! assert (size (d), [3, 1]);

## No-arg result is class double.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! assert (class (indegree (G)), "double");

## Scalar numeric nodeID yields a scalar result.
%!test
%! G = digraph ([1 2 3 1], [2 3 1 3]);
%! d = indegree (G, 3);
%! assert (d, 2);
%! assert (isscalar (d));

## Row-vector nodeIDs yield row-vector result (shape preserved).
%!test
%! G = digraph ([1 2 3 1], [2 3 1 3]);
%! d = indegree (G, [1 3]);
%! assert (size (d), [1, 2]);
%! assert (d, [1 2]);

## Column-vector nodeIDs yield column-vector result (shape preserved).
%!test
%! G = digraph ([1 2 3 1], [2 3 1 3]);
%! d = indegree (G, [1; 3]);
%! assert (size (d), [2, 1]);
%! assert (d, [1; 2]);

## 2-D numeric nodeIDs array preserves shape.
%!test
%! G = digraph ([1 2 3 1], [2 3 1 3]);
%! d = indegree (G, [1 2; 3 1]);
%! assert (size (d), [2, 2]);
%! assert (d, [1 1; 2 1]);

## Empty numeric nodeIDs yields empty result of same shape.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! d = indegree (G, []);
%! assert (isempty (d));

## Self-loop: contributes 1 to in-degree of the looped node.
%!test
%! G = digraph ([1 2 3], [1 3 2]);
%! d = indegree (G);
%! assert (d, [1; 1; 1]);

## Return type is class double for subset form.
%!test
%! G = digraph ([1 2], [2 3]);
%! assert (class (indegree (G, 2)), "double");

## Siever fixture (9 nodes, 12 edges) in-degrees.
%!test
%! s = [1 2 3 3 4 5 5 6 7 7 8 9];
%! t = [2 3 2 4 5 6 9 7 8 9 7 4];
%! G = digraph (s, t);
%! ## node 1: 0 in ; 2: 2 (from 1 and 3) ; 3: 1 ; 4: 2 (3,9) ;
%! ## 5: 1 ; 6: 1 ; 7: 2 (6,8) ; 8: 1 ; 9: 2 (5,7)
%! assert (indegree (G), [0; 2; 1; 2; 1; 1; 2; 1; 2]);

## Weighted digraph: weights are ignored, only edge counts matter.
%!test
%! G = digraph ([1 2 3], [2 3 1], [0.5 0.25 0.75]);
%! assert (indegree (G), [1; 1; 1]);

## Multigraph: parallel edges counted individually.
%!test
%! G = digraph ([1 1 1 2], [2 2 2 3], "multigraph");
%! assert (indegree (G), [0; 3; 1]);

## Multigraph self-loop: counts 1 in-degree for the looped node.
%!test
%! G = digraph ([1 1 2], [1 2 3], "multigraph");
%! assert (indegree (G), [1; 1; 1]);

## Multigraph subset nodeID.
%!test
%! G = digraph ([1 1 1 2], [2 2 2 3], "multigraph");
%! assert (indegree (G, 2), 3);

## Named digraph + scalar char-row nodeID -> scalar numeric result.
%!test
%! G = digraph ([1 1 2], [2 3 3], [], {"alpha", "beta", "gamma"});
%! d = indegree (G, "gamma");
%! assert (d, 2);
%! assert (isscalar (d));

## Named digraph + 1-element cellstr -> scalar result.
%!test
%! G = digraph ([1 1 2], [2 3 3], [], {"alpha", "beta", "gamma"});
%! d = indegree (G, {"gamma"});
%! assert (d, 2);

## Named digraph + row cellstr preserves row shape.
%!test
%! G = digraph ([1 1 2], [2 3 3], [], {"alpha", "beta", "gamma"});
%! d = indegree (G, {"alpha", "beta", "gamma"});
%! assert (size (d), [1, 3]);
%! assert (d, [0 1 2]);

## Named digraph + column cellstr preserves column shape.
%!test
%! G = digraph ([1 1 2], [2 3 3], [], {"alpha", "beta", "gamma"});
%! d = indegree (G, {"alpha"; "gamma"});
%! assert (size (d), [2, 1]);
%! assert (d, [0; 2]);

## Named digraph + numeric index -> numeric result.
%!test
%! G = digraph ([1 1 2], [2 3 3], [], {"alpha", "beta", "gamma"});
%! assert (indegree (G, 3), 2);

## Adjacency-matrix constructor round-trip.
%!test
%! A = sparse ([0 1 1; 0 0 1; 1 0 0]);
%! G = digraph (A);
%! assert (indegree (G), [1; 1; 2]);

## Error: node index out of range (too large).
%!error <invalid node index> indegree (digraph (3), 4)

## Error: node index out of range (zero).
%!error <invalid node index> indegree (digraph (3), 0)

## Error: non-integer node index.
%!error <invalid node index> indegree (digraph (3), 1.5)

## Error: vector with an out-of-range index.
%!error <invalid node index> indegree (digraph (3), [1 2 4])

## Error: non-existent node name.
%!error <not found> ...
%!   indegree (digraph ([1 2], [2 3], [], {"a","b","c"}), "z")

## Error: node name given but digraph has no names.
%!error <no node names|not found> indegree (digraph (3), "foo")

## Error: non-digraph first argument routes through the free-function guard.
%!error <G must be a digraph> indegree (3)
%!error <G must be a digraph> indegree ("hello")
%!error <G must be a digraph> indegree (graph ([1 2], [2 3]))
%!error <G must be a digraph> indegree (graph (3), 1)

## Error: nargin mismatch.
%!error <Invalid call> indegree ()
%!error <too many inputs|Invalid call> indegree (digraph (3), 1, 2)
