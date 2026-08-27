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
## @deftypefn  {} {@var{d} =} outdegree (@var{G})
## @deftypefnx {} {@var{d} =} outdegree (@var{G}, @var{nodeIDs})
## Return the out-degrees of nodes in the digraph @var{G}.
##
## @var{G} must be a @code{digraph} object.  @code{outdegree} is not
## defined on the undirected @code{graph} class; use @code{degree}
## there instead.
##
## With one argument, @code{outdegree (@var{G})} returns a column
## vector of length @code{numnodes (@var{G})} where element @math{i}
## is the number of edges starting at node @math{i}.
##
## With two arguments, @code{outdegree (@var{G}, @var{nodeIDs})}
## returns the out-degrees of only the specified nodes.  @var{nodeIDs}
## may be:
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
## Self-loops contribute one to the out-degree of the looped node.
## For a multigraph, each parallel edge is counted individually, so
## @code{outdegree} reflects the true edge count.
##
## @example
## @group
## G = digraph ([1 2 3 1], [2 3 1 3]);
## outdegree (G)               # @result{}  2
##                             ##           1
##                             ##           1
## outdegree (G, 1)            # @result{}  2
## outdegree (G, [1 2])        # @result{}  2  1
## @end group
## @end example
##
## @seealso{digraph, indegree, degree, successors, predecessors, neighbors}
## @end deftypefn

function d = outdegree (G, nodeIDs)

  ## NOTE: When called with a digraph object, Octave's classdef method
  ## dispatch runs the class-internal @code{outdegree} method and this
  ## free-function body is not reached.  This file exists both as a
  ## canonical documentation target (so @code{help outdegree} works
  ## outside the context of an instance) and as a fallback that gives a
  ## helpful error for non-digraph inputs.

  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif

  if (! isa (G, "digraph"))
    error ("Octave:invalid-input-arg", ...
           "outdegree: G must be a digraph object");
  endif

  ## Defensive delegation: if class dispatch ever skips past the free
  ## function (e.g. future subclassing edge cases) route back to the
  ## class method via dot notation, which is always class-dispatched.
  if (nargin == 1)
    d = G.outdegree ();
  else
    d = G.outdegree (nodeIDs);
  endif

endfunction


## ------------------------------------------------------------------
## BIST blocks
## ------------------------------------------------------------------

## Empty digraph returns a 0-by-1 column.
%!test
%! G = digraph ();
%! d = outdegree (G);
%! assert (d, zeros (0, 1));

## Edgeless N-node digraph returns zeros(N, 1).
%!test
%! G = digraph (5);
%! d = outdegree (G);
%! assert (d, zeros (5, 1));

## Simple digraph: correct out-degrees for every node.
%!test
%! G = digraph ([1 2 3 1], [2 3 1 3]);
%! assert (outdegree (G), [2; 1; 1]);

## No-arg result is a column vector.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! d = outdegree (G);
%! assert (size (d), [3, 1]);

## No-arg result is class double.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! assert (class (outdegree (G)), "double");

## Scalar numeric nodeID yields a scalar result.
%!test
%! G = digraph ([1 2 3 1], [2 3 1 3]);
%! d = outdegree (G, 1);
%! assert (d, 2);
%! assert (isscalar (d));

## Row-vector nodeIDs yield row-vector result (shape preserved).
%!test
%! G = digraph ([1 2 3 1], [2 3 1 3]);
%! d = outdegree (G, [1 2]);
%! assert (size (d), [1, 2]);
%! assert (d, [2 1]);

## Column-vector nodeIDs yield column-vector result (shape preserved).
%!test
%! G = digraph ([1 2 3 1], [2 3 1 3]);
%! d = outdegree (G, [1; 2]);
%! assert (size (d), [2, 1]);
%! assert (d, [2; 1]);

## 2-D numeric nodeIDs array preserves shape.
%!test
%! G = digraph ([1 2 3 1], [2 3 1 3]);
%! d = outdegree (G, [1 2; 3 1]);
%! assert (size (d), [2, 2]);
%! assert (d, [2 1; 1 2]);

## Empty numeric nodeIDs yields empty result.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! d = outdegree (G, []);
%! assert (isempty (d));

## Self-loop: contributes 1 to out-degree of the looped node.
%!test
%! G = digraph ([1 2 3], [1 3 2]);
%! d = outdegree (G);
%! assert (d, [1; 1; 1]);

## Return type is class double for subset form.
%!test
%! G = digraph ([1 2], [2 3]);
%! assert (class (outdegree (G, 2)), "double");

## Siever fixture (9 nodes, 12 edges) out-degrees.
%!test
%! s = [1 2 3 3 4 5 5 6 7 7 8 9];
%! t = [2 3 2 4 5 6 9 7 8 9 7 4];
%! G = digraph (s, t);
%! ## out: 1->{2}=1, 2->{3}=1, 3->{2,4}=2, 4->{5}=1,
%! ## 5->{6,9}=2, 6->{7}=1, 7->{8,9}=2, 8->{7}=1, 9->{4}=1
%! assert (outdegree (G), [1; 1; 2; 1; 2; 1; 2; 1; 1]);

## Weighted digraph: weights are ignored, only edge counts matter.
%!test
%! G = digraph ([1 2 3], [2 3 1], [0.5 0.25 0.75]);
%! assert (outdegree (G), [1; 1; 1]);

## Multigraph: parallel edges counted individually.
%!test
%! G = digraph ([1 1 1 2], [2 2 2 3], "multigraph");
%! assert (outdegree (G), [3; 1; 0]);

## Multigraph self-loop: counts 1 out-degree for the looped node.
%!test
%! G = digraph ([1 1 2], [1 2 3], "multigraph");
%! assert (outdegree (G), [2; 1; 0]);

## Multigraph subset nodeID.
%!test
%! G = digraph ([1 1 1 2], [2 2 2 3], "multigraph");
%! assert (outdegree (G, 1), 3);

## Named digraph + scalar char-row nodeID -> scalar numeric result.
%!test
%! G = digraph ([1 1 2], [2 3 3], [], {"alpha", "beta", "gamma"});
%! d = outdegree (G, "alpha");
%! assert (d, 2);
%! assert (isscalar (d));

## Named digraph + 1-element cellstr -> scalar result.
%!test
%! G = digraph ([1 1 2], [2 3 3], [], {"alpha", "beta", "gamma"});
%! d = outdegree (G, {"alpha"});
%! assert (d, 2);

## Named digraph + row cellstr preserves row shape.
%!test
%! G = digraph ([1 1 2], [2 3 3], [], {"alpha", "beta", "gamma"});
%! d = outdegree (G, {"alpha", "beta", "gamma"});
%! assert (size (d), [1, 3]);
%! assert (d, [2 1 0]);

## Named digraph + column cellstr preserves column shape.
%!test
%! G = digraph ([1 1 2], [2 3 3], [], {"alpha", "beta", "gamma"});
%! d = outdegree (G, {"alpha"; "gamma"});
%! assert (size (d), [2, 1]);
%! assert (d, [2; 0]);

## Named digraph + numeric index -> numeric result.
%!test
%! G = digraph ([1 1 2], [2 3 3], [], {"alpha", "beta", "gamma"});
%! assert (outdegree (G, 1), 2);

## Adjacency-matrix constructor round-trip.
%!test
%! A = sparse ([0 1 1; 0 0 1; 1 0 0]);
%! G = digraph (A);
%! assert (outdegree (G), [2; 1; 1]);

## Error: node index out of range (too large).
%!error <invalid node index> outdegree (digraph (3), 4)

## Error: node index out of range (zero).
%!error <invalid node index> outdegree (digraph (3), 0)

## Error: non-integer node index.
%!error <invalid node index> outdegree (digraph (3), 1.5)

## Error: vector with an out-of-range index.
%!error <invalid node index> outdegree (digraph (3), [1 2 4])

## Error: non-existent node name.
%!error <not found> ...
%!   outdegree (digraph ([1 2], [2 3], [], {"a","b","c"}), "z")

## Error: node name given but digraph has no names.
%!error <no node names|not found> outdegree (digraph (3), "foo")

## Error: non-digraph first argument routes through the free-function guard.
%!error <G must be a digraph> outdegree (3)
%!error <G must be a digraph> outdegree ("hello")
%!error <G must be a digraph> outdegree (graph ([1 2], [2 3]))
%!error <G must be a digraph> outdegree (graph (3), 1)

## Error: nargin mismatch.
%!error <Invalid call> outdegree ()
%!error <too many inputs|Invalid call> outdegree (digraph (3), 1, 2)
