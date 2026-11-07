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
## @deftypefn {} {@var{n} =} edgecount (@var{G}, @var{s}, @var{t})
## Count the edges between node pairs @code{(@var{s}(i), @var{t}(i))}
## in the graph or digraph @var{G}.
##
## @var{G} must be a @code{graph} or @code{digraph} object.  @var{s} and
## @var{t} must have the same number of elements and may be numeric node
## indices, a char row vector (single node name), or a cell array of
## strings.  Each entry @code{@var{n}(i)} is the number of edges from
## @code{@var{s}(i)} to @code{@var{t}(i)} (for a @code{digraph}) or
## between @code{@var{s}(i)} and @code{@var{t}(i)} (for an undirected
## @code{graph}).
##
## For a simple graph or digraph (no parallel edges) every element of
## @var{n} is @code{0} or @code{1}.  For a multigraph (see the
## @qcode{'multigraph'} flag to @code{digraph}), elements of @var{n}
## may be any non-negative integer counting parallel edges in lock-step
## with @code{G.Edges}.
##
## Node-name lookups that fail contribute @code{0} (matching the
## @code{findnode}/@code{findedge} convention: unknown names are not an
## error).  The result is a scalar when @var{s} and @var{t} are scalars,
## and a column vector otherwise.
##
## @example
## @group
## G = digraph ([1 1 2], [2 2 3], "multigraph");
## edgecount (G, 1, 2)             # @result{} 2
## edgecount (G, 2, 3)             # @result{} 1
## edgecount (G, [1 2], [2 3])     # @result{} [2; 1]
##
## H = graph ([1 2 3], [2 3 1]);
## edgecount (H, 1, 2)             # @result{} 1
## edgecount (H, 2, 1)             # @result{} 1 (undirected)
## edgecount (H, 1, 1)             # @result{} 0
## @end group
## @end example
##
## @seealso{graph, digraph, findedge, findnode, numedges, ismultigraph}
## @end deftypefn

function n = edgecount (varargin)

  ## NOTE: When called with a graph or digraph object, Octave's classdef
  ## method dispatch runs the class-internal @code{edgecount} method and
  ## this free-function body is not reached.  This file exists both as a
  ## canonical documentation target (so @code{help edgecount} works
  ## outside the context of an instance) and as a fallback that gives a
  ## helpful error for non-graph inputs.

  if (nargin != 3)
    print_usage ();
  endif

  G = varargin{1};
  s = varargin{2};
  t = varargin{3};

  if (! isa (G, "graph") && ! isa (G, "digraph"))
    error ("Octave:invalid-input-arg", ...
           "edgecount: G must be a graph or digraph object");
  endif

  ## Defensive delegation: classdef method dispatch should intercept any
  ## call with a graph/digraph first arg, but we route through dot
  ## notation to be safe.
  n = G.edgecount (s, t);

endfunction


## ------------------------------------------------------------------
## BIST blocks
## ------------------------------------------------------------------

## ---------------- Basic simple-graph/digraph counts --------------

## Simple digraph, scalar (s, t) with an existing edge -> 1.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! assert (edgecount (G, 1, 2), 1);

## Simple digraph, scalar (s, t) with a missing edge -> 0.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! assert (edgecount (G, 1, 3), 0);

## Simple digraph: reverse direction does NOT match.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! assert (edgecount (G, 2, 1), 0);

## Result class is double.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! assert (class (edgecount (G, 1, 2)), "double");

## Scalar result is scalar-shaped.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! n = edgecount (G, 1, 2);
%! assert (isscalar (n));
%! assert (size (n), [1, 1]);

## Simple graph, scalar (s, t) -> 1 for existing edge.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! assert (edgecount (G, 1, 2), 1);

## Simple graph: reverse direction DOES match (undirected).
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! assert (edgecount (G, 2, 1), 1);

## Simple graph: missing edge -> 0.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! ## 3-cycle: (1,2), (2,3), (1,3) all present.
%! ## Add a 4th isolated node, no edge (1,4).
%! H = graph ([1 2 3], [2 3 1], [], 4);
%! assert (edgecount (H, 1, 4), 0);

## Self-loop counts (digraph).
%!test
%! G = digraph ([1 2 3], [2 3 3]);
%! assert (edgecount (G, 3, 3), 1);

## Self-loop counts (graph).
%!test
%! G = graph ([1 2 3], [2 3 3]);
%! assert (edgecount (G, 3, 3), 1);

## No self-loop present -> 0.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! assert (edgecount (G, 2, 2), 0);

## Edgeless N-node digraph: count is 0 everywhere.
%!test
%! G = digraph (5);
%! assert (edgecount (G, 1, 2), 0);
%! assert (edgecount (G, 3, 3), 0);

## Edgeless N-node graph: count is 0 everywhere.
%!test
%! G = graph (4);
%! assert (edgecount (G, 1, 2), 0);

## ---------------- Multigraph counts ------------------------------

## Multigraph with two parallel edges (1,2) -> count is 2.
%!test
%! G = digraph ([1 1 2], [2 2 3], "multigraph");
%! assert (edgecount (G, 1, 2), 2);

## Multigraph: non-duplicated edge (2,3) still returns 1.
%!test
%! G = digraph ([1 1 2], [2 2 3], "multigraph");
%! assert (edgecount (G, 2, 3), 1);

## Multigraph: reverse direction still does NOT match (digraph).
%!test
%! G = digraph ([1 1 2], [2 2 3], "multigraph");
%! assert (edgecount (G, 2, 1), 0);

## Multigraph: missing edge returns 0.
%!test
%! G = digraph ([1 1 2], [2 2 3], "multigraph");
%! assert (edgecount (G, 1, 3), 0);

## Multigraph with 4 parallel edges (1,2).
%!test
%! G = digraph ([1 1 1 1 2], [2 2 2 2 3], "multigraph");
%! assert (edgecount (G, 1, 2), 4);
%! assert (edgecount (G, 2, 3), 1);

## Multigraph self-loop, 3 parallel.
%!test
%! G = digraph ([1 1 1 2], [1 1 1 3], "multigraph");
%! assert (edgecount (G, 1, 1), 3);
%! assert (edgecount (G, 2, 3), 1);

## ---------------- Vectorized queries -----------------------------

## Vector (s, t) returns column vector of counts.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! n = edgecount (G, [1 2 3], [2 3 1]);
%! assert (size (n), [3, 1]);
%! assert (n, [1; 1; 1]);

## Vector (s, t) with mixed hits and misses.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! n = edgecount (G, [1 1 2], [2 3 3]);
%! assert (n, [1; 0; 1]);

## Column-vector inputs accepted.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! n = edgecount (G, [1; 2; 3], [2; 3; 1]);
%! assert (size (n), [3, 1]);
%! assert (n, [1; 1; 1]);

## Mixed row/column vector inputs work.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! n = edgecount (G, [1 2 3], [2; 3; 1]);
%! assert (n, [1; 1; 1]);

## Empty (s, t) input returns zeros(0, 1).
%!test
%! G = digraph ([1 2], [2 3]);
%! n = edgecount (G, [], []);
%! assert (size (n), [0, 1]);
%! assert (class (n), "double");

## Vectorized multigraph query: parallel edges counted per slot.
%!test
%! G = digraph ([1 1 2 3 3], [2 2 3 4 4], "multigraph");
%! n = edgecount (G, [1 2 3 1], [2 3 4 1]);
%! assert (n, [2; 1; 2; 0]);

## Vectorized on graph (undirected): reverse pair and normal pair
## give identical counts.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! n_fwd = edgecount (G, [1 2 3], [2 3 1]);
%! n_rev = edgecount (G, [2 3 1], [1 2 3]);
%! assert (n_fwd, n_rev);

## 2-D matrix inputs: flattened column-major, result is a column
## vector of length numel(s).
%!test
%! G = digraph ([1 2 3 4], [2 3 4 1]);
%! n = edgecount (G, [1 2; 3 4], [2 3; 4 1]);
%! assert (size (n), [4, 1]);
%! assert (n, [1; 1; 1; 1]);

## Integer class inputs coerce to double.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! n = edgecount (G, int32 (1), int32 (2));
%! assert (n, 1);
%! assert (class (n), "double");

## ---------------- Named graphs -----------------------------------

## String endpoints on named digraph.
%!test
%! G = digraph ([1 2 3], [2 3 1], [], {"alpha", "beta", "gamma"});
%! assert (edgecount (G, "alpha", "beta"), 1);

## String endpoint: missing name yields 0 (not an error).
%!test
%! G = digraph ([1 2 3], [2 3 1], [], {"alpha", "beta", "gamma"});
%! assert (edgecount (G, "alpha", "zed"), 0);
%! assert (edgecount (G, "zed", "alpha"), 0);

## Cellstr endpoints on named digraph.
%!test
%! G = digraph ([1 2 3], [2 3 1], [], {"alpha", "beta", "gamma"});
%! n = edgecount (G, {"alpha", "beta", "gamma"}, {"beta", "gamma", "alpha"});
%! assert (n, [1; 1; 1]);

## Cellstr endpoints with missing names give 0 per pair.
%!test
%! G = digraph ([1 2 3], [2 3 1], [], {"alpha", "beta", "gamma"});
%! n = edgecount (G, {"alpha", "alpha"}, {"beta", "zed"});
%! assert (n, [1; 0]);

## Named graph (undirected): string reverse pair matches.
%!test
%! G = graph ([1 2 3], [2 3 1], [], {"a", "b", "c"});
%! assert (edgecount (G, "b", "a"), edgecount (G, "a", "b"));

## Graph (undirected) multigraph-equivalent query: a simple graph
## has at most one edge per undirected pair, so edgecount is 0/1.
## Here we assert both the canonical and reverse pair give 1.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! assert (edgecount (G, 1, 2), 1);
%! assert (edgecount (G, 2, 1), 1);

## Multigraph digraph lookup with numeric indices: count parallel
## edges regardless of lookup order.
%!test
%! G = digraph ([1 1 2], [2 2 3], "multigraph");
%! assert (edgecount (G, 1, 2), 2);
%! assert (edgecount (G, 2, 3), 1);
%! assert (edgecount (G, 2, 1), 0);

## Name lookup on unnamed graph: missing names yield 0.
%!test
%! G = digraph (3);
%! assert (edgecount (G, "a", "b"), 0);

## Mixed string/numeric input.
%!test
%! G = digraph ([1 2 3], [2 3 1], [], {"a", "b", "c"});
%! assert (edgecount (G, "a", 2), 1);
%! assert (edgecount (G, 1, "b"), 1);

## ---------------- Equivalence with findedge ----------------------

## For a simple graph/digraph, edgecount is 0/1 and matches
## (findedge != 0) exactly.
%!test
%! G = digraph ([1 2 3 4], [2 3 4 1]);
%! s = [1 2 3 4 1 2];
%! t = [2 3 4 1 3 4];
%! n = edgecount (G, s, t);
%! idx = findedge (G, s, t);
%! assert (n, double (idx != 0));

## ---------------- Errors -----------------------------------------

## Error: s and t length mismatch.
%!error <length> edgecount (digraph ([1 2], [2 3]), [1 2], [2])

## Error: out-of-range numeric endpoint.
%!error <invalid node|exceed|out of range> ...
%!   edgecount (digraph ([1 2], [2 3]), 1, 99)

## Error: zero-valued numeric endpoint.
%!error <invalid node|positive|out of range> ...
%!   edgecount (digraph ([1 2], [2 3]), 0, 1)

## Error: non-integer numeric endpoint.
%!error <invalid node|integer> ...
%!   edgecount (digraph ([1 2], [2 3]), 1, 1.5)

## Error: NaN as numeric endpoint.
%!error <invalid node|finite|NaN> ...
%!   edgecount (digraph ([1 2], [2 3]), 1, NaN)

## Error: negative numeric endpoint.
%!error <invalid node|positive|out of range> ...
%!   edgecount (digraph ([1 2], [2 3]), -1, 1)

## Error: non-graph first arg routes through the free-function guard.
%!error <G must be a graph or digraph> edgecount (3, 1, 2)
%!error <G must be a graph or digraph> edgecount ("hello", 1, 2)

## Error: nargin mismatch.
%!error <Invalid call> edgecount ()
%!error <Invalid call> edgecount (digraph (3))
%!error <Invalid call> edgecount (digraph (3), 1)
%!error <Invalid call> edgecount (digraph (3), 1, 2, 3)
