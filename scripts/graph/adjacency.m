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
## @deftypefn  {} {@var{A} =} adjacency (@var{G})
## @deftypefnx {} {@var{A} =} adjacency (@var{G}, @qcode{"weighted"})
## @deftypefnx {} {@var{A} =} adjacency (@var{G}, @var{W})
## Return the sparse adjacency matrix of the graph or digraph @var{G}.
##
## @var{G} must be a @code{graph} or @code{digraph} object.  The returned
## matrix @var{A} is always sparse and has size
## @code{numnodes (@var{G})}-by-@code{numnodes (@var{G})}.  For a
## @code{graph} (undirected) @var{A} is symmetric; for a @code{digraph}
## @var{A}(i, j) is nonzero only when there is an edge from node
## @math{i} to node @math{j}.
##
## With one input @code{adjacency (@var{G})} returns a @emph{binary}
## adjacency matrix: @var{A}(i, j) equals @code{1} when an edge exists
## and @code{0} otherwise.  For a multigraph @code{digraph} (see
## @code{ismultigraph}), parallel edges @emph{accumulate}: @var{A}(i, j)
## counts the number of edges from @math{i} to @math{j}.  The edge
## weights stored on @var{G} are @emph{ignored} by this form.
##
## With @qcode{"weighted"} as the second input, @var{A} contains the
## edge weights.  If @var{G} was constructed without weights every edge
## contributes @code{1} (same result as the one-input form).  For a
## multigraph @code{digraph}, weights of parallel edges are summed into
## the corresponding cell of @var{A}.
##
## With a numeric vector @var{W} of length @code{numedges (@var{G})},
## @var{A} uses @var{W}(k) as the weight of edge @math{k} in place of
## the graph's own weights.  Edge order matches the order of
## @code{@var{G}.Edges}.  For an undirected @code{graph}, each edge
## contributes to both @var{A}(i, j) and @var{A}(j, i) (non-self-loop),
## or a single entry at @var{A}(i, i) for a self-loop.
##
## @example
## @group
## G = digraph ([1 2 3], [2 3 1]);
## full (adjacency (G))           # @result{} [0 1 0; 0 0 1; 1 0 0]
##
## Gw = digraph ([1 2 3], [2 3 1], [10 20 30]);
## full (adjacency (Gw))          # @result{} [0 1 0; 0 0 1; 1 0 0]
## full (adjacency (Gw, "weighted"))
##                                # @result{} [0 10 0; 0 0 20; 30 0 0]
##
## H = graph ([1 2 3], [2 3 1]);
## full (adjacency (H))           # symmetric 3x3 with three 1s per pair
##
## M = digraph ([1 1 2], [2 2 3], "multigraph");
## full (adjacency (M))           # @result{} A(1,2) == 2, A(2,3) == 1
## @end group
## @end example
##
## @seealso{graph, digraph, incidence, laplacian, numedges, numnodes,
##          ismultigraph}
## @end deftypefn

function A = adjacency (G, varargin)

  ## NOTE: When called with a graph or digraph object, Octave's classdef
  ## method dispatch runs the class-internal @code{adjacency} method and
  ## this free-function body is not reached.  This file exists both as a
  ## canonical documentation target (so @code{help adjacency} works
  ## outside the context of an instance) and as a fallback that gives a
  ## helpful error for non-graph inputs.

  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif

  if (! isa (G, "graph") && ! isa (G, "digraph"))
    error ("Octave:invalid-input-arg", ...
           "adjacency: G must be a graph or digraph object");
  endif

  ## Defensive delegation: classdef method dispatch should intercept any
  ## call with a graph/digraph first arg, but we route through dot
  ## notation to be safe.
  if (nargin == 1)
    A = G.adjacency ();
  else
    A = G.adjacency (varargin{1});
  endif

endfunction


## ------------------------------------------------------------------
## BIST blocks
## ------------------------------------------------------------------

## ---------------- Basic one-input form ----------------------------

## Simple digraph no-arg: correct 0/1 adjacency.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! A = adjacency (G);
%! assert (issparse (A));
%! assert (size (A), [3, 3]);
%! assert (full (A), [0 1 0; 0 0 1; 1 0 0]);

## Simple graph no-arg: symmetric 0/1 adjacency.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! A = adjacency (G);
%! assert (issparse (A));
%! assert (size (A), [3, 3]);
%! Af = full (A);
%! assert (Af, Af');
%! assert (Af, [0 1 1; 1 0 1; 1 1 0]);

## Result class is double.
%!test
%! G = digraph ([1 2], [2 3]);
%! A = adjacency (G);
%! assert (class (A), "double");

## Result is always sparse.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! assert (issparse (adjacency (G)));
%! assert (issparse (adjacency (G, "weighted")));
%! assert (issparse (adjacency (G, [1 2 3])));

## ---------------- Empty / edgeless ------------------------------

## Empty digraph.
%!test
%! G = digraph ();
%! A = adjacency (G);
%! assert (size (A), [0, 0]);
%! assert (issparse (A));

## Empty graph.
%!test
%! G = graph ();
%! A = adjacency (G);
%! assert (size (A), [0, 0]);
%! assert (issparse (A));

## Edgeless N-node digraph.
%!test
%! G = digraph (4);
%! A = adjacency (G);
%! assert (size (A), [4, 4]);
%! assert (nnz (A), 0);

## Edgeless N-node graph.
%!test
%! G = graph (5);
%! A = adjacency (G);
%! assert (size (A), [5, 5]);
%! assert (nnz (A), 0);

## Edgeless digraph with 'weighted' still returns empty matrix of
## correct shape.
%!test
%! G = digraph (3);
%! A = adjacency (G, "weighted");
%! assert (size (A), [3, 3]);
%! assert (nnz (A), 0);

## Edgeless digraph with empty custom weight vector.
%!test
%! G = digraph (3);
%! A = adjacency (G, []);
%! assert (size (A), [3, 3]);
%! assert (nnz (A), 0);

## ---------------- Weighted form (binary result) -------------------

## Weighted digraph no-arg: returns 0/1 (not weights).
%!test
%! G = digraph ([1 2 3], [2 3 1], [10 20 30]);
%! A = adjacency (G);
%! assert (full (A), [0 1 0; 0 0 1; 1 0 0]);
%! assert (max (full (A)(:)), 1);

## Weighted graph no-arg: returns 0/1 (not weights).
%!test
%! G = graph ([1 2 3], [2 3 1], [10 20 30]);
%! A = adjacency (G);
%! assert (max (full (A)(:)), 1);
%! assert (nnz (A), 6);   # 3 edges * 2 entries (symmetric)

## ---------------- "weighted" flag ---------------------------------

## Weighted digraph with 'weighted' returns weights.
%!test
%! G = digraph ([1 2 3], [2 3 1], [10 20 30]);
%! A = adjacency (G, "weighted");
%! assert (full (A), [0 10 0; 0 0 20; 30 0 0]);

## Weighted graph with 'weighted' returns symmetric weights.
%!test
%! G = graph ([1 2 3], [2 3 1], [10 20 30]);
%! A = adjacency (G, "weighted");
%! Af = full (A);
%! assert (Af, Af');
%! ## Edges (1,2)=10, (2,3)=20, (1,3)=30 -- lex order
%! assert (Af(1,2), 10); assert (Af(2,1), 10);
%! assert (Af(2,3), 20); assert (Af(3,2), 20);
%! assert (Af(1,3), 30); assert (Af(3,1), 30);

## Unweighted digraph with 'weighted' = 0/1.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! A = adjacency (G, "weighted");
%! assert (full (A), [0 1 0; 0 0 1; 1 0 0]);

## Unweighted graph with 'weighted' = 0/1 symmetric.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! A = adjacency (G, "weighted");
%! assert (full (A), [0 1 1; 1 0 1; 1 1 0]);

## 'weighted' flag is case-insensitive.
%!test
%! G = digraph ([1 2 3], [2 3 1], [10 20 30]);
%! assert (adjacency (G, "weighted"), adjacency (G, "Weighted"));
%! assert (adjacency (G, "weighted"), adjacency (G, "WEIGHTED"));

## 'weighted' flag accepts 1-element cellstr.
%!test
%! G = digraph ([1 2 3], [2 3 1], [10 20 30]);
%! A1 = adjacency (G, "weighted");
%! A2 = adjacency (G, {"weighted"});
%! assert (full (A1), full (A2));

## ---------------- Self-loops -------------------------------------

## Self-loop digraph: A(i,i) = 1.
%!test
%! G = digraph ([1 2 3], [2 3 3]);
%! A = adjacency (G);
%! assert (full (A)(3,3), 1);

## Self-loop graph: A(i,i) = 1 (NOT 2 -- MATLAB adjacency convention,
## differs from degree which counts self-loop as 2).
%!test
%! G = graph ([1 2 3], [2 3 3]);
%! A = adjacency (G);
%! assert (full (A)(3,3), 1);

## Weighted self-loop digraph: 'weighted' returns weight at (i,i).
%!test
%! G = digraph ([1 2 3], [2 3 3], [10 20 30]);
%! A = adjacency (G, "weighted");
%! assert (full (A)(3,3), 30);

## Weighted self-loop graph: 'weighted' returns weight at (i,i)
## (single entry, not doubled).
%!test
%! G = graph ([1 2 3], [2 3 3], [10 20 30]);
%! A = adjacency (G, "weighted");
%! assert (full (A)(3,3), 30);

## ---------------- Custom weight vector (W) ------------------------

## Custom weights W on digraph.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! A = adjacency (G, [10 20 30]);
%! assert (full (A), [0 10 0; 0 0 20; 30 0 0]);

## Custom weights W on graph: symmetric result.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! A = adjacency (G, [100 200 300]);
%! Af = full (A);
%! assert (Af, Af');

## Custom weights W: column vector accepted.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! A = adjacency (G, [10; 20; 30]);
%! assert (full (A), [0 10 0; 0 0 20; 30 0 0]);

## Custom weights W override the graph's stored weights.
%!test
%! G = digraph ([1 2 3], [2 3 1], [7 8 9]);
%! A = adjacency (G, [1 2 3]);
%! assert (full (A), [0 1 0; 0 0 2; 3 0 0]);

## Custom weights W with negative entries OK.
%!test
%! G = digraph ([1 2], [2 3]);
%! A = adjacency (G, [-5 -7]);
%! assert (full (A)(1,2), -5);
%! assert (full (A)(2,3), -7);

## Custom weights W for graph with self-loop: self-loop gets W once.
%!test
%! G = graph ([1 2 3], [2 3 3]);
%! ## Edges in lex order: (1,2), (2,3), (3,3).
%! A = adjacency (G, [10 20 30]);
%! assert (full (A)(3,3), 30);   # self-loop uses W once
%! assert (full (A)(1,2), 10);
%! assert (full (A)(2,1), 10);   # symmetric
%! assert (full (A)(2,3), 20);
%! assert (full (A)(3,2), 20);

## Custom weights W coerced to double from integer class.
%!test
%! G = digraph ([1 2], [2 3]);
%! A = adjacency (G, int32 ([5, 7]));
%! assert (class (A), "double");
%! assert (full (A)(1,2), 5);
%! assert (full (A)(2,3), 7);

## ---------------- Multigraph behaviour ----------------------------

## Multigraph digraph: no-arg counts parallel edges.
%!test
%! G = digraph ([1 1 2], [2 2 3], "multigraph");
%! A = adjacency (G);
%! assert (full (A)(1,2), 2);   # 2 parallel edges
%! assert (full (A)(2,3), 1);

## Multigraph digraph: 'weighted' sums weights of parallel edges.
%!test
%! G = digraph ([1 1 2], [2 2 3], [5 7 3], "multigraph");
%! A = adjacency (G, "weighted");
%! assert (full (A)(1,2), 12);  # 5 + 7
%! assert (full (A)(2,3), 3);

## Multigraph digraph: W vector applied per edge.
%!test
%! G = digraph ([1 1 2], [2 2 3], "multigraph");
%! A = adjacency (G, [4 6 9]);
%! assert (full (A)(1,2), 10);  # 4 + 6
%! assert (full (A)(2,3), 9);

## Multigraph digraph with self-loop: count accumulates at (i,i).
%!test
%! G = digraph ([1 1 1 2], [1 1 1 3], "multigraph");
%! A = adjacency (G);
%! assert (full (A)(1,1), 3);
%! assert (full (A)(2,3), 1);

## ---------------- Named graphs ------------------------------------

## Named digraph: result identical to unnamed.
%!test
%! G = digraph ([1 2 3], [2 3 1], [], {"a", "b", "c"});
%! A = adjacency (G);
%! assert (full (A), [0 1 0; 0 0 1; 1 0 0]);

## Named graph with custom weights: result symmetric.
%!test
%! G = graph ([1 2 3], [2 3 1], [], {"x", "y", "z"});
%! A = adjacency (G, [10 20 30]);
%! Af = full (A);
%! assert (Af, Af');

## ---------------- Shape / symmetry --------------------------------

## Digraph result may be non-symmetric.
%!test
%! G = digraph ([1 2], [2 3]);
%! A = adjacency (G);
%! Af = full (A);
%! assert (! isequal (Af, Af'));

## Graph result is always symmetric.
%!test
%! G = graph ([1 2 3 1], [2 3 4 4]);
%! A = adjacency (G);
%! Af = full (A);
%! assert (Af, Af');

## Graph weighted result is symmetric.
%!test
%! G = graph ([1 2 3], [2 3 1], [5 7 11]);
%! A = adjacency (G, "weighted");
%! Af = full (A);
%! assert (Af, Af');

## Digraph 2-edge example: exact pattern.
%!test
%! G = digraph ([1 1 2], [2 3 3]);
%! A = adjacency (G);
%! assert (full (A), [0 1 1; 0 0 1; 0 0 0]);

## ---------------- Integration ------------------------------------

## Sum of digraph adjacency rows equals outdegree.
%!test
%! G = digraph ([1 1 2 3], [2 3 3 1]);
%! A = adjacency (G);
%! row_sums = full (sum (A, 2));
%! assert (row_sums, [2; 1; 1]);

## Sum of digraph adjacency columns equals indegree.
%!test
%! G = digraph ([1 1 2 3], [2 3 3 1]);
%! A = adjacency (G);
%! col_sums = full (sum (A, 1))(:);
%! assert (col_sums, [1; 1; 2]);

## ---------------- Errors -----------------------------------------

## Non-graph first arg.
%!error <G must be a graph or digraph> adjacency (1)
%!error <G must be a graph or digraph> adjacency ("hello")
%!error <G must be a graph or digraph> adjacency (sparse (3, 3))

## nargin mismatch.
%!error <Invalid call> adjacency ()
%!error <Invalid call> adjacency (digraph (3), "weighted", "extra")

## Unknown flag string.
%!error <unknown|weighted|weight vector> ...
%!   adjacency (digraph ([1 2], [2 3]), "bogus")

## Weight vector of wrong length.
%!error <length|numedges> ...
%!   adjacency (digraph ([1 2], [2 3]), [1])
%!error <length|numedges> ...
%!   adjacency (digraph ([1 2], [2 3]), [1 2 3])

## Non-numeric non-string arg.
%!error <numeric|weight|must be> ...
%!   adjacency (digraph ([1 2], [2 3]), {1, 2})

## Complex weight vector.
%!error <real|complex|numeric> ...
%!   adjacency (digraph ([1 2], [2 3]), [1+1i, 2])
