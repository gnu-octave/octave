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
## @deftypefn {} {@var{L} =} laplacian (@var{G})
## Return the sparse graph Laplacian of the undirected graph @var{G}.
##
## @var{G} must be a @code{graph} object.  The Laplacian is not defined
## on the directed @code{digraph} class; calling @code{laplacian} on a
## @code{digraph} raises an error.
##
## The returned matrix @var{L} is the @code{numnodes (@var{G})}-by-
## @code{numnodes (@var{G})} sparse matrix
## @tex
## $L = D - A$
## @end tex
## @ifnottex
## @math{L = D - A}
## @end ifnottex
## where @math{D} is the diagonal matrix of node degrees and @math{A}
## is the off-diagonal binary adjacency matrix.  Concretely:
##
## @itemize @bullet
## @item
## @code{L(i, i)} equals @code{degree (@var{G}, i)}, the degree of node
## @math{i} in the convention of @code{degree} (each non-self-loop edge
## contributes @code{1}; each self-loop contributes @code{2}).
## @item
## For @math{i} not equal to @math{j}, @code{L(i, j) = L(j, i) = -1}
## when an edge exists between @math{i} and @math{j}, and @code{0}
## otherwise.
## @end itemize
##
## Edge weights stored on @var{G} are @emph{ignored}: the Laplacian is
## always built from the binary edge-presence pattern, matching MATLAB's
## convention.  The result is sparse and of class @code{double}.
##
## @example
## @group
## G = graph ([1 2 3], [2 3 1]);   # triangle
## full (laplacian (G))
## ## @result{} [ 2 -1 -1;
## ##           -1  2 -1;
## ##           -1 -1  2]
##
## P = graph ([1 2 3], [2 3 4]);   # path 1-2-3-4
## full (laplacian (P))
## ## @result{} [ 1 -1  0  0;
## ##           -1  2 -1  0;
## ##            0 -1  2 -1;
## ##            0  0 -1  1]
## @end group
## @end example
##
## @seealso{graph, adjacency, incidence, degree, numnodes, numedges}
## @end deftypefn

function L = laplacian (G)

  ## NOTE: When called with a graph object, Octave's classdef method
  ## dispatch runs the class-internal @code{laplacian} method and this
  ## free-function body is not reached.  This file exists both as a
  ## canonical documentation target (so @code{help laplacian} works
  ## outside the context of an instance) and as a fallback that gives a
  ## helpful error for non-graph inputs.

  if (nargin != 1)
    print_usage ();
  endif

  if (isa (G, "digraph"))
    error ("Octave:invalid-input-arg", ...
           "laplacian: not defined for a digraph; laplacian requires an undirected graph");
  endif

  if (! isa (G, "graph"))
    error ("Octave:invalid-input-arg", ...
           "laplacian: G must be a graph object");
  endif

  ## Defensive delegation: classdef method dispatch should intercept any
  ## call with a graph first arg, but route through dot notation just
  ## in case.
  L = G.laplacian ();

endfunction


## ------------------------------------------------------------------
## BIST blocks
## ------------------------------------------------------------------

## ---------------- Basic shape and class ---------------------------

## Triangle: known 3x3 Laplacian.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! L = laplacian (G);
%! assert (issparse (L));
%! assert (size (L), [3, 3]);
%! assert (full (L), [2 -1 -1; -1 2 -1; -1 -1 2]);

## Result class is double.
%!test
%! G = graph ([1 2], [2 3]);
%! L = laplacian (G);
%! assert (class (L), "double");

## Result is always sparse.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! assert (issparse (laplacian (G)));

## ---------------- Path graph (canonical example) ------------------

## Path graph 1-2-3-4: tridiagonal Laplacian.
%!test
%! G = graph ([1 2 3], [2 3 4]);
%! L = laplacian (G);
%! exp = [ 1 -1  0  0;
%!        -1  2 -1  0;
%!         0 -1  2 -1;
%!         0  0 -1  1];
%! assert (full (L), exp);

## Path-graph row sums are zero (no self-loops, standard property).
%!test
%! G = graph ([1 2 3], [2 3 4]);
%! L = laplacian (G);
%! assert (full (sum (L, 2)), [0; 0; 0; 0]);

## ---------------- Star graph --------------------------------------

## Star graph: centre node has high diagonal entry.
%!test
%! s = ones (1, 5);
%! t = 2:6;
%! G = graph (s, t);
%! L = laplacian (G);
%! assert (full (L(1, 1)), 5);
%! ## Each leaf has degree 1.
%! assert (full (diag (L)(2:6)), ones (5, 1));
%! ## Off-diagonal centre-to-leaf entries are -1.
%! assert (full (L(1, 2:6)), -ones (1, 5));
%! assert (full (L(2:6, 1)), -ones (5, 1));

## ---------------- Empty / edgeless --------------------------------

## Empty graph -> empty 0x0 sparse Laplacian.
%!test
%! G = graph ();
%! L = laplacian (G);
%! assert (size (L), [0, 0]);
%! assert (issparse (L));

## Edgeless N-node graph: all-zero Laplacian (no edges, all degrees 0).
%!test
%! G = graph (5);
%! L = laplacian (G);
%! assert (size (L), [5, 5]);
%! assert (nnz (L), 0);
%! assert (issparse (L));

## ---------------- Symmetry ----------------------------------------

## Laplacian is always symmetric.
%!test
%! G = graph ([1 1 2 3 4], [2 4 3 4 5]);
%! L = laplacian (G);
%! Lf = full (L);
%! assert (Lf, Lf');

## Even with weights present, Laplacian is symmetric (and ignores
## weights).
%!test
%! G = graph ([1 2 3], [2 3 1], [10 20 30]);
%! L = laplacian (G);
%! Lf = full (L);
%! assert (Lf, Lf');
%! ## Same as unweighted triangle: L(i,j) = -1 off-diagonal.
%! assert (Lf, [2 -1 -1; -1 2 -1; -1 -1 2]);

## ---------------- Standard properties -----------------------------

## Off-diagonal: L(i, j) = -1 iff there is an edge (i, j) (no self-loops).
%!test
%! G = graph ([1 2 3 1], [2 3 4 4]);
%! L = laplacian (G);
%! Lf = full (L);
%! ## Edges: (1,2), (2,3), (3,4), (1,4)
%! assert (Lf(1,2), -1); assert (Lf(2,1), -1);
%! assert (Lf(2,3), -1); assert (Lf(3,2), -1);
%! assert (Lf(3,4), -1); assert (Lf(4,3), -1);
%! assert (Lf(1,4), -1); assert (Lf(4,1), -1);
%! ## Non-edges off-diagonal are zero.
%! assert (Lf(1,3), 0); assert (Lf(3,1), 0);
%! assert (Lf(2,4), 0); assert (Lf(4,2), 0);

## Diagonal entries equal degree (without self-loops).
%!test
%! G = graph ([1 2 3 1], [2 3 4 4]);
%! L = laplacian (G);
%! d = degree (G);
%! assert (full (diag (L)), d);

## Row sums are zero when there are no self-loops.
%!test
%! G = graph ([1 2 3 1], [2 3 4 4]);
%! L = laplacian (G);
%! assert (full (sum (L, 2)), zeros (4, 1));

## Column sums are zero when there are no self-loops.
%!test
%! G = graph ([1 2 3 1], [2 3 4 4]);
%! L = laplacian (G);
%! assert (full (sum (L, 1))(:), zeros (4, 1));

## Smallest eigenvalue of L is 0 for any graph.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! L = laplacian (G);
%! ev = sort (eig (full (L)));
%! assert (ev(1), 0, 1e-10);

## Number of zero eigenvalues equals number of connected components
## (handshake-like check on a 2-component graph).
%!test
%! ## Two disconnected triangles {1,2,3} and {4,5,6}.
%! s = [1 2 3 4 5 6];
%! t = [2 3 1 5 6 4];
%! G = graph (s, t);
%! L = laplacian (G);
%! ev = sort (eig (full (L)));
%! assert (ev(1), 0, 1e-10);
%! assert (ev(2), 0, 1e-10);
%! assert (ev(3) > 1e-6);

## ---------------- Self-loops --------------------------------------

## Single self-loop on node 1: L(1,1) = degree(1) = 2 (MATLAB convention),
## off-diagonal contribution is zero.
%!test
%! G = graph ([1], [1]);
%! L = laplacian (G);
%! assert (full (L), 2);

## Self-loop in 3-node graph: only the diagonal of looped node is
## nonzero (= degree = 2), no off-diagonal entries.
%!test
%! G = graph ([2], [2], [], 3);
%! L = laplacian (G);
%! assert (full (L), [0 0 0; 0 2 0; 0 0 0]);

## Self-loop combined with regular edges: diagonal counts self-loops as 2.
%!test
%! G = graph ([1 2 2], [2 2 3]);   # edges: (1,2), self-loop at 2, (2,3)
%! L = laplacian (G);
%! ## degrees: node 1 = 1, node 2 = 4 (self-loop + 2 incident edges),
%! ## node 3 = 1.
%! assert (full (diag (L)), [1; 4; 1]);
%! ## off-diagonal: -1 between connected nodes.
%! assert (full (L(1,2)), -1); assert (full (L(2,1)), -1);
%! assert (full (L(2,3)), -1); assert (full (L(3,2)), -1);

## ---------------- Weights ignored ---------------------------------

## Weighted graph yields the same Laplacian as unweighted (weights
## ignored).
%!test
%! Gu = graph ([1 2 3], [2 3 1]);
%! Gw = graph ([1 2 3], [2 3 1], [0.1 0.2 0.3]);
%! assert (full (laplacian (Gu)), full (laplacian (Gw)));

## Weighted graph with negative weights: still ignored.
%!test
%! G = graph ([1 2 3], [2 3 1], [-1 -2 -3]);
%! L = laplacian (G);
%! assert (full (L), [2 -1 -1; -1 2 -1; -1 -1 2]);

## ---------------- Adjacency-matrix constructor round-trip ----------

## Adjacency-matrix constructor: round-trip with known L.
%!test
%! A = [0 1 1; 1 0 1; 1 1 0];
%! G = graph (A);
%! L = laplacian (G);
%! assert (full (L), [2 -1 -1; -1 2 -1; -1 -1 2]);

## ---------------- Named graphs ------------------------------------

## Named graph: result identical to unnamed.
%!test
%! G = graph ([1 2 3], [2 3 1], [], {"a", "b", "c"});
%! L = laplacian (G);
%! assert (full (L), [2 -1 -1; -1 2 -1; -1 -1 2]);

## ---------------- Larger graph ------------------------------------

## Siever-style 9-node fixture as undirected: confirm L = D - A_off.
%!test
%! s = [1 2 3 3 4 5 5 6 7 7 8 9];
%! t = [2 3 2 4 5 6 9 7 8 9 7 4];
%! pairs = unique (sort ([s.', t.'], 2), "rows");
%! G = graph (pairs(:, 1), pairs(:, 2));
%! L = laplacian (G);
%! ## L = diag(degree) - off-diagonal binary adjacency.
%! d = degree (G);
%! A = adjacency (G);
%! Aoff = A - diag (diag (A));
%! assert (full (L), full (diag (d) - Aoff));

## ---------------- Disconnected node -------------------------------

## Isolated trailing node: degree 0, all-zero row/column.
%!test
%! G = graph ([1 2], [2 3], [], 5);   # nodes 4, 5 are isolated
%! L = laplacian (G);
%! assert (full (diag (L)), [1; 2; 1; 0; 0]);
%! assert (full (L(4, :)), zeros (1, 5));
%! assert (full (L(:, 4)), zeros (5, 1));
%! assert (full (L(5, :)), zeros (1, 5));

## ---------------- Errors on digraph -------------------------------

## Errors when called on a digraph (free-function path).
%!error <not defined for a digraph|laplacian requires> ...
%!   laplacian (digraph ([1 2 3], [2 3 1]))

## Errors when called on a digraph (classdef method path: G.laplacian()).
%!error <not defined for a digraph|laplacian|undirected> ...
%!   __dummy = digraph ([1 2 3], [2 3 1]); __dummy.laplacian ()

## Empty digraph also errors.
%!error <not defined for a digraph|laplacian> ...
%!   laplacian (digraph ())

## ---------------- General input errors ----------------------------

## Non-graph first arg.
%!error <G must be a graph|not defined for a digraph> laplacian (1)
%!error <G must be a graph|not defined for a digraph> laplacian ("hello")
%!error <G must be a graph|not defined for a digraph> laplacian (sparse (3, 3))
%!error <G must be a graph|not defined for a digraph> laplacian ({})

## nargin mismatch.
%!error <Invalid call> laplacian ()
%!error <too many inputs|Invalid call> laplacian (graph (3), 1)
