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
## @deftypefn {} {@var{I} =} incidence (@var{G})
## Return the sparse incidence matrix of the graph or digraph @var{G}.
##
## @var{G} must be a @code{graph} or @code{digraph} object.  The returned
## matrix @var{I} is always sparse with size @code{numnodes (@var{G})}-by-
## @code{numedges (@var{G})}.  Column @math{k} of @var{I} encodes edge
## @math{k} of @var{G} (in the edge order produced by @code{@var{G}.Edges}):
##
## @itemize @bullet
## @item
## For an undirected @code{graph}, edge @math{k} with endpoints @math{i}
## and @math{j} sets @code{@var{I}(i, k) = 1} and @code{@var{I}(j, k) = 1}.
##
## @item
## For a @code{digraph}, edge @math{k} from source @math{s} to
## destination @math{t} sets @code{@var{I}(s, k) = -1} and
## @code{@var{I}(t, k) = +1}.
## @end itemize
##
## Self-loops (edges with identical endpoints) produce an @emph{empty}
## column in @var{I}: no entries are generated because an incidence
## matrix must have exactly two entries per column.  This matches
## MATLAB's convention and means that @code{sum (abs (incidence
## (@var{G})), 1)} is @code{0} for every self-loop column and @code{2}
## for every ordinary edge column.
##
## Edge column order follows @code{@var{G}.Edges.EndNodes}: for a
## non-multigraph this is lexicographic @code{(src, dst)} order, and for
## a multigraph it is the insertion (stable) order in which edges were
## added.  The returned matrix is sparse of class @code{double}.
##
## @example
## @group
## G = digraph ([1 2 3], [2 3 1]);
## full (incidence (G))
## ## @result{} [-1  0  1;
## ##           1 -1  0;
## ##           0  1 -1]
##
## H = graph ([1 2 3], [2 3 1]);
## full (incidence (H))
## ## 3x3 matrix: each column has two 1s at the edge's endpoints
##
## S = graph ([1 2 3], [2 3 3]);   # edge (3, 3) is a self-loop
## full (incidence (S))(:, 3)      # self-loop column is all zero
## @end group
## @end example
##
## @seealso{graph, digraph, adjacency, laplacian, numedges, numnodes}
## @end deftypefn

function I = incidence (G)

  ## NOTE: When called with a graph or digraph object, Octave's classdef
  ## method dispatch runs the class-internal @code{incidence} method and
  ## this free-function body is not reached.  This file exists both as
  ## a canonical documentation target (so @code{help incidence} works
  ## outside the context of an instance) and as a fallback that gives a
  ## helpful error for non-graph inputs.

  if (nargin != 1)
    print_usage ();
  endif

  if (! isa (G, "graph") && ! isa (G, "digraph"))
    error ("Octave:invalid-input-arg", ...
           "incidence: G must be a graph or digraph object");
  endif

  ## Defensive delegation: classdef method dispatch should intercept any
  ## call with a graph/digraph first arg, but we route through dot
  ## notation to be safe.
  I = G.incidence ();

endfunction


## ------------------------------------------------------------------
## BIST blocks
## ------------------------------------------------------------------

## ---------------- Basic digraph ----------------------------------

## Simple digraph 3-cycle: -1 at src rows, +1 at dst rows.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! I = incidence (G);
%! assert (issparse (I));
%! assert (size (I), [3, 3]);
%! ## Edges in lex (src, dst) order: (1,2), (2,3), (3,1)
%! assert (full (I), [-1  0  1; 1 -1  0; 0  1 -1]);

## Digraph: column sums of -1/+1 = 0 for non-self-loop edges.
%!test
%! G = digraph ([1 2 3 1], [2 3 1 3]);
%! I = incidence (G);
%! assert (full (sum (I, 1)), [0, 0, 0, 0]);

## Digraph: row sum = outdegree - indegree.
%!test
%! G = digraph ([1 1 2 3], [2 3 3 1]);
%! I = incidence (G);
%! ## outdegree - indegree per node:
%! ##   node 1: out=2, in=1 -> +1
%! ##   node 2: out=1, in=1 ->  0
%! ##   node 3: out=1, in=2 -> -1
%! ## Wait: rowsum(I) where -1 is src and +1 is dst:
%! ##   rowsum(node i) = (#edges with dst=i) - (#edges with src=i)
%! ##                  = indegree(i) - outdegree(i)
%! got = full (sum (I, 2));
%! expected = [indegree(G, 1) - outdegree(G, 1); ...
%!             indegree(G, 2) - outdegree(G, 2); ...
%!             indegree(G, 3) - outdegree(G, 3)];
%! assert (got, expected);

## ---------------- Basic graph -------------------------------------

## Simple graph triangle: 1 at both endpoint rows per edge.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! I = incidence (G);
%! assert (issparse (I));
%! assert (size (I), [3, 3]);
%! ## Edges in lex order with col1 <= col2: (1,2), (1,3), (2,3)
%! assert (full (I), [1 1 0; 1 0 1; 0 1 1]);

## Graph: each column sums to 2 (non-self-loop edges).
%!test
%! G = graph ([1 2 3 4], [2 3 4 1]);
%! I = incidence (G);
%! assert (full (sum (I, 1)), [2, 2, 2, 2]);

## Graph: row sum = degree (no self-loops).
%!test
%! G = graph ([1 1 2 3], [2 3 3 4]);
%! I = incidence (G);
%! got = full (sum (I, 2));
%! expected = [degree(G, 1); degree(G, 2); degree(G, 3); degree(G, 4)];
%! assert (got, expected);

## ---------------- Result shape / class ---------------------------

## Returned matrix class is double.
%!test
%! G = digraph ([1 2], [2 3]);
%! I = incidence (G);
%! assert (class (I), "double");

%!test
%! G = graph ([1 2], [2 3]);
%! I = incidence (G);
%! assert (class (I), "double");

## Returned matrix is always sparse.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! assert (issparse (incidence (G)));

%!test
%! G = graph ([1 2 3], [2 3 1]);
%! assert (issparse (incidence (G)));

## Shape is N-by-M.
%!test
%! G = digraph ([1 1 2 3], [2 3 3 1]);
%! I = incidence (G);
%! assert (size (I), [numnodes(G), numedges(G)]);

%!test
%! G = graph ([1 1 2 3 4], [2 3 3 4 1]);
%! I = incidence (G);
%! assert (size (I), [numnodes(G), numedges(G)]);

## ---------------- Empty / edgeless -------------------------------

## Empty digraph.
%!test
%! G = digraph ();
%! I = incidence (G);
%! assert (issparse (I));
%! assert (size (I), [0, 0]);

## Empty graph.
%!test
%! G = graph ();
%! I = incidence (G);
%! assert (issparse (I));
%! assert (size (I), [0, 0]);

## Edgeless N-node digraph: shape [N, 0].
%!test
%! G = digraph (4);
%! I = incidence (G);
%! assert (size (I), [4, 0]);
%! assert (nnz (I), 0);

## Edgeless N-node graph: shape [N, 0].
%!test
%! G = graph (5);
%! I = incidence (G);
%! assert (size (I), [5, 0]);
%! assert (nnz (I), 0);

## ---------------- Self-loops -------------------------------------

## Digraph self-loop: corresponding column is all zero.
%!test
%! G = digraph ([1 2 3], [2 3 3]);  # (3,3) self-loop
%! I = incidence (G);
%! assert (size (I), [3, 3]);
%! ## Edges in lex order: (1,2), (2,3), (3,3).
%! assert (full (I(:, 1)), [-1; 1; 0]);
%! assert (full (I(:, 2)), [0; -1; 1]);
%! assert (full (I(:, 3)), [0; 0; 0]);  # self-loop column empty

## Graph self-loop: corresponding column is all zero.
%!test
%! G = graph ([1 2 3], [2 3 3]);
%! I = incidence (G);
%! assert (size (I), [3, 3]);
%! ## Edges in lex order: (1,2), (2,3), (3,3).
%! assert (full (I(:, 1)), [1; 1; 0]);
%! assert (full (I(:, 2)), [0; 1; 1]);
%! assert (full (I(:, 3)), [0; 0; 0]);  # self-loop column empty

## Digraph all self-loops: matrix is all zeros, shape preserved.
%!test
%! G = digraph ([1 2 3], [1 2 3]);
%! I = incidence (G);
%! assert (size (I), [3, 3]);
%! assert (nnz (I), 0);

## Graph all self-loops: matrix is all zeros, shape preserved.
%!test
%! G = graph ([1 2 3], [1 2 3]);
%! I = incidence (G);
%! assert (size (I), [3, 3]);
%! assert (nnz (I), 0);

## ---------------- Weighted inputs ignored ------------------------

## Weighted digraph produces same -1/+1 pattern (weights ignored).
%!test
%! G = digraph ([1 2 3], [2 3 1], [10 20 30]);
%! I = incidence (G);
%! assert (full (I), [-1  0  1; 1 -1  0; 0  1 -1]);

## Weighted graph produces same 1/1 pattern (weights ignored).
%!test
%! G = graph ([1 2 3], [2 3 1], [10 20 30]);
%! I = incidence (G);
%! ## Result values are only 0, 1.
%! vals = unique (full (I)(:));
%! assert (vals, [0; 1]);

## ---------------- Multigraph (digraph) --------------------------

## Multigraph digraph: each parallel edge gets its own column.
%!test
%! G = digraph ([1 1 2], [2 2 3], "multigraph");
%! I = incidence (G);
%! assert (size (I), [3, 3]);
%! assert (nnz (I), 6);   # 3 edges * 2 entries each
%! ## Edges stored in insertion order: (1,2), (1,2), (2,3)
%! assert (full (I(:, 1)), [-1; 1; 0]);
%! assert (full (I(:, 2)), [-1; 1; 0]);
%! assert (full (I(:, 3)), [0; -1; 1]);

## Multigraph digraph self-loops: per-loop column zero.
%!test
%! G = digraph ([1 1 1 2], [1 1 1 3], "multigraph");
%! I = incidence (G);
%! assert (size (I), [3, 4]);
%! ## First three columns are self-loops at node 1 (all zero).
%! assert (full (I(:, 1)), [0; 0; 0]);
%! assert (full (I(:, 2)), [0; 0; 0]);
%! assert (full (I(:, 3)), [0; 0; 0]);
%! assert (full (I(:, 4)), [0; -1; 1]);

## ---------------- Named graphs ----------------------------------

## Named digraph: result identical to unnamed analog.
%!test
%! G = digraph ([1 2 3], [2 3 1], [], {"a", "b", "c"});
%! I = incidence (G);
%! assert (full (I), [-1  0  1; 1 -1  0; 0  1 -1]);

## Named graph: result identical to unnamed analog.
%!test
%! G = graph ([1 2 3], [2 3 1], [], {"x", "y", "z"});
%! I = incidence (G);
%! ## Lex order with col1 <= col2: (1,2), (1,3), (2,3)
%! assert (full (I), [1 1 0; 1 0 1; 0 1 1]);

## ---------------- Integration / identities ----------------------

## Digraph: sum(abs(I), 1) is 2 for ordinary edges, 0 for self-loop
## columns.
%!test
%! G = digraph ([1 2 3 4], [2 3 4 4]);  # last edge is self-loop (4,4)
%! I = incidence (G);
%! ## Edges in lex order: (1,2), (2,3), (3,4), (4,4)
%! col_sum_abs = full (sum (abs (I), 1));
%! assert (col_sum_abs, [2, 2, 2, 0]);

## Graph: sum(I, 1) is 2 for ordinary edges, 0 for self-loop columns.
%!test
%! G = graph ([1 2 3 4], [2 3 4 4]);
%! I = incidence (G);
%! col_sum = full (sum (I, 1));
%! assert (col_sum, [2, 2, 2, 0]);

## Graph: I * I' == A + D where A is adjacency and D is diagonal of
## degrees of non-self-loop edges.  (Exact identity for simple graph:
## I * I' has 2 on diagonal per non-self-loop incidence and the
## adjacency pattern off-diagonal.)  For a simple cycle:
##   I = [1 1 0; 1 0 1; 0 1 1]
##   I*I' = [2 1 1; 1 2 1; 1 1 2]
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! I = incidence (G);
%! M = full (I * I');
%! assert (M, [2 1 1; 1 2 1; 1 1 2]);

## Digraph: -I * I' ~ Laplacian-like only when undirected; for digraph
## the identity is I * I' = D_out + D_in (absolute values).  Focus on
## column sum of non-self-loop being zero:
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! I = incidence (G);
%! assert (full (sum (I, 1)), [0, 0, 0]);

## ---------------- Known example parity --------------------------

## Textbook digraph (wikipedia incidence matrix example scaled-down):
## 4 nodes, 4 edges (1,2), (2,3), (3,4), (4,1) forms a 4-cycle.
%!test
%! G = digraph ([1 2 3 4], [2 3 4 1]);
%! I = incidence (G);
%! ## Edges in lex order: (1,2), (2,3), (3,4), (4,1)
%! expected = [-1  0  0  1; ...
%!              1 -1  0  0; ...
%!              0  1 -1  0; ...
%!              0  0  1 -1];
%! assert (full (I), expected);

## Textbook graph (4-cycle):
%!test
%! G = graph ([1 2 3 4], [2 3 4 1]);
%! I = incidence (G);
%! ## Edges in lex order with col1 <= col2:
%! ##   (1,2), (1,4), (2,3), (3,4)
%! expected = [1 1 0 0; ...
%!             1 0 1 0; ...
%!             0 0 1 1; ...
%!             0 1 0 1];
%! assert (full (I), expected);

## ---------------- Errors ----------------------------------------

## Non-graph first arg.
%!error <G must be a graph or digraph> incidence (1)
%!error <G must be a graph or digraph> incidence ("hello")
%!error <G must be a graph or digraph> incidence (sparse (3, 3))

## nargin mismatch.
%!error <Invalid call> incidence ()
%!error <Invalid call|too many> incidence (digraph (3), "extra")
