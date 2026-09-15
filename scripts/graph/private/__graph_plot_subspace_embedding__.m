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
## @deftypefn {} {@var{M} =} __graph_plot_subspace_embedding__ (@var{G}, @var{dimension})
## Compute a Laplacian spectral embedding of a @code{graph} or
## @code{digraph} in a @var{dimension}-dimensional subspace.
##
## @var{G} is a @code{graph} or @code{digraph}; directed edges are
## treated as undirected for the purpose of computing the Laplacian.
## @var{dimension} is a positive integer specifying the target subspace
## dimension, validated by the caller.
##
## The function builds the symmetric, unweighted, self-loop-free graph
## Laplacian @math{L = D - A}, where @math{A} is the off-diagonal binary
## adjacency matrix and @math{D} is the diagonal of node degrees.  Edge
## weights are ignored, matching the convention used by @code{laplacian}.
##
## The return value @var{M} is a @code{numnodes (@var{G})}-by-
## @var{dimension} matrix whose columns are orthonormal Laplacian
## eigenvectors corresponding to the @var{dimension} smallest
## @emph{non-trivial} eigenvalues (i.e.@: all eigenvectors of the
## constant-mode kernel are skipped).  If the Laplacian has fewer than
## @var{dimension} non-trivial eigenvalues, the remaining columns of
## @var{M} are padded with zero vectors so that the shape of the output
## is always @code{numnodes (@var{G})}-by-@var{dimension}.
##
## Sign ambiguity is resolved deterministically: within each column, the
## first entry with the maximum absolute value is forced to be positive.
##
## Edge cases: @code{numnodes (@var{G}) == 0} returns
## @code{zeros (0, dimension)}; @code{numnodes (@var{G}) == 1} returns
## @code{zeros (1, dimension)}.
##
## This helper is internal to the graph/digraph plot layer and is not
## intended to be called directly by user code.
## @seealso{__graph_plot_subspace__, __graph_plot_subspace3__, laplacian}
## @end deftypefn

function M = __graph_plot_subspace_embedding__ (G, dimension)

  if (nargin != 2)
    print_usage ();
  endif
  if (! (isa (G, "graph") || isa (G, "digraph")))
    error ("Octave:invalid-input-arg", ...
           "__graph_plot_subspace_embedding__: G must be a graph or digraph");
  endif
  if (! (isnumeric (dimension) && isreal (dimension) && isscalar (dimension) ...
         && isfinite (dimension) && dimension == floor (dimension) ...
         && dimension >= 1))
    error ("Octave:invalid-input-arg", ...
           "__graph_plot_subspace_embedding__: DIMENSION must be a positive integer scalar");
  endif

  N = numnodes (G);
  dimension = double (dimension);

  if (N == 0)
    M = zeros (0, dimension);
    return;
  endif
  if (N == 1)
    M = zeros (1, dimension);
    return;
  endif

  ## Build an unweighted, self-loop-free symmetric adjacency matrix.
  ## adjacency() returns the pattern A(i,j) != 0 <=> there is an edge
  ## i->j.  For a digraph this is not symmetric; symmetrize by
  ## (A + A') > 0.  Remove diagonal (self-loops don't contribute to the
  ## standard Laplacian off-diagonal structure).
  A = adjacency (G);
  A = double ((A + A.') != 0);
  A = A - diag (diag (A));

  ## Laplacian L = D - A with D the diagonal of off-diagonal degrees.
  d = full (sum (A, 2));
  L = diag (d) - full (A);

  ## Full eigendecomposition of the (symmetric) Laplacian.  For L
  ## symmetric the eigenvalues are real and eig is stable.
  [V, Dvals] = eig (L);
  eig_vals = real (diag (Dvals));

  ## Sort eigenvalues ascending.
  [sorted_vals, idx] = sort (eig_vals);
  V = real (V(:, idx));

  ## Skip eigenvectors with near-zero eigenvalues (constant modes).  A
  ## connected graph has exactly one such mode; a graph with k components
  ## has k.  The tolerance scales with the maximum eigenvalue so that
  ## very flat Laplacians still skip the kernel reliably.
  max_abs = max (abs (sorted_vals));
  if (max_abs > 0)
    tol = max (1e-10, max_abs * 1e-10);
  else
    tol = 1e-10;
  endif
  skip_count = sum (sorted_vals <= tol);

  ## Take up to `dimension' eigenvectors after the skipped ones, pad
  ## with zero vectors if we don't have enough.
  avail = N - skip_count;
  take = min (avail, dimension);
  M = zeros (N, dimension);
  if (take > 0)
    M(:, 1:take) = V(:, (skip_count + 1):(skip_count + take));
  endif

  ## Sign-normalise each column: the first entry equal to the maximum
  ## absolute value must be positive.  This makes `eig` output
  ## deterministic across platforms / releases.
  for k = 1:dimension
    col = M(:, k);
    absc = abs (col);
    mval = max (absc);
    if (mval > 0)
      i_first = find (absc == mval, 1);
      if (col(i_first) < 0)
        M(:, k) = -col;
      endif
    endif
  endfor

endfunction


## ---------------- BIST ----------------

## Empty graph: 0-by-D.
%!test
%! G = digraph ();
%! M = __graph_plot_subspace_embedding__ (G, 2);
%! assert (size (M), [0, 2]);

## Single-node graph: 1-by-D of zeros.
%!test
%! G = digraph (1);
%! M = __graph_plot_subspace_embedding__ (G, 3);
%! assert (size (M), [1, 3]);
%! assert (M, zeros (1, 3));

## Triangle: non-trivial eigenvectors exist; 2 columns, finite.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! M = __graph_plot_subspace_embedding__ (G, 2);
%! assert (size (M), [3, 2]);
%! assert (all (isfinite (M(:))));

## Path graph 1-2-3-4: second eigenvector is Fiedler, known monotone.
%!test
%! G = graph ([1 2 3], [2 3 4]);
%! M = __graph_plot_subspace_embedding__ (G, 2);
%! ## Fiedler vector of a 4-path is monotone; sign-normalised so
%! ## largest-magnitude entry is positive.
%! v1 = M(:, 1);
%! ## Monotonic (strictly increasing or decreasing).
%! d = diff (v1);
%! assert (all (d > 0) || all (d < 0));

## Requesting more dimensions than available non-trivial eigenvectors
## pads with zero columns.
%!test
%! ## Connected 3-node graph has 2 non-trivial eigenvalues.
%! G = graph ([1 2 3], [2 3 1]);
%! M = __graph_plot_subspace_embedding__ (G, 4);
%! assert (size (M), [3, 4]);
%! assert (all (isfinite (M(:))));
%! ## At least the 4th column must be all-zero padding.
%! assert (M(:, 4), zeros (3, 1));

## Determinism across repeat calls.
%!test
%! G = graph ([1 2 3 4 5], [2 3 4 5 1]);
%! M1 = __graph_plot_subspace_embedding__ (G, 3);
%! M2 = __graph_plot_subspace_embedding__ (G, 3);
%! assert (M1, M2);

## Digraph and undirected graph with same underlying structure produce
## identical embeddings (digraph is symmetrised for the Laplacian).
%!test
%! Gd = digraph ([1 2 3], [2 3 1]);
%! Gu = graph ([1 2 3], [2 3 1]);
%! Md = __graph_plot_subspace_embedding__ (Gd, 2);
%! Mu = __graph_plot_subspace_embedding__ (Gu, 2);
%! assert (Md, Mu, 1e-10);

## Weights are ignored (same as laplacian() convention).
%!test
%! Gu = graph ([1 2 3], [2 3 1]);
%! Gw = graph ([1 2 3], [2 3 1], [10 20 30]);
%! Mu = __graph_plot_subspace_embedding__ (Gu, 2);
%! Mw = __graph_plot_subspace_embedding__ (Gw, 2);
%! assert (Mu, Mw, 1e-10);

## Self-loops are ignored (removed before building the Laplacian).
%!test
%! Gno = graph ([1 2], [2 3]);
%! Gsl = graph ([1 1 2], [1 2 3]);   # self-loop at node 1
%! Mno = __graph_plot_subspace_embedding__ (Gno, 2);
%! Msl = __graph_plot_subspace_embedding__ (Gsl, 2);
%! assert (Mno, Msl, 1e-10);

## Columns are orthonormal (within machine precision) when they are
## non-zero.
%!test
%! G = graph ([1 2 3 4 5], [2 3 4 5 1]);
%! M = __graph_plot_subspace_embedding__ (G, 3);
%! for k = 1:3
%!   assert (norm (M(:, k)), 1, 1e-10);
%! endfor
%! for k = 1:2
%!   assert (abs (M(:, k)' * M(:, k+1)) < 1e-10);
%! endfor

## Sign normalisation: largest-magnitude entry in every non-zero column
## is non-negative (positive by construction).
%!test
%! G = graph ([1 2 3 4 5], [2 3 4 5 1]);
%! M = __graph_plot_subspace_embedding__ (G, 3);
%! for k = 1:3
%!   col = M(:, k);
%!   [~, imax] = max (abs (col));
%!   if (any (col != 0))
%!     assert (col(imax) >= 0);
%!   endif
%! endfor

## Two-node graph: first column non-zero, rest zero-padded.
%!test
%! G = graph ([1], [2]);
%! M = __graph_plot_subspace_embedding__ (G, 3);
%! assert (size (M), [2, 3]);
%! ## Column 1: non-zero (Fiedler-ish, of form +/- [1, -1]/sqrt(2)).
%! assert (any (M(:, 1) != 0));
%! ## Columns 2 and 3: zero padding.
%! assert (M(:, 2), zeros (2, 1));
%! assert (M(:, 3), zeros (2, 1));

## Edgeless graph: all columns zero (every eigenvalue is 0).
%!test
%! G = graph (4);
%! M = __graph_plot_subspace_embedding__ (G, 3);
%! assert (M, zeros (4, 3));

## Dimension = 1 is permitted (returns one eigenvector).
%!test
%! G = graph ([1 2 3], [2 3 4]);
%! M = __graph_plot_subspace_embedding__ (G, 1);
%! assert (size (M), [4, 1]);
%! ## Corresponds to the Fiedler vector.
%! assert (all (isfinite (M(:, 1))));

## Errors.
%!error <graph or digraph> __graph_plot_subspace_embedding__ (1, 2)
%!error <DIMENSION> __graph_plot_subspace_embedding__ (graph (3), -1)
%!error <DIMENSION> __graph_plot_subspace_embedding__ (graph (3), 0)
%!error <DIMENSION> __graph_plot_subspace_embedding__ (graph (3), 1.5)
%!error <DIMENSION> __graph_plot_subspace_embedding__ (graph (3), [2 3])
%!error <DIMENSION> __graph_plot_subspace_embedding__ (graph (3), "two")
%!error <Invalid call> __graph_plot_subspace_embedding__ ()
%!error <Invalid call> __graph_plot_subspace_embedding__ (graph (3))
