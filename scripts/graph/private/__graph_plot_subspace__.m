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
## @deftypefn  {} {[@var{X}, @var{Y}] =} __graph_plot_subspace__ (@var{G})
## @deftypefnx {} {[@var{X}, @var{Y}] =} __graph_plot_subspace__ (@var{G}, @var{dimension})
## Compute a 2-D spectral (Hall-style) layout for a @code{graph} or
## @code{digraph} using Laplacian eigendecomposition.
##
## @var{G} must be a @code{graph} or @code{digraph}.  Directed edges are
## treated as undirected for the purpose of computing the Laplacian.
##
## @var{dimension} (optional) is the dimension of the embedding
## subspace used by the spectral computation, a positive integer with
## @math{dimension >= 2}.  The default is @code{min (100, numnodes (G))}
## but clipped at @code{2} below.  At least two eigenvectors are
## required to produce a 2-D layout; larger values have no effect on
## the 2-D output of this helper because only the first two components
## are used (higher-dimension embeddings are a placeholder for future
## refinement).
##
## Returns @var{X} and @var{Y} as column vectors of length
## @code{numnodes (@var{G})}.  The layout is fully deterministic:
## eigenvectors are sign-normalised so that repeat calls produce
## byte-identical output.  @code{N == 0} returns @code{0-by-1} empty
## columns; @code{N == 1} returns the origin.
##
## Edge weights stored on @var{G} are ignored.  Self-loops are ignored.
## @seealso{__graph_plot_subspace3__, __graph_plot_subspace_embedding__,
## __graph_plot_auto_layout__, plot, GraphPlot}
## @end deftypefn

function [X, Y] = __graph_plot_subspace__ (G, dimension)

  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif
  if (! (isa (G, "graph") || isa (G, "digraph")))
    error ("Octave:invalid-input-arg", ...
           "__graph_plot_subspace__: G must be a graph or digraph");
  endif

  N = numnodes (G);

  ## Edge cases handled before DIMENSION is consulted so the default
  ## (min(100, N)) doesn't trip the "dimension > N" validation on
  ## single-node or empty inputs.
  if (N == 0)
    X = zeros (0, 1);
    Y = zeros (0, 1);
    return;
  elseif (N == 1)
    X = 0;
    Y = 0;
    return;
  endif

  if (nargin < 2 || isempty (dimension))
    dimension = min (100, max (2, N));
  endif

  if (! (isnumeric (dimension) && isreal (dimension) && isscalar (dimension) ...
         && isfinite (dimension) && dimension == floor (dimension)))
    error ("Octave:invalid-input-arg", ...
           "__graph_plot_subspace__: DIMENSION must be a positive integer scalar");
  endif
  dimension = double (dimension);
  if (dimension < 2)
    error ("Octave:invalid-input-arg", ...
           "__graph_plot_subspace__: DIMENSION must be at least 2 for a 2-D subspace layout");
  endif
  if (dimension > N)
    error ("Octave:invalid-input-arg", ...
           "__graph_plot_subspace__: DIMENSION cannot exceed numnodes (G)");
  endif

  ## Compute the spectral embedding in the specified subspace dimension
  ## and use the two principal eigenvector columns as the 2-D layout.
  M = __graph_plot_subspace_embedding__ (G, dimension);
  X = M(:, 1);
  Y = M(:, 2);

endfunction


## ---------------- BIST ----------------

## Empty graph: 0-by-1 columns.
%!test
%! G = digraph ();
%! [X, Y] = __graph_plot_subspace__ (G);
%! assert (size (X), [0, 1]);
%! assert (size (Y), [0, 1]);

## Single-node graph: origin.
%!test
%! G = digraph (1);
%! [X, Y] = __graph_plot_subspace__ (G);
%! assert (X, 0);
%! assert (Y, 0);

## Triangle: finite column coordinates of length 3.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! [X, Y] = __graph_plot_subspace__ (G);
%! assert (numel (X), 3);
%! assert (numel (Y), 3);
%! assert (iscolumn (X));
%! assert (iscolumn (Y));
%! assert (all (isfinite (X)));
%! assert (all (isfinite (Y)));

## Deterministic across repeat calls.
%!test
%! G = graph ([1 2 3 4 5], [2 3 4 5 1]);
%! [X1, Y1] = __graph_plot_subspace__ (G);
%! [X2, Y2] = __graph_plot_subspace__ (G);
%! assert (X1, X2);
%! assert (Y1, Y2);

## Digraph and undirected graph with same structure yield identical
## layouts (Laplacian is symmetrised).
%!test
%! Gd = digraph ([1 2 3], [2 3 1]);
%! Gu = graph  ([1 2 3], [2 3 1]);
%! [Xd, Yd] = __graph_plot_subspace__ (Gd);
%! [Xu, Yu] = __graph_plot_subspace__ (Gu);
%! assert (Xd, Xu, 1e-10);
%! assert (Yd, Yu, 1e-10);

## Path graph 1-2-3-4: Fiedler vector is monotone along the path.  The
## X coordinate (first non-trivial eigenvector) must be monotonic in
## node index.
%!test
%! G = graph ([1 2 3], [2 3 4]);
%! [X, Y] = __graph_plot_subspace__ (G);
%! assert (numel (X), 4);
%! d = diff (X);
%! assert (all (d > 0) || all (d < 0));

## Weights are ignored (uses the same unweighted Laplacian as
## laplacian()).
%!test
%! Gu = graph ([1 2 3], [2 3 1]);
%! Gw = graph ([1 2 3], [2 3 1], [100 200 300]);
%! [Xu, Yu] = __graph_plot_subspace__ (Gu);
%! [Xw, Yw] = __graph_plot_subspace__ (Gw);
%! assert (Xu, Xw, 1e-10);
%! assert (Yu, Yw, 1e-10);

## Isolated nodes still receive coordinates (zero eigenvectors padded).
%!test
%! G = graph ([1 2], [2 3], [], 5);   # nodes 4, 5 isolated
%! [X, Y] = __graph_plot_subspace__ (G);
%! assert (numel (X), 5);
%! assert (numel (Y), 5);
%! assert (all (isfinite (X)));
%! assert (all (isfinite (Y)));

## Explicit Dimension option accepted: minimum dimension (2) works.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! [X, Y] = __graph_plot_subspace__ (G, 2);
%! assert (numel (X), 3);
%! assert (all (isfinite (X)));

## Explicit Dimension option accepted: larger dimension also works.
%!test
%! G = graph ([1 2 3 4 5], [2 3 4 5 1]);
%! [X, Y] = __graph_plot_subspace__ (G, 4);
%! assert (numel (X), 5);
%! assert (all (isfinite (X)));
%! assert (all (isfinite (Y)));

## Default dimension matches min(100, N) for small N.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! [X1, Y1] = __graph_plot_subspace__ (G);
%! [X2, Y2] = __graph_plot_subspace__ (G, 3);   # min(100, 3) == 3
%! assert (X1, X2, 1e-12);
%! assert (Y1, Y2, 1e-12);

## Connected graph: coordinates stay finite and bounded (Laplacian
## eigenvectors are unit-normalised so entries are in [-1, 1]).
%!test
%! G = graph (1:9, 2:10);   # 10-node path
%! [X, Y] = __graph_plot_subspace__ (G);
%! assert (max (abs (X)) <= 1 + 1e-10);
%! assert (max (abs (Y)) <= 1 + 1e-10);

## Determinism is independent of the caller's RNG state (no random
## numbers are used in spectral layout).
%!test
%! G = graph ([1 2 3 4 5], [2 3 4 5 1]);
%! rand ("state", 1);
%! [X1, Y1] = __graph_plot_subspace__ (G);
%! rand ("state", 999);
%! [X2, Y2] = __graph_plot_subspace__ (G);
%! assert (X1, X2);
%! assert (Y1, Y2);

## Two-node graph with an edge: X values have opposite sign, Y is all
## zero (only one non-trivial eigenvector exists).
%!test
%! G = graph ([1], [2]);
%! [X, Y] = __graph_plot_subspace__ (G);
%! assert (numel (X), 2);
%! assert (Y, zeros (2, 1));
%! assert (sign (X(1)) != sign (X(2)) || X(1) == 0);

## Empty opts (dimension) passed as [] is treated as default.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! [X1, Y1] = __graph_plot_subspace__ (G, []);
%! [X2, Y2] = __graph_plot_subspace__ (G);
%! assert (X1, X2);
%! assert (Y1, Y2);

## Errors.
%!error <graph or digraph> __graph_plot_subspace__ (1)
%!error <graph or digraph> __graph_plot_subspace__ ("bogus")
%!error <DIMENSION> __graph_plot_subspace__ (graph (3), "two")
%!error <DIMENSION> __graph_plot_subspace__ (graph (3), 1.5)
%!error <DIMENSION> __graph_plot_subspace__ (graph (3), [2 3])
%!error <at least 2> __graph_plot_subspace__ (graph (3), 1)
%!error <at least 2> __graph_plot_subspace__ (graph (3), -1)
%!error <cannot exceed numnodes> __graph_plot_subspace__ (graph (3), 4)
%!error <Invalid call> __graph_plot_subspace__ ()
