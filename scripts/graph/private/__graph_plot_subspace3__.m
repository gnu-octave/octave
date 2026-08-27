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
## @deftypefn  {} {[@var{X}, @var{Y}, @var{Z}] =} __graph_plot_subspace3__ (@var{G})
## @deftypefnx {} {[@var{X}, @var{Y}, @var{Z}] =} __graph_plot_subspace3__ (@var{G}, @var{dimension})
## Compute a 3-D spectral (Hall-style) layout for a @code{graph} or
## @code{digraph} using Laplacian eigendecomposition.
##
## @var{G} must be a @code{graph} or @code{digraph}.  Directed edges are
## treated as undirected for the purpose of computing the Laplacian.
##
## @var{dimension} (optional) is the dimension of the embedding
## subspace used by the spectral computation, a positive integer with
## @math{dimension >= 3}.  The default is @code{min (100, numnodes (G))}
## but clipped at @code{3} below.  At least three eigenvectors are
## required to produce a 3-D layout; larger values have no effect on
## the output of this helper because only the first three components
## are used.
##
## Returns @var{X}, @var{Y}, and @var{Z} as column vectors of length
## @code{numnodes (@var{G})}.  The layout is fully deterministic:
## eigenvectors are sign-normalised so that repeat calls produce
## byte-identical output.  @code{N == 0} returns @code{0-by-1} empty
## columns; @code{N == 1} returns the origin.  When fewer than three
## non-trivial Laplacian eigenvectors exist (e.g.@: a two-node graph,
## or a highly-disconnected graph), the deficient axes are filled with
## zero vectors so the output shape is always consistent.
##
## Edge weights stored on @var{G} are ignored.  Self-loops are ignored.
## @seealso{__graph_plot_subspace__, __graph_plot_subspace_embedding__,
## __graph_plot_auto_layout__, plot, GraphPlot}
## @end deftypefn

function [X, Y, Z] = __graph_plot_subspace3__ (G, dimension)

  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif
  if (! (isa (G, "graph") || isa (G, "digraph")))
    error ("Octave:invalid-input-arg", ...
           "__graph_plot_subspace3__: G must be a graph or digraph");
  endif

  N = numnodes (G);

  ## Edge cases handled before DIMENSION validation.
  if (N == 0)
    X = zeros (0, 1);
    Y = zeros (0, 1);
    Z = zeros (0, 1);
    return;
  elseif (N == 1)
    X = 0;
    Y = 0;
    Z = 0;
    return;
  endif

  if (nargin < 2 || isempty (dimension))
    dimension = min (100, max (3, N));
  endif

  if (! (isnumeric (dimension) && isreal (dimension) && isscalar (dimension) ...
         && isfinite (dimension) && dimension == floor (dimension)))
    error ("Octave:invalid-input-arg", ...
           "__graph_plot_subspace3__: DIMENSION must be a positive integer scalar");
  endif
  dimension = double (dimension);
  if (dimension < 3)
    error ("Octave:invalid-input-arg", ...
           "__graph_plot_subspace3__: DIMENSION must be at least 3 for a 3-D subspace layout");
  endif
  if (dimension > N)
    error ("Octave:invalid-input-arg", ...
           "__graph_plot_subspace3__: DIMENSION cannot exceed numnodes (G)");
  endif

  ## Compute the spectral embedding and use the three principal
  ## eigenvector columns as the 3-D layout.
  M = __graph_plot_subspace_embedding__ (G, dimension);
  X = M(:, 1);
  Y = M(:, 2);
  Z = M(:, 3);

endfunction


## ---------------- BIST ----------------

## Empty graph: 0-by-1 columns for X, Y, Z.
%!test
%! G = digraph ();
%! [X, Y, Z] = __graph_plot_subspace3__ (G);
%! assert (size (X), [0, 1]);
%! assert (size (Y), [0, 1]);
%! assert (size (Z), [0, 1]);

## Single-node graph: origin.
%!test
%! G = digraph (1);
%! [X, Y, Z] = __graph_plot_subspace3__ (G);
%! assert (X, 0);
%! assert (Y, 0);
%! assert (Z, 0);

## 4-node triangle-fan: finite 3-D column coordinates.
%!test
%! G = graph ([1 1 1 2], [2 3 4 3]);
%! [X, Y, Z] = __graph_plot_subspace3__ (G);
%! assert (numel (X), 4);
%! assert (numel (Y), 4);
%! assert (numel (Z), 4);
%! assert (iscolumn (X));
%! assert (iscolumn (Y));
%! assert (iscolumn (Z));
%! assert (all (isfinite (X)));
%! assert (all (isfinite (Y)));
%! assert (all (isfinite (Z)));

## Determinism across repeat calls.
%!test
%! G = graph ([1 2 3 4 5], [2 3 4 5 1]);
%! [X1, Y1, Z1] = __graph_plot_subspace3__ (G);
%! [X2, Y2, Z2] = __graph_plot_subspace3__ (G);
%! assert (X1, X2);
%! assert (Y1, Y2);
%! assert (Z1, Z2);

## Z is non-trivial (non-zero somewhere) for a graph that has >= 3
## non-trivial Laplacian eigenvalues.
%!test
%! G = graph ([1 2 3 4 5], [2 3 4 5 1]);   # 5-cycle has 4 non-trivial eigenvalues
%! [~, ~, Z] = __graph_plot_subspace3__ (G);
%! assert (any (abs (Z) > 1e-6));

## Digraph and undirected graph with same structure yield identical
## 3-D layouts.
%!test
%! Gd = digraph ([1 2 3 4], [2 3 4 1]);
%! Gu = graph ([1 2 3 4], [2 3 4 1]);
%! [Xd, Yd, Zd] = __graph_plot_subspace3__ (Gd);
%! [Xu, Yu, Zu] = __graph_plot_subspace3__ (Gu);
%! assert (Xd, Xu, 1e-10);
%! assert (Yd, Yu, 1e-10);
%! assert (Zd, Zu, 1e-10);

## First two components agree with the 2-D subspace layout (same
## embedding).
%!test
%! G = graph ([1 2 3 4 5], [2 3 4 5 1]);
%! [X2, Y2] = __graph_plot_subspace__ (G);
%! [X3, Y3, ~] = __graph_plot_subspace3__ (G);
%! ## subspace uses default dim = min(100, N); subspace3 uses default
%! ## dim = min(100, N) too; both call embedding with the same D, so
%! ## columns 1-2 must match.
%! assert (X2, X3, 1e-10);
%! assert (Y2, Y3, 1e-10);

## Weights ignored.
%!test
%! Gu = graph ([1 2 3 4], [2 3 4 1]);
%! Gw = graph ([1 2 3 4], [2 3 4 1], [10 20 30 40]);
%! [Xu, Yu, Zu] = __graph_plot_subspace3__ (Gu);
%! [Xw, Yw, Zw] = __graph_plot_subspace3__ (Gw);
%! assert (Xu, Xw, 1e-10);
%! assert (Yu, Yw, 1e-10);
%! assert (Zu, Zw, 1e-10);

## Isolated nodes still receive coordinates.
%!test
%! G = graph ([1 2 3], [2 3 4], [], 6);   # nodes 5, 6 isolated
%! [X, Y, Z] = __graph_plot_subspace3__ (G);
%! assert (numel (X), 6);
%! assert (numel (Y), 6);
%! assert (numel (Z), 6);

## Explicit Dimension option works (minimum 3).
%!test
%! G = graph ([1 2 3 4], [2 3 4 1]);
%! [X, Y, Z] = __graph_plot_subspace3__ (G, 3);
%! assert (numel (X), 4);
%! assert (all (isfinite (Z)));

## Explicit Dimension option works (larger).
%!test
%! G = graph ([1 2 3 4 5], [2 3 4 5 1]);
%! [X, Y, Z] = __graph_plot_subspace3__ (G, 4);
%! assert (numel (X), 5);
%! assert (all (isfinite (X)));
%! assert (all (isfinite (Y)));
%! assert (all (isfinite (Z)));

## Empty dimension ([]) falls back to default.
%!test
%! G = graph ([1 2 3 4], [2 3 4 1]);
%! [X1, Y1, Z1] = __graph_plot_subspace3__ (G, []);
%! [X2, Y2, Z2] = __graph_plot_subspace3__ (G);
%! assert (X1, X2);
%! assert (Y1, Y2);
%! assert (Z1, Z2);

## Coordinates are bounded: unit-normalised eigenvectors have entries in
## [-1, 1].
%!test
%! G = graph (1:9, 2:10);   # 10-node path, has 9 non-trivial eigenvectors
%! [X, Y, Z] = __graph_plot_subspace3__ (G);
%! assert (max (abs (X)) <= 1 + 1e-10);
%! assert (max (abs (Y)) <= 1 + 1e-10);
%! assert (max (abs (Z)) <= 1 + 1e-10);

## Determinism independent of caller RNG state (no random numbers used).
%!test
%! G = graph ([1 2 3 4 5], [2 3 4 5 1]);
%! rand ("state", 1);
%! [X1, Y1, Z1] = __graph_plot_subspace3__ (G);
%! rand ("state", 12345);
%! [X2, Y2, Z2] = __graph_plot_subspace3__ (G);
%! assert (X1, X2);
%! assert (Y1, Y2);
%! assert (Z1, Z2);

## 3-node graph: only 2 non-trivial eigenvectors exist, so Z is the
## zero-padded column.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! [X, Y, Z] = __graph_plot_subspace3__ (G);
%! assert (Z, zeros (3, 1));

## 3-D layout differs from 2-D layout on the same graph (Z is a new
## axis).
%!test
%! G = graph ([1 2 3 4 5], [2 3 4 5 1]);
%! [X2, Y2]       = __graph_plot_subspace__ (G);
%! [X3, Y3, Z3]   = __graph_plot_subspace3__ (G);
%! assert (any (abs (Z3) > 1e-6));
%! ## X and Y should agree, Z is the new axis.
%! assert (X2, X3, 1e-10);
%! assert (Y2, Y3, 1e-10);

## Self-loops ignored.
%!test
%! Gno = graph ([1 2 3], [2 3 4]);
%! Gsl = graph ([1 1 2 3], [1 2 3 4]);   # self-loop at 1
%! [Xn, Yn, Zn] = __graph_plot_subspace3__ (Gno);
%! [Xs, Ys, Zs] = __graph_plot_subspace3__ (Gsl);
%! assert (Xn, Xs, 1e-10);
%! assert (Yn, Ys, 1e-10);
%! assert (Zn, Zs, 1e-10);

## Errors.
%!error <graph or digraph> __graph_plot_subspace3__ (1)
%!error <graph or digraph> __graph_plot_subspace3__ ("bogus")
%!error <DIMENSION> __graph_plot_subspace3__ (graph (4), "three")
%!error <DIMENSION> __graph_plot_subspace3__ (graph (4), 1.5)
%!error <DIMENSION> __graph_plot_subspace3__ (graph (4), [3 4])
%!error <at least 3> __graph_plot_subspace3__ (graph (4), 2)
%!error <at least 3> __graph_plot_subspace3__ (graph (4), -1)
%!error <cannot exceed numnodes> __graph_plot_subspace3__ (graph (4), 5)
%!error <Invalid call> __graph_plot_subspace3__ ()
