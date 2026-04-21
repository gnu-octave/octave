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
## @deftypefn  {} {[@var{X}, @var{Y}, @var{Z}] =} __graph_plot_force3__ (@var{G})
## @deftypefnx {} {[@var{X}, @var{Y}, @var{Z}] =} __graph_plot_force3__ (@var{G}, @var{weight_effect})
## Compute 3-D Fruchterman-Reingold force-directed layout for a
## @code{graph} or @code{digraph}.
##
## @var{G} must be a @code{graph} or @code{digraph}.  @var{weight_effect}
## is a lowercase string selecting how edge weights influence the
## attractive force between connected nodes:
##
## @table @code
## @item none
## Default.  Weights are ignored; every edge contributes unit
## attraction.
## @item direct
## The attractive force is multiplied by the edge weight: heavier
## edges pull their endpoints closer.
## @item inverse
## The attractive force is divided by the edge weight: heavier edges
## become @emph{longer} springs (common when weights encode distances).
## @end table
##
## Returns @var{X}, @var{Y}, and @var{Z} as column vectors of length
## @code{numnodes (@var{G})}.  The placement is always deterministic:
## the random initial positions used by the algorithm are seeded from a
## fixed value internally, then the caller's global RNG state is
## restored on exit.  @code{N == 0} returns @code{0-by-1} empty
## columns; @code{N == 1} returns the origin.
##
## This is the 3-D counterpart of @code{__graph_plot_force__}: the same
## repulsive/attractive force model is used but the displacement is
## computed in three dimensions with the ideal spring length
## @code{k = (1/N)^(1/3)} reflecting a unit-volume layout.
## @seealso{__graph_plot_force__, __graph_plot_auto_layout__, plot, GraphPlot}
## @end deftypefn

function [X, Y, Z] = __graph_plot_force3__ (G, weight_effect)

  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif
  if (! (isa (G, "graph") || isa (G, "digraph")))
    error ("Octave:invalid-input-arg", ...
           "__graph_plot_force3__: G must be a graph or digraph");
  endif
  if (nargin < 2)
    weight_effect = "none";
  endif
  if (! (ischar (weight_effect) && isrow (weight_effect)))
    error ("Octave:invalid-input-arg", ...
           "__graph_plot_force3__: WEIGHT_EFFECT must be a character vector");
  endif
  weight_effect = lower (weight_effect);
  if (! any (strcmp (weight_effect, {"none", "direct", "inverse"})))
    error ("Octave:invalid-input-arg", ...
           "__graph_plot_force3__: unknown WEIGHT_EFFECT '%s'", ...
           weight_effect);
  endif

  N = numnodes (G);

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

  ## Build a symmetric weighted adjacency matrix in which A(i, j) is the
  ## strength of the attractive force between nodes i and j.  Both
  ## directions of a directed edge contribute (digraph + undirected graph
  ## treated identically by F-R).  Self-loops cancel naturally because
  ## the pairwise displacement is zero for a self pair.
  A = adjacency (G, "weighted");
  A = A + A.';

  ## Translate weights into per-edge attractive-force factors.
  switch (weight_effect)
    case "none"
      W_eff = double (A > 0);
    case "direct"
      W_eff = full (A);
    case "inverse"
      W_eff = zeros (N, N);
      mask = (A != 0);
      if (any (mask(:)))
        W_eff(mask) = 1 ./ full (A(mask));
      endif
  endswitch

  ## Seed the global RNG to a fixed value for reproducible initial
  ## positions, and restore the caller's state on exit.
  rng_state = rand ("state");
  unwind_protect
    rand ("state", 42);
    pos = rand (N, 3) - 0.5;
  unwind_protect_cleanup
    rand ("state", rng_state);
  end_unwind_protect

  ## Ideal spring length for a unit-volume 3-D layout.  Standard F-R
  ## choice is k = C * (volume / N)^(1/dim); with C = 1 and volume = 1
  ## this gives k = (1/N)^(1/3).
  volume = 1;
  k = (volume / N)^(1/3);
  k2 = k * k;

  ## Iteration budget and initial "temperature" (maximum step per node
  ## per iteration).  100 iterations with a linear cool-down to zero is
  ## the classic F-R recipe.
  iters = 100;
  t0 = 0.1;

  eps_dist = 1e-9;

  for iter = 1:iters
    ## Pairwise displacements in each dimension.
    dx = pos(:, 1) - pos(:, 1).';
    dy = pos(:, 2) - pos(:, 2).';
    dz = pos(:, 3) - pos(:, 3).';
    dist = sqrt (dx.^2 + dy.^2 + dz.^2);

    ## Floor distances to avoid division by zero at coincident points.
    dist_safe = max (dist, eps_dist);

    ## Repulsive magnitude: k^2 / d between every pair.
    rep = k2 ./ dist_safe;

    ## Attractive magnitude: d^2 / k * W_eff(i, j).  W_eff is zero for
    ## non-adjacent pairs, so only edges contribute.
    att = (dist.^2 / k) .* W_eff;

    ## Net magnitude.  Zero out the diagonal so nodes don't push
    ## themselves.
    mag = rep - att;
    mag(1:(N+1):end) = 0;

    ## Force vector contributions in x, y, z.
    fx = mag .* (dx ./ dist_safe);
    fy = mag .* (dy ./ dist_safe);
    fz = mag .* (dz ./ dist_safe);

    ## Per-node displacement = sum over every other node.
    disp_x = sum (fx, 2);
    disp_y = sum (fy, 2);
    disp_z = sum (fz, 2);

    ## Limit displacement magnitude to the temperature.
    disp_mag = sqrt (disp_x.^2 + disp_y.^2 + disp_z.^2);
    disp_mag_safe = max (disp_mag, 1e-12);

    ## Linear cool-down: temperature shrinks to zero by the last
    ## iteration.
    t = t0 * (1 - (iter - 1) / iters);
    scale = min (disp_mag, t) ./ disp_mag_safe;

    pos(:, 1) = pos(:, 1) + disp_x .* scale;
    pos(:, 2) = pos(:, 2) + disp_y .* scale;
    pos(:, 3) = pos(:, 3) + disp_z .* scale;
  endfor

  X = pos(:, 1);
  Y = pos(:, 2);
  Z = pos(:, 3);

endfunction


## ---------------- BIST ----------------

## Empty graph: all three coordinate vectors are 0-by-1.
%!test
%! G = digraph ();
%! [X, Y, Z] = __graph_plot_force3__ (G);
%! assert (size (X), [0, 1]);
%! assert (size (Y), [0, 1]);
%! assert (size (Z), [0, 1]);

## Single-node graph: origin in 3-D.
%!test
%! G = digraph (1);
%! [X, Y, Z] = __graph_plot_force3__ (G);
%! assert (X, 0);
%! assert (Y, 0);
%! assert (Z, 0);

## 3-node triangle: finite coordinates, length N, column shape, 3-D.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! [X, Y, Z] = __graph_plot_force3__ (G);
%! assert (numel (X), 3);
%! assert (numel (Y), 3);
%! assert (numel (Z), 3);
%! assert (iscolumn (X));
%! assert (iscolumn (Y));
%! assert (iscolumn (Z));
%! assert (all (isfinite (X)));
%! assert (all (isfinite (Y)));
%! assert (all (isfinite (Z)));

## Z is not identically zero for a non-trivial graph (i.e. this really
## is a 3-D layout and not just a 2-D layout with a Z column).
%!test
%! G = digraph ([1 2 3 4 5], [2 3 4 5 1]);
%! [X, Y, Z] = __graph_plot_force3__ (G);
%! assert (any (abs (Z) > 1e-6));

## Deterministic: repeated calls produce identical coordinates.
%!test
%! G = digraph ([1 2 3 4 5], [2 3 4 5 1]);
%! [X1, Y1, Z1] = __graph_plot_force3__ (G);
%! [X2, Y2, Z2] = __graph_plot_force3__ (G);
%! assert (X1, X2);
%! assert (Y1, Y2);
%! assert (Z1, Z2);

## Determinism is independent of the caller's RNG state.
%!test
%! G = digraph ([1 2 3 4 5], [2 3 4 5 1]);
%! rand ("state", 1);
%! [X1, Y1, Z1] = __graph_plot_force3__ (G);
%! rand ("state", 999);
%! [X2, Y2, Z2] = __graph_plot_force3__ (G);
%! assert (X1, X2, 1e-12);
%! assert (Y1, Y2, 1e-12);
%! assert (Z1, Z2, 1e-12);

## Does not permanently disturb the caller's RNG state.
%!test
%! G = digraph ([1 2 3 4 5], [2 3 4 5 1]);
%! rand ("state", 7);
%! before = rand (1, 5);
%! rand ("state", 7);
%! [X, Y, Z] = __graph_plot_force3__ (G);
%! after = rand (1, 5);
%! assert (before, after, 1e-12);

## Explicit 'none' weight effect matches default.
%!test
%! G = digraph ([1 2 3], [2 3 1], [10 20 30]);
%! [X1, Y1, Z1] = __graph_plot_force3__ (G);
%! [X2, Y2, Z2] = __graph_plot_force3__ (G, "none");
%! assert (X1, X2);
%! assert (Y1, Y2);
%! assert (Z1, Z2);

## 'direct' weight effect differs from 'none' when weights are
## non-trivial.
%!test
%! G = digraph ([1 2 3], [2 3 1], [1 1 100]);
%! [Xn, Yn, Zn] = __graph_plot_force3__ (G, "none");
%! [Xd, Yd, Zd] = __graph_plot_force3__ (G, "direct");
%! assert (any (abs (Xn - Xd) > 1e-6) ...
%!         || any (abs (Yn - Yd) > 1e-6) ...
%!         || any (abs (Zn - Zd) > 1e-6));

## 'inverse' weight effect differs from 'none' when weights are
## non-trivial.
%!test
%! G = digraph ([1 2 3], [2 3 1], [1 1 100]);
%! [Xn, Yn, Zn] = __graph_plot_force3__ (G, "none");
%! [Xi, Yi, Zi] = __graph_plot_force3__ (G, "inverse");
%! assert (any (abs (Xn - Xi) > 1e-6) ...
%!         || any (abs (Yn - Yi) > 1e-6) ...
%!         || any (abs (Zn - Zi) > 1e-6));

## Weight-effect names are case-insensitive.
%!test
%! G = digraph ([1 2], [2 3], [5 5]);
%! [X1, Y1, Z1] = __graph_plot_force3__ (G, "direct");
%! [X2, Y2, Z2] = __graph_plot_force3__ (G, "DIRECT");
%! [X3, Y3, Z3] = __graph_plot_force3__ (G, "Direct");
%! assert (X1, X2);
%! assert (X1, X3);
%! assert (Y1, Y2);
%! assert (Y1, Y3);
%! assert (Z1, Z2);
%! assert (Z1, Z3);

## Weight-effect 'inverse' with an unweighted graph equals 'none'.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! [Xn, Yn, Zn] = __graph_plot_force3__ (G, "none");
%! [Xi, Yi, Zi] = __graph_plot_force3__ (G, "inverse");
%! assert (Xn, Xi, 1e-12);
%! assert (Yn, Yi, 1e-12);
%! assert (Zn, Zi, 1e-12);

## Undirected graph works with the same interface.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! [X, Y, Z] = __graph_plot_force3__ (G);
%! assert (numel (X), 3);
%! assert (numel (Z), 3);
%! assert (all (isfinite (X)));
%! assert (all (isfinite (Y)));
%! assert (all (isfinite (Z)));

## Graph with isolated nodes still returns coordinates for every node.
%!test
%! G = digraph ([1 2], [2 3], [], 6);
%! [X, Y, Z] = __graph_plot_force3__ (G);
%! assert (numel (X), 6);
%! assert (numel (Y), 6);
%! assert (numel (Z), 6);
%! assert (all (isfinite (X)));
%! assert (all (isfinite (Y)));
%! assert (all (isfinite (Z)));

## Self-loops do not crash.
%!test
%! G = digraph ([1 2 3], [1 3 1]);
%! [X, Y, Z] = __graph_plot_force3__ (G);
%! assert (numel (X), 3);
%! assert (all (isfinite (X)));
%! assert (all (isfinite (Y)));
%! assert (all (isfinite (Z)));

## Undirected graph and digraph with both directions yield identical
## layouts (F-R symmetrization).
%!test
%! Gd = digraph ([1 2 2 3 3 1], [2 1 3 2 1 3]);
%! Gu = graph ([1 2 3], [2 3 1]);
%! [Xd, Yd, Zd] = __graph_plot_force3__ (Gd);
%! [Xu, Yu, Zu] = __graph_plot_force3__ (Gu);
%! assert (Xd, Xu, 1e-10);
%! assert (Yd, Yu, 1e-10);
%! assert (Zd, Zu, 1e-10);

## Connected graph: coordinates stay in a bounded range (sanity check).
%!test
%! N = 10;
%! s = 1:(N-1);
%! t = 2:N;
%! G = digraph (s, t);
%! [X, Y, Z] = __graph_plot_force3__ (G);
%! assert (max (abs (X)), 5, 5);     # within [-10, 10] loosely
%! assert (max (abs (Y)), 5, 5);
%! assert (max (abs (Z)), 5, 5);

## Connected edges come out closer than non-adjacent pairs on a 4-node
## path graph.
%!test
%! G = graph ([1 2 3], [2 3 4]);
%! [X, Y, Z] = __graph_plot_force3__ (G);
%! P = [X, Y, Z];
%! d12 = norm (P(1, :) - P(2, :));
%! d34 = norm (P(3, :) - P(4, :));
%! d14 = norm (P(1, :) - P(4, :));
%! assert (d14 > d12);
%! assert (d14 > d34);

## The 3-D layout for a non-colinear graph has a non-trivial Z spread
## (the algorithm actually uses the third dimension, not just X-Y).
%!test
%! G = graph ([1 1 1 1 2 3 4 5], [2 3 4 5 3 4 5 2]);
%! [X, Y, Z] = __graph_plot_force3__ (G);
%! assert (max (Z) - min (Z) > 1e-3);

## 3-D layout differs from the 2-D layout (same graph).
%!test
%! G = graph ([1 2 3 4 5], [2 3 4 5 1]);
%! [X2, Y2] = __graph_plot_force__ (G);
%! [X3, Y3, Z3] = __graph_plot_force3__ (G);
%! ## Different algorithm in 3-D: not expected to match.
%! assert (any (abs (X2 - X3) > 1e-6) ...
%!         || any (abs (Y2 - Y3) > 1e-6) ...
%!         || any (abs (Z3) > 1e-6));

## Errors.
%!error <graph or digraph> __graph_plot_force3__ (1)
%!error <graph or digraph> __graph_plot_force3__ ("bogus")
%!error <character vector> __graph_plot_force3__ (digraph (3), 1)
%!error <unknown WEIGHT_EFFECT> __graph_plot_force3__ (digraph (3), "nope")
%!error <Invalid call> __graph_plot_force3__ ()
