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
## @deftypefn  {} {[@var{X}, @var{Y}] =} __graph_plot_force__ (@var{G})
## @deftypefnx {} {[@var{X}, @var{Y}] =} __graph_plot_force__ (@var{G}, @var{weight_effect})
## Compute 2-D Fruchterman-Reingold force-directed layout for a
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
## Returns @var{X} and @var{Y} as column vectors of length
## @code{numnodes (@var{G})}.  The placement is always deterministic:
## the random initial positions used by the algorithm are seeded from a
## fixed value internally, then the caller's global RNG state is
## restored on exit.  @code{N == 0} returns @code{0-by-1} empty
## columns; @code{N == 1} returns the origin.
##
## For a @code{digraph}, the directionality of edges does not affect
## the force computation: the attractive force between @math{u} and
## @math{v} depends only on whether a link exists, not on its
## orientation.  Parallel edges (multigraphs) contribute their summed
## weights to the attractive force between the endpoint pair.
## @seealso{__graph_plot_auto_layout__, plot, GraphPlot}
## @end deftypefn

function [X, Y] = __graph_plot_force__ (G, weight_effect)

  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif
  if (! (isa (G, "graph") || isa (G, "digraph")))
    error ("Octave:invalid-input-arg", ...
           "__graph_plot_force__: G must be a graph or digraph");
  endif
  if (nargin < 2)
    weight_effect = "none";
  endif
  if (! (ischar (weight_effect) && isrow (weight_effect)))
    error ("Octave:invalid-input-arg", ...
           "__graph_plot_force__: WEIGHT_EFFECT must be a character vector");
  endif
  weight_effect = lower (weight_effect);
  if (! any (strcmp (weight_effect, {"none", "direct", "inverse"})))
    error ("Octave:invalid-input-arg", ...
           "__graph_plot_force__: unknown WEIGHT_EFFECT '%s'", ...
           weight_effect);
  endif

  N = numnodes (G);

  if (N == 0)
    X = zeros (0, 1);
    Y = zeros (0, 1);
    return;
  elseif (N == 1)
    X = 0;
    Y = 0;
    return;
  endif

  ## Build a symmetric weighted adjacency matrix in which A(i, j) is the
  ## strength of the attractive force between nodes i and j.  Both
  ## directions of a directed edge contribute (digraph + undirected
  ## graph treated identically by F-R).
  A = adjacency (G, "weighted");
  A = A + A.';
  ## Symmetrizing doubles the entries on self-loops and on bidirectional
  ## edge pairs, but the relative ordering of forces is preserved and
  ## self-loops cancel anyway because dx/dy are zero.

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
    pos = rand (N, 2) - 0.5;
  unwind_protect_cleanup
    rand ("state", rng_state);
  end_unwind_protect

  ## Ideal spring length.  The standard F-R choice is
  ## k = C * sqrt (area / N); we use area = 1 and C = 1 which yields
  ## k ~= 1 / sqrt (N), a reasonable scale for a unit-sized layout.
  area = 1;
  k = sqrt (area / N);
  k2 = k * k;

  ## Iteration budget and initial "temperature" (maximum step per node
  ## per iteration).  100 iterations with a linear cool-down to zero is
  ## the classic F-R recipe.
  iters = 100;
  t0 = 0.1;

  eps_dist = 1e-9;

  for iter = 1:iters
    ## Pairwise displacements: dx(i, j) = pos(i, 1) - pos(j, 1).  The
    ## row-minus-column-transpose broadcast produces an NxN matrix of
    ## signed differences that is antisymmetric.
    dx = pos(:, 1) - pos(:, 1).';
    dy = pos(:, 2) - pos(:, 2).';
    dist = sqrt (dx.^2 + dy.^2);

    ## Floor distances to avoid division by zero at coincident points.
    dist_safe = max (dist, eps_dist);

    ## Repulsive magnitude: k^2 / d between every pair (including
    ## self-pairs, which will zero out below because dx/dy are 0).
    rep = k2 ./ dist_safe;

    ## Attractive magnitude: d^2 / k * W_eff(i, j).  W_eff is zero for
    ## non-adjacent pairs, so only edges contribute.
    att = (dist.^2 / k) .* W_eff;

    ## Net magnitude (positive -> push apart, negative -> pull
    ## together).  Zero out the diagonal so nodes don't push themselves.
    mag = rep - att;
    mag(1:(N+1):end) = 0;

    ## Force vector contributions in x and y.
    fx = mag .* (dx ./ dist_safe);
    fy = mag .* (dy ./ dist_safe);

    ## Per-node displacement = sum of force contributions from every
    ## other node.
    disp_x = sum (fx, 2);
    disp_y = sum (fy, 2);

    ## Limit displacement magnitude to the temperature.
    disp_mag = sqrt (disp_x.^2 + disp_y.^2);
    disp_mag_safe = max (disp_mag, 1e-12);

    ## Linear cool-down: temperature shrinks to zero by the last
    ## iteration.
    t = t0 * (1 - (iter - 1) / iters);
    scale = min (disp_mag, t) ./ disp_mag_safe;

    pos(:, 1) = pos(:, 1) + disp_x .* scale;
    pos(:, 2) = pos(:, 2) + disp_y .* scale;
  endfor

  X = pos(:, 1);
  Y = pos(:, 2);

endfunction


## ---------------- BIST ----------------

## Empty graph: both coordinate vectors are 0-by-1.
%!test
%! G = digraph ();
%! [X, Y] = __graph_plot_force__ (G);
%! assert (size (X), [0, 1]);
%! assert (size (Y), [0, 1]);

## Single-node graph: origin.
%!test
%! G = digraph (1);
%! [X, Y] = __graph_plot_force__ (G);
%! assert (X, 0);
%! assert (Y, 0);

## 3-node triangle: finite coordinates, length N, column shape.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! [X, Y] = __graph_plot_force__ (G);
%! assert (numel (X), 3);
%! assert (numel (Y), 3);
%! assert (iscolumn (X));
%! assert (iscolumn (Y));
%! assert (all (isfinite (X)));
%! assert (all (isfinite (Y)));

## Deterministic: repeated calls produce identical coordinates.
%!test
%! G = digraph ([1 2 3 4 5], [2 3 4 5 1]);
%! [X1, Y1] = __graph_plot_force__ (G);
%! [X2, Y2] = __graph_plot_force__ (G);
%! assert (X1, X2);
%! assert (Y1, Y2);

## Determinism is independent of the caller's RNG state (the helper
## seeds internally and restores).
%!test
%! G = digraph ([1 2 3 4 5], [2 3 4 5 1]);
%! rand ("state", 1);
%! [X1, Y1] = __graph_plot_force__ (G);
%! rand ("state", 999);
%! [X2, Y2] = __graph_plot_force__ (G);
%! assert (X1, X2, 1e-12);
%! assert (Y1, Y2, 1e-12);

## Does not permanently disturb the caller's RNG state: before/after
## rand () calls produce the same sequence whether or not the layout
## was called in between.
%!test
%! G = digraph ([1 2 3 4 5], [2 3 4 5 1]);
%! rand ("state", 7);
%! before = rand (1, 5);
%! rand ("state", 7);
%! [X, Y] = __graph_plot_force__ (G);
%! after = rand (1, 5);
%! assert (before, after, 1e-12);

## Explicit 'none' weight effect matches default.
%!test
%! G = digraph ([1 2 3], [2 3 1], [10 20 30]);
%! [X1, Y1] = __graph_plot_force__ (G);
%! [X2, Y2] = __graph_plot_force__ (G, "none");
%! assert (X1, X2);
%! assert (Y1, Y2);

## 'direct' weight effect differs from 'none' when weights are
## non-trivial (edges with larger weights pull closer).
%!test
%! G = digraph ([1 2 3], [2 3 1], [1 1 100]);
%! [Xn, Yn] = __graph_plot_force__ (G, "none");
%! [Xd, Yd] = __graph_plot_force__ (G, "direct");
%! assert (any (abs (Xn - Xd) > 1e-6) || any (abs (Yn - Yd) > 1e-6));

## 'inverse' weight effect differs from 'none' when weights are
## non-trivial.
%!test
%! G = digraph ([1 2 3], [2 3 1], [1 1 100]);
%! [Xn, Yn] = __graph_plot_force__ (G, "none");
%! [Xi, Yi] = __graph_plot_force__ (G, "inverse");
%! assert (any (abs (Xn - Xi) > 1e-6) || any (abs (Yn - Yi) > 1e-6));

## Weight effect names are case-insensitive.
%!test
%! G = digraph ([1 2], [2 3], [5 5]);
%! [X1, Y1] = __graph_plot_force__ (G, "direct");
%! [X2, Y2] = __graph_plot_force__ (G, "DIRECT");
%! [X3, Y3] = __graph_plot_force__ (G, "Direct");
%! assert (X1, X2);
%! assert (X1, X3);
%! assert (Y1, Y2);
%! assert (Y1, Y3);

## Weight-effect 'inverse' with an unweighted graph equals 'none'
## (every edge has unit weight -> 1/1 == 1).
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! [Xn, Yn] = __graph_plot_force__ (G, "none");
%! [Xi, Yi] = __graph_plot_force__ (G, "inverse");
%! assert (Xn, Xi, 1e-12);
%! assert (Yn, Yi, 1e-12);

## Undirected graph works with the same interface.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! [X, Y] = __graph_plot_force__ (G);
%! assert (numel (X), 3);
%! assert (all (isfinite (X)));
%! assert (all (isfinite (Y)));

## Graph with isolated nodes still returns coordinates for every node.
%!test
%! G = digraph ([1 2], [2 3], [], 6);
%! [X, Y] = __graph_plot_force__ (G);
%! assert (numel (X), 6);
%! assert (numel (Y), 6);
%! assert (all (isfinite (X)));
%! assert (all (isfinite (Y)));

## Self-loops do not crash (the dx/dy for a self-loop is identically 0
## so the self-interaction is naturally zeroed).
%!test
%! G = digraph ([1 2 3], [1 3 1]);
%! [X, Y] = __graph_plot_force__ (G);
%! assert (numel (X), 3);
%! assert (all (isfinite (X)));
%! assert (all (isfinite (Y)));

## Graph and digraph with the same underlying structure (edges in both
## directions) yield identical layouts under the F-R symmetrization.
%!test
%! Gd = digraph ([1 2 2 3 3 1], [2 1 3 2 1 3]);
%! Gu = graph ([1 2 3], [2 3 1]);
%! [Xd, Yd] = __graph_plot_force__ (Gd);
%! [Xu, Yu] = __graph_plot_force__ (Gu);
%! assert (Xd, Xu, 1e-10);
%! assert (Yd, Yu, 1e-10);

## Connected graph: coordinates stay in a bounded range (sanity check
## — no divergence).
%!test
%! N = 10;
%! s = 1:(N-1);
%! t = 2:N;
%! G = digraph (s, t);
%! [X, Y] = __graph_plot_force__ (G);
%! assert (max (abs (X)), 5, 5);     # within [-10, 10] loosely
%! assert (max (abs (Y)), 5, 5);

## Connected edges come out closer than non-adjacent pairs on average
## (gross sanity check for a 4-node path graph).
%!test
%! G = graph ([1 2 3], [2 3 4]);
%! [X, Y] = __graph_plot_force__ (G);
%! P = [X, Y];
%! d12 = norm (P(1, :) - P(2, :));
%! d34 = norm (P(3, :) - P(4, :));
%! d14 = norm (P(1, :) - P(4, :));
%! assert (d14 > d12);
%! assert (d14 > d34);

## Errors.
%!error <graph or digraph> __graph_plot_force__ (1)
%!error <graph or digraph> __graph_plot_force__ ("bogus")
%!error <character vector> __graph_plot_force__ (digraph (3), 1)
%!error <unknown WEIGHT_EFFECT> __graph_plot_force__ (digraph (3), "nope")
%!error <Invalid call> __graph_plot_force__ ()
