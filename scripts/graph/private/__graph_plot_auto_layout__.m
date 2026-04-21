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
## @deftypefn  {} {[@var{X}, @var{Y}] =} __graph_plot_auto_layout__ (@var{G}, @var{layout})
## @deftypefnx {} {[@var{X}, @var{Y}] =} __graph_plot_auto_layout__ (@var{G}, @var{layout}, @var{opts})
## @deftypefnx {} {[@var{X}, @var{Y}, @var{Z}] =} __graph_plot_auto_layout__ (@dots{})
## Compute node coordinates for @code{plot}'ting a @code{graph} or
## @code{digraph}.
##
## @var{G} must be a @code{graph} or @code{digraph}.  @var{layout} is a
## lowercase string selecting the layout algorithm.  Recognised values:
##
## @table @code
## @item auto
## Default.  Dispatches by node count: fewer than 100 nodes use the
## @qcode{"subspace"} branch, the rest use the @qcode{"force"} branch.
## @item subspace
## Placeholder routed to a deterministic circle layout (to be replaced
## by the proper spectral subspace algorithm in a subsequent story).
## @item force
## Fruchterman-Reingold 2-D force-directed layout.  See
## @code{__graph_plot_force__} for details.  When the graph is
## weighted, the @code{WeightEffect} option selects how edge weights
## enter the attractive force: @qcode{"none"} (default),
## @qcode{"direct"}, or @qcode{"inverse"}.
## @item force3
## Fruchterman-Reingold 3-D force-directed layout.  See
## @code{__graph_plot_force3__} for details.  Honours the same
## @code{WeightEffect} options as the 2-D @qcode{"force"} layout.
## This is the only currently-supported 3-D layout: it is the layout
## that populates a non-empty @var{Z} output.
## @item layered
## Sugiyama-style hierarchical layout based on longest-path layer
## assignment plus iterated barycenter crossing reduction.  See
## @code{__graph_plot_layered__}.  Honours @code{Direction},
## @code{Sources}, @code{Sinks}, and @code{AssignLayers} options.
## @item circle
## Production layout.  Place the @var{N} nodes of @var{G} uniformly on
## the unit circle at angles @code{theta(k) = 2*pi*(k-1)/N}.  Node 1
## lands at @code{(1, 0)} and the remaining nodes fan out
## counter-clockwise.  @code{N == 0} returns @code{0-by-1} empty
## columns; @code{N == 1} returns the origin (a single node cannot
## define a unit circle position).
## @end table
##
## @var{opts} (optional) is a struct holding layout-specific options.
## Currently recognised fields:
## @table @code
## @item WeightEffect
## Character vector forwarded to @code{__graph_plot_force__} or
## @code{__graph_plot_force3__} when the @qcode{"force"} /
## @qcode{"force3"} branches are active.  Ignored by the other layouts.
## @item Direction
## Character vector forwarded to @code{__graph_plot_layered__} when the
## @qcode{"layered"} branch is active.  Ignored by the other layouts.
## @item Sources
## Numeric vector of node indices forwarded to
## @code{__graph_plot_layered__}.  Ignored by the other layouts.
## @item Sinks
## Numeric vector of node indices forwarded to
## @code{__graph_plot_layered__}.  Ignored by the other layouts.
## @item AssignLayers
## Character vector forwarded to @code{__graph_plot_layered__}.
## Ignored by the other layouts.
## @end table
##
## Returns @var{X} and @var{Y} as column vectors of length
## @code{numnodes (G)}.  The optional third output @var{Z} is populated
## only by 3-D layouts (currently @qcode{"force3"}); 2-D layouts return
## @code{zeros (0, 1)} for @var{Z}.
## @seealso{plot, GraphPlot, __graph_plot_force__, __graph_plot_force3__,
## __graph_plot_layered__}
## @end deftypefn

function [X, Y, Z] = __graph_plot_auto_layout__ (G, layout, opts)

  if (nargin < 2 || nargin > 3)
    print_usage ();
  endif
  if (! (isa (G, "graph") || isa (G, "digraph")))
    error ("Octave:invalid-input-arg", ...
           "__graph_plot_auto_layout__: G must be a graph or digraph");
  endif
  if (! (ischar (layout) && isrow (layout)))
    error ("Octave:invalid-input-arg", ...
           "__graph_plot_auto_layout__: LAYOUT must be a character vector");
  endif
  if (nargin < 3 || isempty (opts))
    opts = struct ();
  endif
  if (! isstruct (opts))
    error ("Octave:invalid-input-arg", ...
           "__graph_plot_auto_layout__: OPTS must be a struct");
  endif

  ## Default WeightEffect when the caller did not supply one.
  if (isfield (opts, "WeightEffect"))
    weight_effect = opts.WeightEffect;
  else
    weight_effect = "none";
  endif

  ## Layered-layout options with sensible defaults.
  if (isfield (opts, "Direction"))
    direction = opts.Direction;
  else
    direction = "down";
  endif
  if (isfield (opts, "Sources"))
    sources = opts.Sources;
  else
    sources = [];
  endif
  if (isfield (opts, "Sinks"))
    sinks = opts.Sinks;
  else
    sinks = [];
  endif
  if (isfield (opts, "AssignLayers"))
    assign_layers = opts.AssignLayers;
  else
    assign_layers = "auto";
  endif

  N = numnodes (G);
  layout = lower (layout);

  ## Default Z: empty column for every 2-D layout.  The force3 branch
  ## overwrites it with the 3-D coordinate vector.
  Z = zeros (0, 1);

  switch (layout)
    case "auto"
      ## Auto dispatches by node count: fewer than 100 nodes use the
      ## "subspace" branch (still a circle placeholder until US-GP06);
      ## 100 and above use the production "force" branch.
      if (N < 100)
        [X, Y] = __gp_layout_circle__ (N);
      else
        [X, Y] = __graph_plot_force__ (G, weight_effect);
      endif
    case "subspace"
      [X, Y] = __gp_layout_circle__ (N);
    case "force"
      [X, Y] = __graph_plot_force__ (G, weight_effect);
    case "force3"
      [X, Y, Z] = __graph_plot_force3__ (G, weight_effect);
    case "layered"
      [X, Y] = __graph_plot_layered__ (G, direction, sources, sinks, ...
                                       assign_layers);
    case "circle"
      [X, Y] = __gp_layout_circle__ (N);
    otherwise
      error ("Octave:invalid-input-arg", ...
             "__graph_plot_auto_layout__: unknown layout '%s'", layout);
  endswitch

endfunction


## Local helper: unit-circle placement used as the deterministic
## fallback for every layout branch at the US-GP01 checkpoint.
function [X, Y] = __gp_layout_circle__ (N)

  if (N == 0)
    X = zeros (0, 1);
    Y = zeros (0, 1);
    return;
  elseif (N == 1)
    X = 0;
    Y = 0;
    return;
  endif

  theta = (2 * pi) * ((0:(N - 1)).') / N;
  X = cos (theta);
  Y = sin (theta);

endfunction


## ---------------- BIST ----------------

## Empty graph: both coordinate vectors are 0-by-1.
%!test
%! G = digraph ();
%! [X, Y] = __graph_plot_auto_layout__ (G, "auto");
%! assert (size (X), [0, 1]);
%! assert (size (Y), [0, 1]);

## Single-node graph: origin.
%!test
%! G = digraph (1);
%! [X, Y] = __graph_plot_auto_layout__ (G, "auto");
%! assert (X, 0);
%! assert (Y, 0);

## Small graph uses the subspace branch; finite coordinates, length N.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! [X, Y] = __graph_plot_auto_layout__ (G, "auto");
%! assert (numel (X), 3);
%! assert (numel (Y), 3);
%! assert (all (isfinite (X)));
%! assert (all (isfinite (Y)));

## Large graph uses the force branch; finite coordinates, length N.
%!test
%! N = 150;
%! G = digraph (1:(N-1), 2:N);
%! [X, Y] = __graph_plot_auto_layout__ (G, "auto");
%! assert (numel (X), N);
%! assert (numel (Y), N);
%! assert (all (isfinite (X)));
%! assert (all (isfinite (Y)));

## Explicit layout names all accepted.
%!test
%! G = digraph ([1 2], [2 3]);
%! for name = {"auto", "subspace", "force", "circle"}
%!   [X, Y] = __graph_plot_auto_layout__ (G, name{1});
%!   assert (numel (X), 3);
%!   assert (numel (Y), 3);
%!   assert (all (isfinite (X)));
%!   assert (all (isfinite (Y)));
%! endfor

## Layout names are case-insensitive.
%!test
%! G = digraph ([1 2], [2 3]);
%! [X1, Y1] = __graph_plot_auto_layout__ (G, "auto");
%! [X2, Y2] = __graph_plot_auto_layout__ (G, "AUTO");
%! assert (X1, X2);
%! assert (Y1, Y2);

## Circle layout places nodes on the unit circle.
%!test
%! G = digraph (6);
%! [X, Y] = __graph_plot_auto_layout__ (G, "circle");
%! assert (sqrt (X.^2 + Y.^2), ones (6, 1), 1e-12);

## Undirected graphs are handled identically.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! [X, Y] = __graph_plot_auto_layout__ (G, "auto");
%! assert (numel (X), 3);
%! assert (numel (Y), 3);

## Errors.
%!error <graph or digraph> __graph_plot_auto_layout__ (1, "auto")
%!error <character vector> __graph_plot_auto_layout__ (digraph (3), 1)
%!error <unknown layout> __graph_plot_auto_layout__ (digraph (3), "nope")
%!error <Invalid call> __graph_plot_auto_layout__ (digraph (3))

## -------- US-GP02 circle layout: additional coverage --------

## N == 0 under 'circle' returns 0-by-1 columns.
%!test
%! G = digraph ();
%! [X, Y] = __graph_plot_auto_layout__ (G, "circle");
%! assert (size (X), [0, 1]);
%! assert (size (Y), [0, 1]);

## N == 1 under 'circle' returns the origin.
%!test
%! G = digraph (1);
%! [X, Y] = __graph_plot_auto_layout__ (G, "circle");
%! assert (X, 0);
%! assert (Y, 0);

## N == 2 under 'circle' places nodes diametrically opposite on the
## unit circle.
%!test
%! G = digraph (2);
%! [X, Y] = __graph_plot_auto_layout__ (G, "circle");
%! assert (sqrt (X.^2 + Y.^2), ones (2, 1), 1e-12);
%! assert (X(2), -X(1), 1e-12);
%! assert (Y(2), -Y(1), 1e-12);

## Consecutive chord lengths are equal for every N (uniform spacing).
%!test
%! for N = [3 4 5 6 8 12 25 100]
%!   G = digraph (N);
%!   [X, Y] = __graph_plot_auto_layout__ (G, "circle");
%!   assert (sqrt (X.^2 + Y.^2), ones (N, 1), 1e-10);
%!   dx = diff ([X; X(1)]);
%!   dy = diff ([Y; Y(1)]);
%!   chord = sqrt (dx.^2 + dy.^2);
%!   expected = 2 * sin (pi / N);
%!   assert (chord, expected * ones (N, 1), 1e-10);
%! endfor

## Circle layout starts at angle 0 (node 1 at (1, 0)) and advances
## counter-clockwise.
%!test
%! G = digraph (4);
%! [X, Y] = __graph_plot_auto_layout__ (G, "circle");
%! assert (X, [1; 0; -1; 0], 1e-10);
%! assert (Y, [0; 1; 0; -1], 1e-10);

## Circle layout is deterministic -- same G -> same X/Y across calls.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! [X1, Y1] = __graph_plot_auto_layout__ (G, "circle");
%! [X2, Y2] = __graph_plot_auto_layout__ (G, "circle");
%! assert (X1, X2);
%! assert (Y1, Y2);

## Circle layout works on an undirected graph with the same rules.
%!test
%! G = graph ([1 2 3 4], [2 3 4 1]);
%! [X, Y] = __graph_plot_auto_layout__ (G, "circle");
%! assert (sqrt (X.^2 + Y.^2), ones (4, 1), 1e-12);
%! assert (X, [1; 0; -1; 0], 1e-10);

## Circle layout for a named graph: coordinates driven by numnodes
## only, independent of node names and of edge weights.
%!test
%! G1 = digraph ([1 2 3], [2 3 1]);
%! G2 = digraph ([1 2 3], [2 3 1], [10 20 30]);
%! G3 = digraph ({"a","b","c"}, {"b","c","a"}, [], {"a","b","c"});
%! [X1, Y1] = __graph_plot_auto_layout__ (G1, "circle");
%! [X2, Y2] = __graph_plot_auto_layout__ (G2, "circle");
%! [X3, Y3] = __graph_plot_auto_layout__ (G3, "circle");
%! assert (X1, X2); assert (Y1, Y2);
%! assert (X1, X3); assert (Y1, Y3);

## Circle layout honours the full node count including isolated
## trailing nodes introduced via the N-form constructor.
%!test
%! G = digraph ([1 2], [2 3], [1 1], 10);
%! [X, Y] = __graph_plot_auto_layout__ (G, "circle");
%! assert (numel (X), 10);
%! assert (numel (Y), 10);
%! assert (sqrt (X.^2 + Y.^2), ones (10, 1), 1e-12);

## Circle layout returns column vectors regardless of N.
%!test
%! for N = [0 1 2 3 7 50]
%!   G = digraph (N);
%!   [X, Y] = __graph_plot_auto_layout__ (G, "circle");
%!   assert (iscolumn (X) || isempty (X));
%!   assert (iscolumn (Y) || isempty (Y));
%! endfor

## -------- US-GP03 force layout: integration coverage --------

## 'force' layout now delegates to Fruchterman-Reingold.  Coordinates
## must be finite and column-shaped.
%!test
%! G = digraph ([1 2 3 4], [2 3 4 1]);
%! [X, Y] = __graph_plot_auto_layout__ (G, "force");
%! assert (numel (X), 4);
%! assert (numel (Y), 4);
%! assert (iscolumn (X));
%! assert (iscolumn (Y));
%! assert (all (isfinite (X)));
%! assert (all (isfinite (Y)));

## Force layout is deterministic across repeat calls.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! [X1, Y1] = __graph_plot_auto_layout__ (G, "force");
%! [X2, Y2] = __graph_plot_auto_layout__ (G, "force");
%! assert (X1, X2);
%! assert (Y1, Y2);

## Force layout differs from circle placement on the same graph.
%!test
%! G = digraph ([1 2 3 4], [2 3 4 1]);
%! [Xc, Yc] = __graph_plot_auto_layout__ (G, "circle");
%! [Xf, Yf] = __graph_plot_auto_layout__ (G, "force");
%! assert (any (abs (Xc - Xf) > 1e-6) || any (abs (Yc - Yf) > 1e-6));

## 'auto' on a large graph (N >= 100) routes to force, not circle.
## The two layouts should produce different coordinates.
%!test
%! N = 120;
%! G = digraph (1:(N-1), 2:N);
%! [Xa, Ya] = __graph_plot_auto_layout__ (G, "auto");
%! [Xc, Yc] = __graph_plot_auto_layout__ (G, "circle");
%! assert (any (abs (Xa - Xc) > 1e-6) || any (abs (Ya - Yc) > 1e-6));

## 'auto' on a large graph matches 'force'.
%!test
%! N = 110;
%! G = digraph (1:(N-1), 2:N);
%! [Xa, Ya] = __graph_plot_auto_layout__ (G, "auto");
%! [Xf, Yf] = __graph_plot_auto_layout__ (G, "force");
%! assert (Xa, Xf);
%! assert (Ya, Yf);

## 'auto' on a small graph still uses the subspace placeholder (circle),
## NOT force.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! [Xa, Ya] = __graph_plot_auto_layout__ (G, "auto");
%! [Xc, Yc] = __graph_plot_auto_layout__ (G, "circle");
%! assert (Xa, Xc);
%! assert (Ya, Yc);

## opts.WeightEffect forwards to the force branch.
%!test
%! G = digraph ([1 2 3], [2 3 1], [1 1 100]);
%! [X_none, Y_none] = __graph_plot_auto_layout__ (G, "force");
%! opts.WeightEffect = "direct";
%! [X_dir, Y_dir] = __graph_plot_auto_layout__ (G, "force", opts);
%! assert (any (abs (X_none - X_dir) > 1e-6) ...
%!         || any (abs (Y_none - Y_dir) > 1e-6));

## opts.WeightEffect='inverse' forwards correctly.
%!test
%! G = digraph ([1 2 3], [2 3 1], [1 1 100]);
%! opts.WeightEffect = "inverse";
%! [X, Y] = __graph_plot_auto_layout__ (G, "force", opts);
%! assert (numel (X), 3);
%! assert (all (isfinite (X)));

## Empty opts struct is allowed and ignored.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! opts = struct ();
%! [X1, Y1] = __graph_plot_auto_layout__ (G, "force", opts);
%! [X2, Y2] = __graph_plot_auto_layout__ (G, "force");
%! assert (X1, X2);
%! assert (Y1, Y2);

## [] passed for opts is treated as the default struct.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! [X1, Y1] = __graph_plot_auto_layout__ (G, "force", []);
%! [X2, Y2] = __graph_plot_auto_layout__ (G, "force");
%! assert (X1, X2);
%! assert (Y1, Y2);

## Non-struct opts is rejected.
%!error <OPTS must be a struct> ...
%!   __graph_plot_auto_layout__ (digraph (3), "force", "nope")

## WeightEffect is ignored for non-force layouts (no error from unknown
## WeightEffect under "circle" because the force branch is not
## entered).
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! opts.WeightEffect = "bogus_value_never_reached";
%! [X1, Y1] = __graph_plot_auto_layout__ (G, "circle", opts);
%! [X2, Y2] = __graph_plot_auto_layout__ (G, "circle");
%! assert (X1, X2);
%! assert (Y1, Y2);

## -------- US-GP04 force3 (3-D force) layout: coverage --------

## 'force3' layout returns 3-D coordinates via the optional 3rd output.
%!test
%! G = digraph ([1 2 3 4], [2 3 4 1]);
%! [X, Y, Z] = __graph_plot_auto_layout__ (G, "force3");
%! assert (numel (X), 4);
%! assert (numel (Y), 4);
%! assert (numel (Z), 4);
%! assert (iscolumn (X));
%! assert (iscolumn (Y));
%! assert (iscolumn (Z));
%! assert (all (isfinite (X)));
%! assert (all (isfinite (Y)));
%! assert (all (isfinite (Z)));

## force3 layout Z is non-trivial (not all zeros).
%!test
%! G = digraph ([1 2 3 4 5], [2 3 4 5 1]);
%! [~, ~, Z] = __graph_plot_auto_layout__ (G, "force3");
%! assert (any (abs (Z) > 1e-6));

## force3 layout matches the direct __graph_plot_force3__ helper.
%!test
%! G = digraph ([1 2 3 4], [2 3 4 1]);
%! [Xa, Ya, Za] = __graph_plot_auto_layout__ (G, "force3");
%! [Xh, Yh, Zh] = __graph_plot_force3__ (G);
%! assert (Xa, Xh);
%! assert (Ya, Yh);
%! assert (Za, Zh);

## force3 layout name is case-insensitive.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! [X1, Y1, Z1] = __graph_plot_auto_layout__ (G, "force3");
%! [X2, Y2, Z2] = __graph_plot_auto_layout__ (G, "FORCE3");
%! [X3, Y3, Z3] = __graph_plot_auto_layout__ (G, "Force3");
%! assert (X1, X2);
%! assert (X1, X3);
%! assert (Z1, Z2);

## force3 forwards opts.WeightEffect to the helper.
%!test
%! G = digraph ([1 2 3], [2 3 1], [1 1 100]);
%! [Xn, Yn, Zn] = __graph_plot_auto_layout__ (G, "force3");
%! opts.WeightEffect = "direct";
%! [Xd, Yd, Zd] = __graph_plot_auto_layout__ (G, "force3", opts);
%! assert (any (abs (Xn - Xd) > 1e-6) ...
%!         || any (abs (Yn - Yd) > 1e-6) ...
%!         || any (abs (Zn - Zd) > 1e-6));

## 2-D layouts return an empty Z column when the 3rd output is
## requested.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! [X, Y, Z] = __graph_plot_auto_layout__ (G, "circle");
%! assert (size (Z), [0, 1]);
%! [X, Y, Z] = __graph_plot_auto_layout__ (G, "force");
%! assert (size (Z), [0, 1]);
%! [X, Y, Z] = __graph_plot_auto_layout__ (G, "subspace");
%! assert (size (Z), [0, 1]);
%! [X, Y, Z] = __graph_plot_auto_layout__ (G, "auto");
%! assert (size (Z), [0, 1]);

## force3 on an empty graph yields 0-by-1 columns for X, Y, and Z.
%!test
%! G = digraph ();
%! [X, Y, Z] = __graph_plot_auto_layout__ (G, "force3");
%! assert (size (X), [0, 1]);
%! assert (size (Y), [0, 1]);
%! assert (size (Z), [0, 1]);

## Backward-compatible 2-output call on a 3-D layout still works
## (3rd output is simply dropped).
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! [X, Y] = __graph_plot_auto_layout__ (G, "force3");
%! assert (numel (X), 3);
%! assert (numel (Y), 3);
%! assert (all (isfinite (X)));
%! assert (all (isfinite (Y)));

## -------- US-GP05 layered layout: integration coverage --------

## 'layered' layout produces finite column-shape coordinates.
%!test
%! G = digraph ([1 2 3], [2 3 4]);
%! [X, Y] = __graph_plot_auto_layout__ (G, "layered");
%! assert (numel (X), 4);
%! assert (numel (Y), 4);
%! assert (iscolumn (X));
%! assert (iscolumn (Y));
%! assert (all (isfinite (X)));
%! assert (all (isfinite (Y)));

## 'layered' default matches the helper with default arguments.
%!test
%! G = digraph ([1 2 3], [2 3 4]);
%! [Xa, Ya] = __graph_plot_auto_layout__ (G, "layered");
%! [Xh, Yh] = __graph_plot_layered__ (G);
%! assert (Xa, Xh);
%! assert (Ya, Yh);

## 'layered' name is case-insensitive.
%!test
%! G = digraph ([1 2], [2 3]);
%! [X1, Y1] = __graph_plot_auto_layout__ (G, "layered");
%! [X2, Y2] = __graph_plot_auto_layout__ (G, "LAYERED");
%! [X3, Y3] = __graph_plot_auto_layout__ (G, "Layered");
%! assert (X1, X2);
%! assert (X1, X3);
%! assert (Y1, Y2);
%! assert (Y1, Y3);

## Direction option flows through to the layered helper.
%!test
%! G = digraph ([1 2 3], [2 3 4]);
%! opts.Direction = "up";
%! [Xa, Ya] = __graph_plot_auto_layout__ (G, "layered", opts);
%! [Xh, Yh] = __graph_plot_layered__ (G, "up");
%! assert (Xa, Xh);
%! assert (Ya, Yh);

## Direction "right" flows through.
%!test
%! G = digraph ([1 2], [2 3]);
%! opts.Direction = "right";
%! [Xa, Ya] = __graph_plot_auto_layout__ (G, "layered", opts);
%! [Xh, Yh] = __graph_plot_layered__ (G, "right");
%! assert (Xa, Xh);
%! assert (Ya, Yh);

## Sources option flows through.
%!test
%! G = digraph ([1 2 3], [2 3 4]);
%! opts.Sources = 3;
%! [Xa, Ya] = __graph_plot_auto_layout__ (G, "layered", opts);
%! [Xh, Yh] = __graph_plot_layered__ (G, "down", 3);
%! assert (Xa, Xh);
%! assert (Ya, Yh);

## Sinks option flows through.
%!test
%! G = digraph ([1 2 3], [2 3 4]);
%! opts.Sinks = 1;
%! [Xa, Ya] = __graph_plot_auto_layout__ (G, "layered", opts);
%! [Xh, Yh] = __graph_plot_layered__ (G, "down", [], 1);
%! assert (Xa, Xh);
%! assert (Ya, Yh);

## AssignLayers option flows through.
%!test
%! G = digraph ([1 2 1], [2 4, 3]);
%! opts.AssignLayers = "alap";
%! [Xa, Ya] = __graph_plot_auto_layout__ (G, "layered", opts);
%! [Xh, Yh] = __graph_plot_layered__ (G, "down", [], [], "alap");
%! assert (Xa, Xh);
%! assert (Ya, Yh);

## 3rd output Z is empty for the layered layout.
%!test
%! G = digraph ([1 2 3], [2 3 4]);
%! [X, Y, Z] = __graph_plot_auto_layout__ (G, "layered");
%! assert (size (Z), [0, 1]);

## layered on an empty graph returns 0-by-1 columns.
%!test
%! G = digraph ();
%! [X, Y] = __graph_plot_auto_layout__ (G, "layered");
%! assert (size (X), [0, 1]);
%! assert (size (Y), [0, 1]);

## layered on a single-node graph returns origin.
%!test
%! G = digraph (1);
%! [X, Y] = __graph_plot_auto_layout__ (G, "layered");
%! assert (X, 0);
%! assert (Y, 0);

## layered on an undirected graph works.
%!test
%! G = graph ([1 2 3], [2 3 4]);
%! [X, Y] = __graph_plot_auto_layout__ (G, "layered");
%! assert (numel (X), 4);
%! assert (Y(1), 0);

## Layered options are ignored by non-layered layouts.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! opts.Direction = "up";
%! opts.Sources = 1;
%! opts.Sinks = 3;
%! opts.AssignLayers = "alap";
%! [X1, Y1] = __graph_plot_auto_layout__ (G, "circle", opts);
%! [X2, Y2] = __graph_plot_auto_layout__ (G, "circle");
%! assert (X1, X2);
%! assert (Y1, Y2);
