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
## @deftypefn  {} {@var{h} =} plot (@var{G})
## @deftypefnx {} {@var{h} =} plot (@var{G}, @var{name}, @var{value}, @dots{})
## Render a @code{graph} or @code{digraph} and return a
## @code{GraphPlot} handle.
##
## @code{plot (@var{G})} places the nodes of @var{G} using the default
## @qcode{"auto"} layout (subspace for graphs with fewer than 100 nodes,
## Fruchterman-Reingold force otherwise), draws the edges as line
## segments, draws the nodes as markers, and returns the resulting
## @code{GraphPlot} handle.  Trailing @var{name}/@var{value} pairs
## forward to the @code{GraphPlot} constructor; see @code{GraphPlot} for
## the full list of accepted options.
##
## Recognised layout names for @qcode{"Layout"} include
## @qcode{"auto"} (default), @qcode{"circle"} (uniform unit-circle
## placement, node 1 at @code{(1, 0)}, remaining nodes counter-clockwise),
## @qcode{"subspace"}, and @qcode{"force"}.  Layout names are
## case-insensitive.
##
## When @var{G} is neither a @code{graph} nor a @code{digraph}, this
## file's free-function body is not used because Octave dispatches
## @code{plot} to its standard @file{scripts/plot/draw/plot} for
## numeric inputs.  The free-function body here exists primarily so
## @code{help plot} discovers the graph-overload docstring and so that
## explicit dispatch via @code{plot (G)} against a graph/digraph first
## argument resolves through this class file.
##
## @example
## @group
## G = digraph ([1 2 3], [2 3 1]);
## h = plot (G);
## @end group
## @end example
##
## @seealso{GraphPlot, graph, digraph}
## @end deftypefn

function h = plot (G, varargin)

  if (nargin < 1)
    print_usage ();
  endif

  if (! (isa (G, "graph") || isa (G, "digraph")))
    error ("Octave:invalid-input-arg", ...
           "plot: G must be a graph or digraph object");
  endif

  ## Defensive delegation: classdef method dispatch usually intercepts
  ## @code{plot (G, ...)} when G is a graph/digraph, but we keep the
  ## fallback explicit so the help text is self-contained.
  h = G.plot (varargin{:});

endfunction


## ---------------- BIST ----------------

## Basic call on a small digraph returns a GraphPlot handle.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = plot (G);
%!   assert (isa (h, "GraphPlot"));
%!   assert (h.NumNodes, 3);
%!   assert (h.NumEdges, 3);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Basic call on an undirected graph returns a GraphPlot handle.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = plot (G);
%!   assert (isa (h, "GraphPlot"));
%!   assert (h.NumNodes, 3);
%!   assert (h.NumEdges, 3);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Empty graph / digraph produces a GraphPlot with zero counts.
%!test
%! G = digraph ();
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = plot (G);
%!   assert (isa (h, "GraphPlot"));
%!   assert (h.NumNodes, 0);
%!   assert (h.NumEdges, 0);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Single-node digraph plots without error.
%!test
%! G = digraph (1);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = plot (G);
%!   assert (h.NumNodes, 1);
%!   assert (h.NumEdges, 0);
%!   assert (numel (h.XData), 1);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## 'auto' layout dispatches by size: small graphs get deterministic
## coordinates (stored in XData/YData as column vectors).
%!test
%! G = digraph ([1 2 3 4 5], [2 3 4 5 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = plot (G, "Layout", "auto");
%!   assert (iscolumn (h.XData));
%!   assert (iscolumn (h.YData));
%!   assert (numel (h.XData), 5);
%!   assert (numel (h.YData), 5);
%!   assert (all (isfinite (h.XData)));
%!   assert (all (isfinite (h.YData)));
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Plot works under the gnuplot headless toolkit as well.
%!test
%! gtk = graphics_toolkit ();
%! warn_state = warning ("off", "Octave:gnuplot-graphics");
%! hf = [];
%! unwind_protect
%!   graphics_toolkit ("gnuplot");
%!   hf = figure ("visible", "off");
%!   G = digraph ([1 2 3], [2 3 1]);
%!   h = plot (G);
%!   assert (isa (h, "GraphPlot"));
%!   assert (h.NumNodes, 3);
%! unwind_protect_cleanup
%!   if (! isempty (hf))
%!     close (hf);
%!   endif
%!   warning (warn_state);
%!   graphics_toolkit (gtk);
%! end_unwind_protect

## 'auto' layout with a 150-node digraph uses the large-graph branch
## and still produces finite coordinates.
%!test
%! N = 150;
%! s = 1:(N-1);
%! t = 2:N;
%! G = digraph (s, t);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = plot (G, "Layout", "auto");
%!   assert (numel (h.XData), N);
%!   assert (numel (h.YData), N);
%!   assert (all (isfinite (h.XData)));
%!   assert (all (isfinite (h.YData)));
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Custom XData / YData pass through to the GraphPlot.
%!test
%! G = digraph ([1 2], [2 3]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = plot (G, "XData", [0 1 2], "YData", [0 1 0]);
%!   assert (h.XData, [0; 1; 2]);
%!   assert (h.YData, [0; 1; 0]);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## -------- US-GP02 circle layout via plot() --------

## plot(G, 'Layout', 'circle') places nodes on the unit circle.
%!test
%! G = digraph ([1 2 3 4], [2 3 4 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = plot (G, "Layout", "circle");
%!   assert (isa (h, "GraphPlot"));
%!   assert (numel (h.XData), 4);
%!   assert (numel (h.YData), 4);
%!   assert (iscolumn (h.XData));
%!   assert (iscolumn (h.YData));
%!   assert (sqrt (h.XData.^2 + h.YData.^2), ones (4, 1), 1e-12);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Undirected graph + circle layout: unit-circle + uniform chord length.
%!test
%! G = graph ([1 2 3 4 5], [2 3 4 5 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = plot (G, "Layout", "circle");
%!   assert (isa (h, "GraphPlot"));
%!   assert (numel (h.XData), 5);
%!   assert (sqrt (h.XData.^2 + h.YData.^2), ones (5, 1), 1e-12);
%!   dx = diff ([h.XData; h.XData(1)]);
%!   dy = diff ([h.YData; h.YData(1)]);
%!   chord = sqrt (dx.^2 + dy.^2);
%!   expected = 2 * sin (pi / 5);
%!   assert (chord, expected * ones (5, 1), 1e-10);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Circle layout name is case-insensitive via plot().
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h1 = plot (G, "Layout", "circle");
%!   h2 = plot (G, "Layout", "CIRCLE");
%!   h3 = plot (G, "Layout", "Circle");
%!   assert (h1.XData, h2.XData, 1e-12);
%!   assert (h1.YData, h2.YData, 1e-12);
%!   assert (h1.XData, h3.XData, 1e-12);
%!   assert (h1.YData, h3.YData, 1e-12);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## N == 1 under circle layout: single node at origin.
%!test
%! G = digraph (1);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = plot (G, "Layout", "circle");
%!   assert (h.XData, 0);
%!   assert (h.YData, 0);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## N == 0 (empty digraph) under circle layout.
%!test
%! G = digraph ();
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = plot (G, "Layout", "circle");
%!   assert (isempty (h.XData));
%!   assert (isempty (h.YData));
%!   assert (h.NumNodes, 0);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## N == 100 under circle layout: still all on the unit circle.
%!test
%! G = digraph (100);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = plot (G, "Layout", "circle");
%!   assert (numel (h.XData), 100);
%!   assert (numel (h.YData), 100);
%!   assert (sqrt (h.XData.^2 + h.YData.^2), ones (100, 1), 1e-10);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Circle layout with named nodes + edge weights: XY driven by numnodes
## only.
%!test
%! G = digraph ({"a","b","c"}, {"b","c","a"}, [1 2 3], {"a","b","c"});
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = plot (G, "Layout", "circle");
%!   assert (numel (h.XData), 3);
%!   assert (sqrt (h.XData.^2 + h.YData.^2), ones (3, 1), 1e-12);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Isolated trailing nodes are laid out too (N-form constructor).
%!test
%! G = digraph ([1 2], [2 3], [1 1], 10);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = plot (G, "Layout", "circle");
%!   assert (h.NumNodes, 10);
%!   assert (numel (h.XData), 10);
%!   assert (sqrt (h.XData.^2 + h.YData.^2), ones (10, 1), 1e-12);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Circle layout is deterministic via plot() too.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h1 = plot (G, "Layout", "circle");
%!   h2 = plot (G, "Layout", "circle");
%!   assert (h1.XData, h2.XData);
%!   assert (h1.YData, h2.YData);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## %!demo — small directed cycle under gnuplot (safe headless demo).
%!demo
%! G = digraph ([1 2 3 4], [2 3 4 1]);
%! h = plot (G);
%! title ("digraph 4-cycle");

## %!demo — 6-node cycle laid out on the unit circle.
%!demo
%! G = graph ([1 2 3 4 5 6], [2 3 4 5 6 1]);
%! h = plot (G, "Layout", "circle");
%! title ("graph 6-cycle on unit circle");
%! axis equal;
