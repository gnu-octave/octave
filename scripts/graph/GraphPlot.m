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

classdef GraphPlot < handle

  ## -*- texinfo -*-
  ## @deftypefn  {} {@var{h} =} GraphPlot (@var{G})
  ## @deftypefnx {} {@var{h} =} GraphPlot (@var{G}, @var{name}, @var{value}, @dots{})
  ## Handle class representing a rendered graph or digraph figure.
  ##
  ## A @code{GraphPlot} stores per-node coordinates @code{XData} and
  ## @code{YData} (and optionally @code{ZData}), references to the
  ## underlying axes / line / scatter handles created when the graph was
  ## drawn, and the associated @code{graph} or @code{digraph} instance.
  ## The class is a handle so property updates reflect on the same
  ## rendered figure without returning a new object.
  ##
  ## @code{GraphPlot} objects are typically created by calling
  ## @code{plot (G)} on a @code{graph} or @code{digraph}.  Direct
  ## construction is reserved for internal use.
  ##
  ## Public properties populated for every @code{GraphPlot}:
  ## @table @code
  ## @item XData
  ## Column vector of node x-coordinates of length @code{numnodes (G)}.
  ## @item YData
  ## Column vector of node y-coordinates of length @code{numnodes (G)}.
  ## @item ZData
  ## Column vector of node z-coordinates (empty for 2-D layouts).
  ## @item NodeColor
  ## RGB triplet used to render the nodes.
  ## @item EdgeColor
  ## RGB triplet used to render the edges.
  ## @item Marker
  ## Marker style used to render the nodes.  Default @qcode{"o"}.
  ## @item MarkerSize
  ## Marker size used to render the nodes.  Default @code{4}.
  ## @item LineStyle
  ## Line style used to render the edges.  Default @qcode{"-"}.
  ## @item LineWidth
  ## Line width used to render the edges.  Default @code{0.5}.
  ## @item NumNodes
  ## Number of nodes in the underlying graph.  Read-only.
  ## @item NumEdges
  ## Number of edges in the underlying graph.  Read-only.
  ## @end table
  ## @seealso{graph, digraph, plot}
  ## @end deftypefn

  properties
    XData = zeros (0, 1);
    YData = zeros (0, 1);
    ZData = zeros (0, 1);
    NodeColor = [0 0.4470 0.7410];
    EdgeColor = [0 0.4470 0.7410];
    Marker = "o";
    MarkerSize = 4;
    LineStyle = "-";
    LineWidth = 0.5;
  endproperties

  properties (SetAccess = private)
    NumNodes = 0;
    NumEdges = 0;
  endproperties

  properties (Access = private)
    ## Axes handle the graph was drawn on.
    axes_ = [];

    ## Handle of the scatter / marker object that renders the nodes.
    node_handle_ = [];

    ## Column vector of line handles, one per rendered edge.
    edge_handles_ = [];

    ## Sources and targets of the rendered edges (numeric indices into
    ## XData / YData).  Stored so that later property updates can
    ## recompute edge coordinates without re-querying the graph.
    edge_src_ = zeros (0, 1);
    edge_dst_ = zeros (0, 1);
  endproperties

  methods

    function h = GraphPlot (varargin)

      ## -*- texinfo -*-
      ## @deftypefn  {} {@var{h} =} GraphPlot ()
      ## @deftypefnx {} {@var{h} =} GraphPlot (@var{G})
      ## @deftypefnx {} {@var{h} =} GraphPlot (@var{G}, @var{name}, @var{value}, @dots{})
      ## Construct a @code{GraphPlot} handle.
      ##
      ## With no arguments, return an empty @code{GraphPlot} with no
      ## rendered nodes or edges (used internally by @code{plot} for the
      ## empty-graph case).  With a @code{graph} or @code{digraph} as the
      ## first input, compute a layout (default @qcode{"auto"}: subspace
      ## for graphs with fewer than 100 nodes, Fruchterman-Reingold
      ## force otherwise) and render nodes and edges on the current axes.
      ## Trailing @var{name}/@var{value} pairs override layout and
      ## appearance properties; see @code{plot} for the full list.
      ## @seealso{plot, graph, digraph}
      ## @end deftypefn

      if (nargin == 0)
        return;
      endif

      G = varargin{1};
      if (! (isa (G, "graph") || isa (G, "digraph")))
        error ("Octave:invalid-input-arg", ...
               "GraphPlot: G must be a graph or digraph object");
      endif

      opts = varargin(2:end);
      if (mod (numel (opts), 2) != 0)
        error ("Octave:invalid-input-arg", ...
               "GraphPlot: name-value options must come in pairs");
      endif

      ## Default layout = "auto".  Recognised as key "Layout".
      layout = "auto";
      xdata_user = [];
      ydata_user = [];
      layout_opts = struct ();
      for ii = 1:2:numel (opts)
        name = opts{ii};
        if (! (ischar (name) && isrow (name)))
          error ("Octave:invalid-input-arg", ...
                 "GraphPlot: option names must be character vectors");
        endif
        val = opts{ii + 1};
        switch (lower (name))
          case "layout"
            if (! (ischar (val) && isrow (val)))
              error ("Octave:invalid-input-arg", ...
                     "GraphPlot: Layout must be a character vector");
            endif
            layout = lower (val);
          case "xdata"
            xdata_user = val(:);
          case "ydata"
            ydata_user = val(:);
          case "weighteffect"
            if (! (ischar (val) && isrow (val)))
              error ("Octave:invalid-input-arg", ...
                     "GraphPlot: WeightEffect must be a character vector");
            endif
            layout_opts.WeightEffect = lower (val);
          case "direction"
            if (! (ischar (val) && isrow (val)))
              error ("Octave:invalid-input-arg", ...
                     "GraphPlot: Direction must be a character vector");
            endif
            layout_opts.Direction = lower (val);
          case "sources"
            layout_opts.Sources = val;
          case "sinks"
            layout_opts.Sinks = val;
          case "assignlayers"
            if (! (ischar (val) && isrow (val)))
              error ("Octave:invalid-input-arg", ...
                     "GraphPlot: AssignLayers must be a character vector");
            endif
            layout_opts.AssignLayers = lower (val);
          otherwise
            error ("Octave:invalid-input-arg", ...
                   "GraphPlot: unknown option '%s'", name);
        endswitch
      endfor

      N = numnodes (G);
      h.NumNodes = N;
      h.NumEdges = numedges (G);

      ## Compute layout.  'auto' dispatches to subspace / force helpers
      ## by size threshold.  The optional third return value Z is
      ## populated by 3-D layouts (currently "force3" only) and left
      ## empty for every 2-D layout.
      if (! isempty (xdata_user) && ! isempty (ydata_user))
        if (numel (xdata_user) != N || numel (ydata_user) != N)
          error ("Octave:invalid-input-arg", ...
                 "GraphPlot: XData / YData length must equal numnodes (G)");
        endif
        X = double (xdata_user);
        Y = double (ydata_user);
        Z = zeros (0, 1);
      else
        [X, Y, Z] = __graph_plot_auto_layout__ (G, layout, layout_opts);
      endif

      h.XData = X(:);
      h.YData = Y(:);
      if (isempty (Z))
        h.ZData = zeros (0, 1);
      else
        h.ZData = Z(:);
      endif
      is_3d = ! isempty (h.ZData);

      ## Figure out the edge list once so we can plot edges and cache
      ## the source/destination indices for future redraws.
      if (h.NumEdges > 0)
        E = G.Edges;
        h.edge_src_ = E.EndNodes(:, 1);
        h.edge_dst_ = E.EndNodes(:, 2);
      else
        h.edge_src_ = zeros (0, 1);
        h.edge_dst_ = zeros (0, 1);
      endif

      ## Render on the current axes.  This creates a figure if none
      ## exists.  Rendering is wrapped in try/catch so that non-graphics
      ## environments (e.g.  @code{--no-window-system}) still construct a
      ## valid @code{GraphPlot} object even if the actual draw fails.
      ## 3-D layouts (ZData non-empty) draw edges via plot3 and nodes
      ## via scatter3-style marker plot so the axes gain a Z axis.
      try
        h.axes_ = newplot ();
        was_hold = ishold (h.axes_);
        if (! was_hold)
          hold (h.axes_, "on");
        endif

        ## Plot edges first so nodes draw on top.
        edge_handles = zeros (h.NumEdges, 1);
        for kk = 1:h.NumEdges
          s = h.edge_src_(kk);
          t = h.edge_dst_(kk);
          if (is_3d)
            edge_handles(kk) = plot3 (h.axes_, ...
                                      [h.XData(s), h.XData(t)], ...
                                      [h.YData(s), h.YData(t)], ...
                                      [h.ZData(s), h.ZData(t)], ...
                                      "Color", h.EdgeColor, ...
                                      "LineStyle", h.LineStyle, ...
                                      "LineWidth", h.LineWidth);
          else
            edge_handles(kk) = plot (h.axes_, ...
                                     [h.XData(s), h.XData(t)], ...
                                     [h.YData(s), h.YData(t)], ...
                                     "Color", h.EdgeColor, ...
                                     "LineStyle", h.LineStyle, ...
                                     "LineWidth", h.LineWidth);
          endif
        endfor
        h.edge_handles_ = edge_handles;

        ## Plot nodes as scatter markers.
        if (N > 0)
          if (is_3d)
            h.node_handle_ = plot3 (h.axes_, h.XData, h.YData, h.ZData, ...
                                    "LineStyle", "none", ...
                                    "Marker", h.Marker, ...
                                    "MarkerSize", h.MarkerSize, ...
                                    "Color", h.NodeColor, ...
                                    "MarkerFaceColor", h.NodeColor, ...
                                    "MarkerEdgeColor", h.NodeColor);
          else
            h.node_handle_ = plot (h.axes_, h.XData, h.YData, ...
                                   "LineStyle", "none", ...
                                   "Marker", h.Marker, ...
                                   "MarkerSize", h.MarkerSize, ...
                                   "Color", h.NodeColor, ...
                                   "MarkerFaceColor", h.NodeColor, ...
                                   "MarkerEdgeColor", h.NodeColor);
          endif
        endif

        if (! was_hold)
          hold (h.axes_, "off");
        endif
      catch err
        ## Rendering failed; keep data but leave graphics handles empty.
        h.axes_ = [];
        h.node_handle_ = [];
        h.edge_handles_ = [];
      end_try_catch

    endfunction

  endmethods

endclassdef


## ---------------- BIST ----------------

## Default construction: empty GraphPlot with zero counts.
%!test
%! h = GraphPlot ();
%! assert (isa (h, "GraphPlot"));
%! assert (h.NumNodes, 0);
%! assert (h.NumEdges, 0);
%! assert (isempty (h.XData));
%! assert (isempty (h.YData));

## Construct from a simple digraph, data populated, no error.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   assert (isa (h, "GraphPlot"));
%!   assert (h.NumNodes, 3);
%!   assert (h.NumEdges, 3);
%!   assert (numel (h.XData), 3);
%!   assert (numel (h.YData), 3);
%!   assert (iscolumn (h.XData));
%!   assert (iscolumn (h.YData));
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Construct from an undirected graph, same shape guarantees.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   assert (isa (h, "GraphPlot"));
%!   assert (h.NumNodes, 3);
%!   assert (h.NumEdges, 3);
%!   assert (numel (h.XData), 3);
%!   assert (numel (h.YData), 3);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Default colours match MATLAB-style default blue.
%!test
%! h = GraphPlot ();
%! assert (h.NodeColor, [0 0.4470 0.7410], 1e-12);
%! assert (h.EdgeColor, [0 0.4470 0.7410], 1e-12);
%! assert (h.Marker, "o");
%! assert (h.MarkerSize, 4);

## Handle-class semantics: shallow copy shares state.
%!test
%! h1 = GraphPlot ();
%! h2 = h1;
%! h2.XData = [1; 2; 3];
%! assert (h1.XData, [1; 2; 3]);

## Edgeless digraph still plots without error.
%!test
%! G = digraph (4);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   assert (h.NumNodes, 4);
%!   assert (h.NumEdges, 0);
%!   assert (numel (h.XData), 4);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## XData / YData overrides take precedence over the layout.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G, "XData", [0 1 2], "YData", [0 0 0]);
%!   assert (h.XData, [0; 1; 2]);
%!   assert (h.YData, [0; 0; 0]);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## XData / YData length mismatch errors out.
%!error <length> ...
%!   GraphPlot (digraph ([1 2], [2 3]), "XData", [0 1], "YData", [0 1])

## Non-graph first arg rejected.
%!error <graph or digraph> GraphPlot (1)
%!error <graph or digraph> GraphPlot ("bogus")

## Unknown option rejected.
%!error <unknown option> GraphPlot (digraph (2), "Bogus", 1)

## Odd number of name-value args rejected.
%!error <pairs> GraphPlot (digraph (2), "Layout")

## Layout must be a string.
%!error <character vector> GraphPlot (digraph (2), "Layout", 1)

## -------- US-GP02 circle layout via GraphPlot --------

## 'Layout','circle' selects unit-circle placement.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G, "Layout", "circle");
%!   assert (isa (h, "GraphPlot"));
%!   assert (sqrt (h.XData.^2 + h.YData.^2), ones (3, 1), 1e-12);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Circle layout matches between graph and digraph for same N / shape.
%!test
%! Gd = digraph (6);
%! Gg = graph (6);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hd = GraphPlot (Gd, "Layout", "circle");
%!   hg = GraphPlot (Gg, "Layout", "circle");
%!   assert (hd.XData, hg.XData);
%!   assert (hd.YData, hg.YData);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## XData / YData overrides win over the circle layout.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G, "Layout", "circle", ...
%!                  "XData", [10 20 30], "YData", [0 0 0]);
%!   assert (h.XData, [10; 20; 30]);
%!   assert (h.YData, [0; 0; 0]);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Circle layout node-1 starts at (1, 0) and advances counter-clockwise.
%!test
%! G = digraph (4);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G, "Layout", "circle");
%!   assert (h.XData, [1; 0; -1; 0], 1e-10);
%!   assert (h.YData, [0; 1; 0; -1], 1e-10);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## -------- US-GP03 force layout via GraphPlot --------

## 'Layout','force' picks the Fruchterman-Reingold layout.
%!test
%! G = digraph ([1 2 3 4], [2 3 4 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G, "Layout", "force");
%!   assert (isa (h, "GraphPlot"));
%!   assert (numel (h.XData), 4);
%!   assert (numel (h.YData), 4);
%!   assert (all (isfinite (h.XData)));
%!   assert (all (isfinite (h.YData)));
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Force layout is deterministic with seed reset in test.
%!test
%! G = digraph ([1 2 3 4 5], [2 3 4 5 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   rand ("state", 11);
%!   h1 = GraphPlot (G, "Layout", "force");
%!   rand ("state", 22);
%!   h2 = GraphPlot (G, "Layout", "force");
%!   assert (h1.XData, h2.XData, 1e-12);
%!   assert (h1.YData, h2.YData, 1e-12);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## 'WeightEffect','direct' passes through to the force layout.
%!test
%! G = digraph ([1 2 3], [2 3 1], [1 1 100]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hn = GraphPlot (G, "Layout", "force");
%!   hd = GraphPlot (G, "Layout", "force", "WeightEffect", "direct");
%!   assert (any (abs (hn.XData - hd.XData) > 1e-6) ...
%!           || any (abs (hn.YData - hd.YData) > 1e-6));
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## 'WeightEffect','inverse' passes through.
%!test
%! G = digraph ([1 2 3], [2 3 1], [1 1 100]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hi = GraphPlot (G, "Layout", "force", "WeightEffect", "inverse");
%!   assert (numel (hi.XData), 3);
%!   assert (all (isfinite (hi.XData)));
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## 'WeightEffect' values are case-insensitive.
%!test
%! G = digraph ([1 2 3], [2 3 1], [2 3 5]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h1 = GraphPlot (G, "Layout", "force", "WeightEffect", "Direct");
%!   h2 = GraphPlot (G, "Layout", "force", "WeightEffect", "DIRECT");
%!   h3 = GraphPlot (G, "Layout", "force", "WeightEffect", "direct");
%!   assert (h1.XData, h2.XData, 1e-12);
%!   assert (h1.XData, h3.XData, 1e-12);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## WeightEffect applies only when layout=force: with circle layout it
## is silently accepted but has no effect.
%!test
%! G = digraph ([1 2 3], [2 3 1], [1 2 3]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h1 = GraphPlot (G, "Layout", "circle");
%!   h2 = GraphPlot (G, "Layout", "circle", "WeightEffect", "direct");
%!   assert (h1.XData, h2.XData);
%!   assert (h1.YData, h2.YData);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## WeightEffect must be a string.
%!error <WeightEffect must be a character vector> ...
%!   GraphPlot (digraph (3), "WeightEffect", 1)

## Unknown WeightEffect value is reported by the force helper.
%!error <unknown WEIGHT_EFFECT> ...
%!   GraphPlot (digraph ([1 2], [2 3]), "Layout", "force", ...
%!              "WeightEffect", "nope")

## Auto layout with N >= 100 invokes force and respects WeightEffect.
%!test
%! N = 105;
%! G = digraph (1:(N-1), 2:N);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   ha = GraphPlot (G);   # auto = force because N >= 100
%!   hf2 = GraphPlot (G, "Layout", "force");
%!   assert (ha.XData, hf2.XData);
%!   assert (ha.YData, hf2.YData);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## -------- US-GP04 force3 (3-D force) layout via GraphPlot --------

## 'Layout','force3' populates ZData and returns a valid GraphPlot.
%!test
%! G = digraph ([1 2 3 4 5], [2 3 4 5 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G, "Layout", "force3");
%!   assert (isa (h, "GraphPlot"));
%!   assert (h.NumNodes, 5);
%!   assert (numel (h.XData), 5);
%!   assert (numel (h.YData), 5);
%!   assert (numel (h.ZData), 5);
%!   assert (iscolumn (h.XData));
%!   assert (iscolumn (h.YData));
%!   assert (iscolumn (h.ZData));
%!   assert (all (isfinite (h.XData)));
%!   assert (all (isfinite (h.YData)));
%!   assert (all (isfinite (h.ZData)));
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## 2-D layouts leave ZData empty; force3 leaves it N-long.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h2 = GraphPlot (G, "Layout", "force");
%!   h3 = GraphPlot (G, "Layout", "force3");
%!   assert (isempty (h2.ZData));
%!   assert (numel (h3.ZData), 3);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## force3 coordinates equal direct call to __graph_plot_force3__
## helper.
%!test
%! G = digraph ([1 2 3 4], [2 3 4 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G, "Layout", "force3");
%!   [Xh, Yh, Zh] = __graph_plot_force3__ (G);
%!   assert (h.XData, Xh);
%!   assert (h.YData, Yh);
%!   assert (h.ZData, Zh);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## force3 is deterministic and independent of caller RNG state.
%!test
%! G = digraph ([1 2 3 4 5], [2 3 4 5 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   rand ("state", 11);
%!   h1 = GraphPlot (G, "Layout", "force3");
%!   rand ("state", 99);
%!   h2 = GraphPlot (G, "Layout", "force3");
%!   assert (h1.XData, h2.XData, 1e-12);
%!   assert (h1.YData, h2.YData, 1e-12);
%!   assert (h1.ZData, h2.ZData, 1e-12);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## force3 layout name is case-insensitive.
%!test
%! G = digraph ([1 2], [2 3]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h1 = GraphPlot (G, "Layout", "force3");
%!   h2 = GraphPlot (G, "Layout", "FORCE3");
%!   h3 = GraphPlot (G, "Layout", "Force3");
%!   assert (h1.XData, h2.XData, 1e-12);
%!   assert (h1.ZData, h2.ZData, 1e-12);
%!   assert (h1.XData, h3.XData, 1e-12);
%!   assert (h1.ZData, h3.ZData, 1e-12);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## WeightEffect passes through to force3 (same mechanism as force).
%!test
%! G = digraph ([1 2 3], [2 3 1], [1 1 100]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hn = GraphPlot (G, "Layout", "force3");
%!   hd = GraphPlot (G, "Layout", "force3", "WeightEffect", "direct");
%!   assert (any (abs (hn.XData - hd.XData) > 1e-6) ...
%!           || any (abs (hn.YData - hd.YData) > 1e-6) ...
%!           || any (abs (hn.ZData - hd.ZData) > 1e-6));
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## force3 Z spread is non-zero on a non-trivial graph.
%!test
%! G = graph ([1 1 1 1 2 3 4 5], [2 3 4 5 3 4 5 2]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G, "Layout", "force3");
%!   assert (max (h.ZData) - min (h.ZData) > 1e-3);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Empty and single-node digraphs under force3.
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h0 = GraphPlot (digraph (), "Layout", "force3");
%!   assert (h0.NumNodes, 0);
%!   assert (isempty (h0.XData));
%!   assert (isempty (h0.ZData));
%!   h1 = GraphPlot (digraph (1), "Layout", "force3");
%!   assert (h1.NumNodes, 1);
%!   assert (h1.XData, 0);
%!   assert (h1.YData, 0);
%!   assert (h1.ZData, 0);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Undirected graph under force3 also produces 3-D coordinates.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G, "Layout", "force3");
%!   assert (isa (h, "GraphPlot"));
%!   assert (numel (h.ZData), 3);
%!   assert (all (isfinite (h.ZData)));
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## -------- US-GP05 layered (Sugiyama) layout via GraphPlot --------

## 'Layout','layered' produces finite column-shape coordinates.
%!test
%! G = digraph ([1 2 3], [2 3 4]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G, "Layout", "layered");
%!   assert (isa (h, "GraphPlot"));
%!   assert (h.NumNodes, 4);
%!   assert (numel (h.XData), 4);
%!   assert (numel (h.YData), 4);
%!   assert (iscolumn (h.XData));
%!   assert (iscolumn (h.YData));
%!   assert (all (isfinite (h.XData)));
%!   assert (all (isfinite (h.YData)));
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## layered matches the private helper exactly (deterministic).
%!test
%! G = digraph ([1 2 3], [2 3 4]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G, "Layout", "layered");
%!   [Xh, Yh] = __graph_plot_layered__ (G);
%!   assert (h.XData, Xh);
%!   assert (h.YData, Yh);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## 'Direction' passes through to the layered helper.
%!test
%! G = digraph ([1 2 3], [2 3 4]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h_d = GraphPlot (G, "Layout", "layered", "Direction", "down");
%!   h_u = GraphPlot (G, "Layout", "layered", "Direction", "up");
%!   assert (h_u.YData, -h_d.YData);
%!   assert (h_u.XData, h_d.XData);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## 'Direction','right': Y flat, X spans layer axis.
%!test
%! G = digraph ([1 2 3], [2 3 4]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G, "Layout", "layered", "Direction", "right");
%!   assert (h.YData, zeros (4, 1));
%!   assert (h.XData(1), 0);
%!   assert (h.XData(4), 3);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## 'Direction' is case-insensitive.
%!test
%! G = digraph ([1 2], [2 3]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h1 = GraphPlot (G, "Layout", "layered", "Direction", "Down");
%!   h2 = GraphPlot (G, "Layout", "layered", "Direction", "DOWN");
%!   h3 = GraphPlot (G, "Layout", "layered", "Direction", "down");
%!   assert (h1.XData, h2.XData);
%!   assert (h1.XData, h3.XData);
%!   assert (h1.YData, h2.YData);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## 'Sources' option forces a node into layer 1.
%!test
%! G = digraph ([1 2 3], [2 3 4]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G, "Layout", "layered", "Sources", 3);
%!   assert (h.YData(3), 0);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## 'Sinks' option forces a node into the last layer.
%!test
%! G = digraph ([1 2 3], [2 3 4]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G, "Layout", "layered", "Sinks", 1);
%!   assert (h.YData(1), min (h.YData));
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## 'AssignLayers','alap' differs from 'asap' when a node can be delayed.
%!test
%! G = digraph ([1 2 1], [2 4, 3]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h_as = GraphPlot (G, "Layout", "layered", "AssignLayers", "asap");
%!   h_al = GraphPlot (G, "Layout", "layered", "AssignLayers", "alap");
%!   assert (h_as.YData(3), -1);
%!   assert (h_al.YData(3), -2);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## 'AssignLayers' is case-insensitive.
%!test
%! G = digraph ([1 2 1], [2 4, 3]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h1 = GraphPlot (G, "Layout", "layered", "AssignLayers", "ALAP");
%!   h2 = GraphPlot (G, "Layout", "layered", "AssignLayers", "alap");
%!   assert (h1.YData, h2.YData);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## layered works on an undirected graph too (uses BFS from first
## source or node 1).
%!test
%! G = graph ([1 2 3], [2 3 4]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G, "Layout", "layered");
%!   assert (h.YData(1), 0);
%!   assert (h.YData(4), -3);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Empty graph under layered returns 0-node GraphPlot.
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (digraph (), "Layout", "layered");
%!   assert (h.NumNodes, 0);
%!   assert (isempty (h.XData));
%!   assert (isempty (h.YData));
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Layered options validated up front.
%!error <Direction must be a character vector> ...
%!   GraphPlot (digraph (3), "Layout", "layered", "Direction", 1)
%!error <AssignLayers must be a character vector> ...
%!   GraphPlot (digraph (3), "Layout", "layered", "AssignLayers", 1)

## Unknown direction/assignlayers values propagate as errors from the
## helper.
%!error <unknown DIRECTION> ...
%!   GraphPlot (digraph (3), "Layout", "layered", "Direction", "nowhere")
%!error <unknown ASSIGNLAYERS> ...
%!   GraphPlot (digraph (3), "Layout", "layered", "AssignLayers", "bogus")

## Layered options under non-layered layouts are silently ignored.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h1 = GraphPlot (G, "Layout", "circle");
%!   h2 = GraphPlot (G, "Layout", "circle", "Direction", "up", ...
%!                   "Sources", 2, "Sinks", 3, "AssignLayers", "alap");
%!   assert (h1.XData, h2.XData);
%!   assert (h1.YData, h2.YData);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Deterministic output across repeat calls.
%!test
%! G = digraph ([1 1 2 3], [2 3 4 4]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h1 = GraphPlot (G, "Layout", "layered");
%!   h2 = GraphPlot (G, "Layout", "layered");
%!   assert (h1.XData, h2.XData);
%!   assert (h1.YData, h2.YData);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
