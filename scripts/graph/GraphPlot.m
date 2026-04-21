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
  ## RGB triplet or color name used to render the nodes.
  ## @item EdgeColor
  ## RGB triplet or color name used to render the edges.
  ## @item Marker
  ## Marker style used to render the nodes.  Default @qcode{"o"}.
  ## @item MarkerSize
  ## Marker size used to render the nodes.  Default @code{4}.
  ## @item LineStyle
  ## Line style used to render the edges.  Default @qcode{"-"}.
  ## @item LineWidth
  ## Line width used to render the edges.  Default @code{0.5}.
  ## @item NodeLabel
  ## Column cell array of strings, one per node, used as text labels
  ## drawn next to each node marker.  Defaults to the graph's
  ## @code{Nodes.Name} column when present, or to the string indices
  ## @qcode{"1"}, @qcode{"2"}, @dots{} otherwise.
  ## @item NodeLabelMode
  ## @qcode{"auto"} (default) means @code{NodeLabel} is regenerated from
  ## the underlying graph; @qcode{"manual"} means the current
  ## @code{NodeLabel} is preserved.  Assigning to @code{NodeLabel}
  ## implicitly switches @code{NodeLabelMode} to @qcode{"manual"};
  ## re-assigning @code{NodeLabelMode = "auto"} regenerates the
  ## defaults.
  ## @item NodeLabelColor
  ## RGB triplet or color name used to render the node labels.
  ## Default @code{[0 0 0]}.
  ## @item NodeFontSize
  ## Font size (points, positive scalar) for node labels.  Default
  ## @code{8}.
  ## @item NodeFontName
  ## Font family name for node labels.  Default @qcode{"Helvetica"}.
  ## @item NodeFontAngle
  ## @qcode{"normal"} (default) or @qcode{"italic"}.
  ## @item NodeFontWeight
  ## @qcode{"normal"} (default) or @qcode{"bold"}.
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
    NodeLabelColor = [0 0 0];
    NodeFontSize = 8;
    NodeFontName = "Helvetica";
    NodeFontAngle = "normal";
    NodeFontWeight = "normal";
  endproperties

  properties (Dependent)
    ## NodeLabel and NodeLabelMode are Dependent so the set methods can
    ## cooperate: setting NodeLabel flips NodeLabelMode to 'manual' and
    ## setting NodeLabelMode back to 'auto' regenerates NodeLabel from
    ## the cached graph.
    NodeLabel
    NodeLabelMode
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

    ## Column vector of text handles, one per rendered node label.
    node_label_handles_ = [];

    ## Cached underlying graph/digraph object.  Used for 'auto'
    ## NodeLabel regeneration after NodeLabelMode flips back to
    ## 'auto'.
    graph_ = [];

    ## Backing fields for the Dependent NodeLabel / NodeLabelMode
    ## properties.
    node_label_ = cell (0, 1);
    node_label_mode_ = "auto";

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
      ## first input, compute a layout (default @qcode{"auto"}: spectral
      ## @qcode{"subspace"} for graphs with fewer than 100 nodes,
      ## Fruchterman-Reingold @qcode{"force"} otherwise) and render nodes
      ## and edges on the current axes.  Trailing @var{name}/@var{value}
      ## pairs override layout and appearance properties; see
      ## @code{plot} for the full list.
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
      ## Cosmetic options recorded but applied after NumNodes is known.
      cosmetic_sets = {};
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
          case "dimension"
            layout_opts.Dimension = val;
          case {"nodecolor", "marker", "markersize", "nodelabel", ...
                "nodelabelmode", "nodelabelcolor", "nodefontsize", ...
                "nodefontname", "nodefontangle", "nodefontweight"}
            cosmetic_sets(end+1, 1:2) = {lower(name), val};
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

      ## Cache the underlying graph so NodeLabelMode='auto' can
      ## regenerate labels later, and seed NodeLabel with the auto
      ## default.  Mode remains 'auto' until overridden below.
      h.graph_ = G;
      h.node_label_ = __graph_plot_default_labels__ (G, N);
      h.node_label_mode_ = "auto";

      ## Apply user-supplied cosmetic overrides in the order they were
      ## given so the final state reflects any intentional reordering
      ## (e.g.  NodeLabel followed by NodeLabelMode='auto' leaves the
      ## auto-regenerated labels in place).
      for kk = 1:size (cosmetic_sets, 1)
        key = cosmetic_sets{kk, 1};
        val = cosmetic_sets{kk, 2};
        switch (key)
          case "nodecolor"
            h.NodeColor = val;
          case "marker"
            h.Marker = val;
          case "markersize"
            h.MarkerSize = val;
          case "nodelabel"
            h.NodeLabel = val;
          case "nodelabelmode"
            h.NodeLabelMode = val;
          case "nodelabelcolor"
            h.NodeLabelColor = val;
          case "nodefontsize"
            h.NodeFontSize = val;
          case "nodefontname"
            h.NodeFontName = val;
          case "nodefontangle"
            h.NodeFontAngle = val;
          case "nodefontweight"
            h.NodeFontWeight = val;
        endswitch
      endfor

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

        ## Render node labels as text objects slightly offset from each
        ## marker.  Labels are drawn even in 3-D (with zero Z offset).
        ## Rendering is best-effort: a failure does not invalidate the
        ## public property state.
        labels = h.NodeLabel;
        if (N > 0 && numel (labels) == N)
          th = zeros (N, 1);
          for kk = 1:N
            if (is_3d)
              th(kk) = text (h.axes_, h.XData(kk), h.YData(kk), ...
                             h.ZData(kk), labels{kk}, ...
                             "Color", h.NodeLabelColor, ...
                             "FontSize", h.NodeFontSize, ...
                             "FontName", h.NodeFontName, ...
                             "FontAngle", h.NodeFontAngle, ...
                             "FontWeight", h.NodeFontWeight);
            else
              th(kk) = text (h.axes_, h.XData(kk), h.YData(kk), ...
                             labels{kk}, ...
                             "Color", h.NodeLabelColor, ...
                             "FontSize", h.NodeFontSize, ...
                             "FontName", h.NodeFontName, ...
                             "FontAngle", h.NodeFontAngle, ...
                             "FontWeight", h.NodeFontWeight);
            endif
          endfor
          h.node_label_handles_ = th;
        endif

        if (! was_hold)
          hold (h.axes_, "off");
        endif
      catch err
        ## Rendering failed; keep data but leave graphics handles empty.
        h.axes_ = [];
        h.node_handle_ = [];
        h.edge_handles_ = [];
        h.node_label_handles_ = [];
      end_try_catch

    endfunction

    ## ------------ Dependent property accessors ------------

    function L = get.NodeLabel (h)
      L = h.node_label_;
    endfunction

    function h = set.NodeLabel (h, val)
      h.node_label_ = __graph_plot_validate_nodelabel__ (val, h.NumNodes);
      h.node_label_mode_ = "manual";
    endfunction

    function M = get.NodeLabelMode (h)
      M = h.node_label_mode_;
    endfunction

    function h = set.NodeLabelMode (h, val)
      if (! (ischar (val) && isrow (val)))
        error ("Octave:invalid-input-arg", ...
               "GraphPlot: NodeLabelMode must be 'auto' or 'manual'");
      endif
      v = lower (val);
      if (! any (strcmp (v, {"auto", "manual"})))
        error ("Octave:invalid-input-arg", ...
               "GraphPlot: NodeLabelMode must be 'auto' or 'manual'");
      endif
      if (strcmp (v, "auto") && ! isempty (h.graph_))
        ## Regenerate the auto labels from the cached graph.
        h.node_label_ = __graph_plot_default_labels__ (h.graph_, ...
                                                      h.NumNodes);
      endif
      h.node_label_mode_ = v;
    endfunction

    ## ------------ Validated setters for cosmetic properties ------------

    function h = set.NodeColor (h, val)
      h.NodeColor = __graph_plot_validate_colorspec__ (val, "NodeColor");
    endfunction

    function h = set.EdgeColor (h, val)
      h.EdgeColor = __graph_plot_validate_colorspec__ (val, "EdgeColor");
    endfunction

    function h = set.NodeLabelColor (h, val)
      h.NodeLabelColor = ...
        __graph_plot_validate_colorspec__ (val, "NodeLabelColor");
    endfunction

    function h = set.Marker (h, val)
      if (! (ischar (val) && isrow (val)))
        error ("Octave:invalid-input-arg", ...
               "GraphPlot: Marker must be a character vector");
      endif
      valid = {"+", "o", "*", ".", "x", "s", "square", "d", "diamond", ...
               "^", "v", ">", "<", "p", "pentagram", "h", "hexagram", ...
               "none"};
      if (! any (strcmp (val, valid)))
        error ("Octave:invalid-input-arg", ...
               "GraphPlot: Marker value '%s' is not supported", val);
      endif
      h.Marker = val;
    endfunction

    function h = set.MarkerSize (h, val)
      if (! (isnumeric (val) && isscalar (val) && isreal (val) ...
             && isfinite (val) && val > 0))
        error ("Octave:invalid-input-arg", ...
               "GraphPlot: MarkerSize must be a positive real scalar");
      endif
      h.MarkerSize = double (val);
    endfunction

    function h = set.NodeFontSize (h, val)
      if (! (isnumeric (val) && isscalar (val) && isreal (val) ...
             && isfinite (val) && val > 0))
        error ("Octave:invalid-input-arg", ...
               "GraphPlot: NodeFontSize must be a positive real scalar");
      endif
      h.NodeFontSize = double (val);
    endfunction

    function h = set.NodeFontName (h, val)
      if (! (ischar (val) && isrow (val)))
        error ("Octave:invalid-input-arg", ...
               "GraphPlot: NodeFontName must be a character vector");
      endif
      h.NodeFontName = val;
    endfunction

    function h = set.NodeFontAngle (h, val)
      if (! (ischar (val) && isrow (val)))
        error ("Octave:invalid-input-arg", ...
               "GraphPlot: NodeFontAngle must be 'normal' or 'italic'");
      endif
      v = lower (val);
      if (! any (strcmp (v, {"normal", "italic"})))
        error ("Octave:invalid-input-arg", ...
               "GraphPlot: NodeFontAngle must be 'normal' or 'italic'");
      endif
      h.NodeFontAngle = v;
    endfunction

    function h = set.NodeFontWeight (h, val)
      if (! (ischar (val) && isrow (val)))
        error ("Octave:invalid-input-arg", ...
               "GraphPlot: NodeFontWeight must be 'normal' or 'bold'");
      endif
      v = lower (val);
      if (! any (strcmp (v, {"normal", "bold"})))
        error ("Octave:invalid-input-arg", ...
               "GraphPlot: NodeFontWeight must be 'normal' or 'bold'");
      endif
      h.NodeFontWeight = v;
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

## -------- US-GP06 subspace / subspace3 layout via GraphPlot --------

## 'Layout','subspace' picks the 2-D spectral layout.
%!test
%! G = graph ([1 2 3 4], [2 3 4 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G, "Layout", "subspace");
%!   assert (isa (h, "GraphPlot"));
%!   assert (numel (h.XData), 4);
%!   assert (numel (h.YData), 4);
%!   assert (iscolumn (h.XData));
%!   assert (iscolumn (h.YData));
%!   assert (all (isfinite (h.XData)));
%!   assert (all (isfinite (h.YData)));
%!   assert (isempty (h.ZData));
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## 'Layout','subspace' matches direct helper call (no Dimension).
%!test
%! G = graph ([1 2 3 4], [2 3 4 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G, "Layout", "subspace");
%!   [Xh, Yh] = __graph_plot_subspace__ (G);
%!   assert (h.XData, Xh);
%!   assert (h.YData, Yh);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Subspace layout is deterministic across repeat calls (no RNG used).
%!test
%! G = graph ([1 2 3 4 5], [2 3 4 5 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   rand ("state", 7);
%!   h1 = GraphPlot (G, "Layout", "subspace");
%!   rand ("state", 77);
%!   h2 = GraphPlot (G, "Layout", "subspace");
%!   assert (h1.XData, h2.XData);
%!   assert (h1.YData, h2.YData);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## 'Dimension' option forwards to the subspace helper.
%!test
%! G = graph ([1 2 3 4 5], [2 3 4 5 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G, "Layout", "subspace", "Dimension", 3);
%!   [Xh, Yh] = __graph_plot_subspace__ (G, 3);
%!   assert (h.XData, Xh);
%!   assert (h.YData, Yh);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## 'Layout','subspace' name is case-insensitive.
%!test
%! G = graph ([1 2 3 4], [2 3 4 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h1 = GraphPlot (G, "Layout", "subspace");
%!   h2 = GraphPlot (G, "Layout", "SUBSPACE");
%!   h3 = GraphPlot (G, "Layout", "Subspace");
%!   assert (h1.XData, h2.XData);
%!   assert (h1.XData, h3.XData);
%!   assert (h1.YData, h2.YData);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## 'Layout','subspace3' populates ZData.
%!test
%! G = graph ([1 2 3 4 5], [2 3 4 5 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G, "Layout", "subspace3");
%!   assert (isa (h, "GraphPlot"));
%!   assert (numel (h.ZData), 5);
%!   assert (iscolumn (h.ZData));
%!   assert (any (abs (h.ZData) > 1e-6));
%!   assert (all (isfinite (h.ZData)));
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## 'Layout','subspace3' matches direct helper call.
%!test
%! G = graph ([1 2 3 4 5], [2 3 4 5 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G, "Layout", "subspace3");
%!   [Xh, Yh, Zh] = __graph_plot_subspace3__ (G);
%!   assert (h.XData, Xh);
%!   assert (h.YData, Yh);
%!   assert (h.ZData, Zh);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## 'Layout','subspace3' name is case-insensitive.
%!test
%! G = graph ([1 2 3 4 5], [2 3 4 5 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h1 = GraphPlot (G, "Layout", "subspace3");
%!   h2 = GraphPlot (G, "Layout", "SUBSPACE3");
%!   h3 = GraphPlot (G, "Layout", "Subspace3");
%!   assert (h1.XData, h2.XData);
%!   assert (h1.ZData, h3.ZData);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## 'Dimension' option forwards to subspace3 helper.
%!test
%! G = graph ([1 2 3 4 5], [2 3 4 5 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G, "Layout", "subspace3", "Dimension", 4);
%!   [Xh, Yh, Zh] = __graph_plot_subspace3__ (G, 4);
%!   assert (h.XData, Xh);
%!   assert (h.YData, Yh);
%!   assert (h.ZData, Zh);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Bad Dimension value surfaces as helper error.
%!error <at least 2> GraphPlot (graph ([1 2 3], [2 3 1]), ...
%!                               "Layout", "subspace", "Dimension", 1)
%!error <at least 3> GraphPlot (graph ([1 2 3], [2 3 1]), ...
%!                               "Layout", "subspace3", "Dimension", 2)

## Dimension option ignored under non-subspace layouts.
%!test
%! G = graph ([1 2 3 4], [2 3 4 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h1 = GraphPlot (G, "Layout", "circle", "Dimension", 2);
%!   h2 = GraphPlot (G, "Layout", "circle");
%!   assert (h1.XData, h2.XData);
%!   assert (h1.YData, h2.YData);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## 'auto' layout on a small graph now equals 'subspace' (US-GP06 put
## real spectral code in place of the circle placeholder).
%!test
%! G = graph ([1 2 3 4 5], [2 3 4 5 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h_auto = GraphPlot (G, "Layout", "auto");
%!   h_sub  = GraphPlot (G, "Layout", "subspace");
%!   assert (h_auto.XData, h_sub.XData);
%!   assert (h_auto.YData, h_sub.YData);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## -------- US-GP07 node cosmetic properties --------

## Default cosmetic properties on an empty GraphPlot.
%!test
%! h = GraphPlot ();
%! assert (h.NodeColor, [0 0.4470 0.7410], 1e-12);
%! assert (h.Marker, "o");
%! assert (h.MarkerSize, 4);
%! assert (iscell (h.NodeLabel));
%! assert (isempty (h.NodeLabel));
%! assert (h.NodeFontSize, 8);
%! assert (h.NodeFontName, "Helvetica");
%! assert (h.NodeFontAngle, "normal");
%! assert (h.NodeFontWeight, "normal");
%! assert (h.NodeLabelMode, "auto");
%! assert (h.NodeLabelColor, [0 0 0]);

## NodeLabel defaults to "1","2",... on an unnamed graph.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   assert (iscell (h.NodeLabel));
%!   assert (numel (h.NodeLabel), 3);
%!   assert (h.NodeLabel, {"1"; "2"; "3"});
%!   assert (h.NodeLabelMode, "auto");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## NodeLabel defaults to G.Nodes.Name on a named graph.
%!test
%! G = digraph ([1 2 3], [2 3 1], [], {"alpha", "beta", "gamma"});
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   assert (iscell (h.NodeLabel));
%!   assert (h.NodeLabel, {"alpha"; "beta"; "gamma"});
%!   assert (h.NodeLabelMode, "auto");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## NodeLabel defaults are column cellstr even for unnamed graph.
%!test
%! G = graph (4);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   assert (h.NodeLabel, {"1"; "2"; "3"; "4"});
%!   assert (iscolumn (h.NodeLabel));
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## NodeColor can be set via name-value in the constructor.
%!test
%! G = digraph ([1 2], [2 3]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G, "NodeColor", [1 0 0]);
%!   assert (h.NodeColor, [1 0 0]);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## NodeColor accepts a color-name string.
%!test
%! G = digraph ([1 2], [2 3]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G, "NodeColor", "red");
%!   assert (h.NodeColor, [1 0 0]);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## NodeColor can be assigned directly (handle semantics).
%!test
%! h = GraphPlot ();
%! h.NodeColor = [0.2 0.4 0.6];
%! assert (h.NodeColor, [0.2 0.4 0.6], 1e-12);
%! h.NodeColor = "g";
%! assert (h.NodeColor, [0 1 0]);

## Invalid NodeColor values are rejected.
%!error <NodeColor> ...
%! h = GraphPlot (); h.NodeColor = [2 0 0];
%!error <NodeColor> ...
%! h = GraphPlot (); h.NodeColor = [0 0];
%!error <NodeColor> ...
%! h = GraphPlot (); h.NodeColor = "notacolor";
%!error <NodeColor> ...
%! GraphPlot (digraph (2), "NodeColor", [2 2 2])

## Marker can be set and read back.
%!test
%! G = digraph ([1 2], [2 3]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G, "Marker", "square");
%!   assert (h.Marker, "square");
%!   h.Marker = "^";
%!   assert (h.Marker, "^");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Marker validation.
%!error <Marker> h = GraphPlot (); h.Marker = 1;
%!error <Marker> h = GraphPlot (); h.Marker = "bogus";

## MarkerSize default, set, validate.
%!test
%! G = digraph ([1 2], [2 3]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G, "MarkerSize", 12);
%!   assert (h.MarkerSize, 12);
%!   h.MarkerSize = 3.5;
%!   assert (h.MarkerSize, 3.5);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## MarkerSize must be positive numeric scalar.
%!error <MarkerSize> h = GraphPlot (); h.MarkerSize = -1;
%!error <MarkerSize> h = GraphPlot (); h.MarkerSize = 0;
%!error <MarkerSize> h = GraphPlot (); h.MarkerSize = [1 2];
%!error <MarkerSize> h = GraphPlot (); h.MarkerSize = "big";

## NodeLabel can be set to a cellstr (column or row); stored as column.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G, "NodeLabel", {"A", "B", "C"});
%!   assert (h.NodeLabel, {"A"; "B"; "C"});
%!   assert (h.NodeLabelMode, "manual");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Assigning NodeLabel after construction flips NodeLabelMode to 'manual'.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   assert (h.NodeLabelMode, "auto");
%!   h.NodeLabel = {"x"; "y"; "z"};
%!   assert (h.NodeLabel, {"x"; "y"; "z"});
%!   assert (h.NodeLabelMode, "manual");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Numeric NodeLabel vector is converted to cellstr.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G, "NodeLabel", [10 20 30]);
%!   assert (iscell (h.NodeLabel));
%!   assert (h.NodeLabel, {"10"; "20"; "30"});
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Length-mismatched NodeLabel is rejected.
%!error <NodeLabel> ...
%!   GraphPlot (digraph ([1 2 3], [2 3 1]), "NodeLabel", {"a", "b"})
%!error <NodeLabel> ...
%!   h = GraphPlot (digraph ([1 2], [2 3])); h.NodeLabel = {"p"};

## NodeFontSize default, set, validate.
%!test
%! G = digraph ([1 2], [2 3]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G, "NodeFontSize", 14);
%!   assert (h.NodeFontSize, 14);
%!   h.NodeFontSize = 10;
%!   assert (h.NodeFontSize, 10);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!error <NodeFontSize> h = GraphPlot (); h.NodeFontSize = 0;
%!error <NodeFontSize> h = GraphPlot (); h.NodeFontSize = -1;
%!error <NodeFontSize> h = GraphPlot (); h.NodeFontSize = [1 2];
%!error <NodeFontSize> h = GraphPlot (); h.NodeFontSize = "big";

## NodeFontName default, set, validate.
%!test
%! G = digraph ([1 2], [2 3]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G, "NodeFontName", "Arial");
%!   assert (h.NodeFontName, "Arial");
%!   h.NodeFontName = "Times";
%!   assert (h.NodeFontName, "Times");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!error <NodeFontName> h = GraphPlot (); h.NodeFontName = 1;
%!error <NodeFontName> h = GraphPlot (); h.NodeFontName = {"Arial"};

## NodeFontAngle default 'normal', accepts 'italic', rejects other.
%!test
%! G = digraph ([1 2], [2 3]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G, "NodeFontAngle", "italic");
%!   assert (h.NodeFontAngle, "italic");
%!   h.NodeFontAngle = "normal";
%!   assert (h.NodeFontAngle, "normal");
%!   h.NodeFontAngle = "ITALIC";
%!   assert (h.NodeFontAngle, "italic");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!error <NodeFontAngle> h = GraphPlot (); h.NodeFontAngle = "bold";
%!error <NodeFontAngle> h = GraphPlot (); h.NodeFontAngle = 1;

## NodeFontWeight default 'normal', accepts 'bold', rejects other.
%!test
%! G = digraph ([1 2], [2 3]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G, "NodeFontWeight", "bold");
%!   assert (h.NodeFontWeight, "bold");
%!   h.NodeFontWeight = "NORMAL";
%!   assert (h.NodeFontWeight, "normal");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!error <NodeFontWeight> h = GraphPlot (); h.NodeFontWeight = "italic";
%!error <NodeFontWeight> h = GraphPlot (); h.NodeFontWeight = 1;

## NodeLabelMode default 'auto', accepts 'manual', rejects other.
%!test
%! h = GraphPlot ();
%! assert (h.NodeLabelMode, "auto");
%! h.NodeLabelMode = "manual";
%! assert (h.NodeLabelMode, "manual");
%! h.NodeLabelMode = "AUTO";
%! assert (h.NodeLabelMode, "auto");

%!error <NodeLabelMode> h = GraphPlot (); h.NodeLabelMode = "bogus";
%!error <NodeLabelMode> h = GraphPlot (); h.NodeLabelMode = 1;

## NodeLabelMode='auto' after a manual label reset regenerates defaults
## from the underlying graph.
%!test
%! G = digraph ([1 2 3], [2 3 1], [], {"a", "b", "c"});
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   h.NodeLabel = {"x"; "y"; "z"};    # now manual
%!   assert (h.NodeLabelMode, "manual");
%!   h.NodeLabelMode = "auto";
%!   assert (h.NodeLabel, {"a"; "b"; "c"});
%!   assert (h.NodeLabelMode, "auto");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## NodeLabelColor default, set via name-value, assign directly, validate.
%!test
%! G = digraph ([1 2], [2 3]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G, "NodeLabelColor", [0.5 0.5 0.5]);
%!   assert (h.NodeLabelColor, [0.5 0.5 0.5], 1e-12);
%!   h.NodeLabelColor = "b";
%!   assert (h.NodeLabelColor, [0 0 1]);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!error <NodeLabelColor> h = GraphPlot (); h.NodeLabelColor = [2 0 0];
%!error <NodeLabelColor> h = GraphPlot (); h.NodeLabelColor = "nocolor";

## All cosmetic properties forwarded via constructor name-value in one call.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G, ...
%!                  "NodeColor", [1 0 0], ...
%!                  "Marker", "s", ...
%!                  "MarkerSize", 8, ...
%!                  "NodeLabel", {"a", "b", "c"}, ...
%!                  "NodeFontSize", 12, ...
%!                  "NodeFontName", "Arial", ...
%!                  "NodeFontAngle", "italic", ...
%!                  "NodeFontWeight", "bold", ...
%!                  "NodeLabelColor", [0.2 0.2 0.2]);
%!   assert (h.NodeColor, [1 0 0]);
%!   assert (h.Marker, "s");
%!   assert (h.MarkerSize, 8);
%!   assert (h.NodeLabel, {"a"; "b"; "c"});
%!   assert (h.NodeFontSize, 12);
%!   assert (h.NodeFontName, "Arial");
%!   assert (h.NodeFontAngle, "italic");
%!   assert (h.NodeFontWeight, "bold");
%!   assert (h.NodeLabelColor, [0.2 0.2 0.2], 1e-12);
%!   assert (h.NodeLabelMode, "manual");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Option names are case-insensitive on input (handled by lower()).
%!test
%! G = digraph ([1 2], [2 3]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G, "nodecolor", [0 1 0], "MARKER", "x", ...
%!                  "NodeFontSize", 9);
%!   assert (h.NodeColor, [0 1 0]);
%!   assert (h.Marker, "x");
%!   assert (h.NodeFontSize, 9);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Handle-class semantics: cosmetic property assignment is visible in
## aliased handles.
%!test
%! h1 = GraphPlot ();
%! h2 = h1;
%! h2.NodeFontSize = 22;
%! assert (h1.NodeFontSize, 22);
%! h1.Marker = "d";
%! assert (h2.Marker, "d");
