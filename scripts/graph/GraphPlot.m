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
  ## Line style used to render the edges.  Default @qcode{"-"}.  One of
  ## @qcode{"-"}, @qcode{"--"}, @qcode{":"}, @qcode{"-."}, or
  ## @qcode{"none"}.
  ## @item LineWidth
  ## Line width used to render the edges.  Default @code{0.5}.
  ## @item ArrowSize
  ## Arrow size used for @code{digraph} edge arrowheads, in points.
  ## Default @code{7}.  Ignored for undirected graphs.
  ## @item ArrowPosition
  ## Normalised location along each edge at which the arrowhead is
  ## drawn, strictly between 0 and 1.  Default @code{0.5}.
  ## @item EdgeAlpha
  ## Transparency of rendered edges, a scalar in @code{[0, 1]}.
  ## Default @code{0.5}.
  ## @item EdgeLabel
  ## Column cell array of strings of length @code{numedges (G)}, or an
  ## empty cell for no labels.  When @code{EdgeLabelMode} is
  ## @qcode{"auto"}, the default for an unweighted graph is empty and
  ## for a weighted graph is the column cellstr obtained by applying
  ## @code{num2str} to each entry of @code{G.Edges.Weight}.
  ## @item EdgeLabelMode
  ## @qcode{"auto"} (default) or @qcode{"manual"}.  Assigning to
  ## @code{EdgeLabel} flips @code{EdgeLabelMode} to @qcode{"manual"};
  ## re-assigning @code{EdgeLabelMode = "auto"} regenerates the
  ## defaults from the cached graph.
  ## @item EdgeFontSize
  ## Font size (points, positive scalar) for edge labels.  Default
  ## @code{8}.
  ## @item EdgeFontName
  ## Font family name for edge labels.  Default @qcode{"Helvetica"}.
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
    ArrowSize = 7;
    ArrowPosition = 0.5;
    EdgeAlpha = 0.5;
    EdgeFontSize = 8;
    EdgeFontName = "Helvetica";
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
    ## EdgeLabel and EdgeLabelMode mirror the node-label pattern: the
    ## manual setter for EdgeLabel flips EdgeLabelMode to 'manual' and
    ## EdgeLabelMode='auto' regenerates the default labels (weights
    ## for a weighted graph, empty otherwise) from the cached graph.
    EdgeLabel
    EdgeLabelMode
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

    ## Backing fields for the Dependent EdgeLabel / EdgeLabelMode
    ## properties.
    edge_label_ = cell (0, 1);
    edge_label_mode_ = "auto";

    ## Column vector of text handles, one per rendered edge label.
    edge_label_handles_ = [];

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
                "nodefontname", "nodefontangle", "nodefontweight", ...
                "edgecolor", "linewidth", "linestyle", "arrowsize", ...
                "arrowposition", "edgealpha", "edgelabel", ...
                "edgelabelmode", "edgefontsize", "edgefontname"}
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

      ## Seed EdgeLabel with its auto default (weight strings when
      ## weighted, empty otherwise).
      h.edge_label_ = __graph_plot_default_edge_labels__ (G, h.NumEdges);
      h.edge_label_mode_ = "auto";

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
          case "edgecolor"
            h.EdgeColor = val;
          case "linewidth"
            h.LineWidth = val;
          case "linestyle"
            h.LineStyle = val;
          case "arrowsize"
            h.ArrowSize = val;
          case "arrowposition"
            h.ArrowPosition = val;
          case "edgealpha"
            h.EdgeAlpha = val;
          case "edgelabel"
            h.EdgeLabel = val;
          case "edgelabelmode"
            h.EdgeLabelMode = val;
          case "edgefontsize"
            h.EdgeFontSize = val;
          case "edgefontname"
            h.EdgeFontName = val;
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

        ## Render edge labels at the along-edge ArrowPosition (the
        ## same point used for the arrowhead of a digraph).  Rendering
        ## is best-effort: a failure does not invalidate public
        ## property state.
        elabels = h.edge_label_;
        M = h.NumEdges;
        if (M > 0 && numel (elabels) == M)
          eth = zeros (M, 1);
          for kk = 1:M
            s = h.edge_src_(kk);
            t = h.edge_dst_(kk);
            ap = h.ArrowPosition;
            xm = h.XData(s) + ap * (h.XData(t) - h.XData(s));
            ym = h.YData(s) + ap * (h.YData(t) - h.YData(s));
            if (is_3d)
              zm = h.ZData(s) + ap * (h.ZData(t) - h.ZData(s));
              eth(kk) = text (h.axes_, xm, ym, zm, elabels{kk}, ...
                              "Color", h.EdgeColor, ...
                              "FontSize", h.EdgeFontSize, ...
                              "FontName", h.EdgeFontName);
            else
              eth(kk) = text (h.axes_, xm, ym, elabels{kk}, ...
                              "Color", h.EdgeColor, ...
                              "FontSize", h.EdgeFontSize, ...
                              "FontName", h.EdgeFontName);
            endif
          endfor
          h.edge_label_handles_ = eth;
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
        h.edge_label_handles_ = [];
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

    function L = get.EdgeLabel (h)
      L = h.edge_label_;
    endfunction

    function h = set.EdgeLabel (h, val)
      h.edge_label_ = __graph_plot_validate_edgelabel__ (val, h.NumEdges);
      h.edge_label_mode_ = "manual";
    endfunction

    function M = get.EdgeLabelMode (h)
      M = h.edge_label_mode_;
    endfunction

    function h = set.EdgeLabelMode (h, val)
      if (! (ischar (val) && isrow (val)))
        error ("Octave:invalid-input-arg", ...
               "GraphPlot: EdgeLabelMode must be 'auto' or 'manual'");
      endif
      v = lower (val);
      if (! any (strcmp (v, {"auto", "manual"})))
        error ("Octave:invalid-input-arg", ...
               "GraphPlot: EdgeLabelMode must be 'auto' or 'manual'");
      endif
      if (strcmp (v, "auto") && ! isempty (h.graph_))
        ## Regenerate the auto labels from the cached graph.
        h.edge_label_ = __graph_plot_default_edge_labels__ (h.graph_, ...
                                                            h.NumEdges);
      endif
      h.edge_label_mode_ = v;
    endfunction

    ## ------------ Methods: highlight, ... ------------

    function highlight (h, varargin)

      ## -*- texinfo -*-
      ## @deftypefn  {} {} highlight (@var{h}, @var{nodes})
      ## @deftypefnx {} {} highlight (@var{h}, @var{nodes}, @var{name}, @var{value}, @dots{})
      ## @deftypefnx {} {} highlight (@var{h}, @var{s}, @var{t})
      ## @deftypefnx {} {} highlight (@var{h}, @var{s}, @var{t}, @var{name}, @var{value}, @dots{})
      ## @deftypefnx {} {} highlight (@var{h}, "Edges", @var{idx})
      ## @deftypefnx {} {} highlight (@var{h}, "Edges", @var{idx}, @var{name}, @var{value}, @dots{})
      ## Highlight the specified nodes or edges of a @code{GraphPlot}.
      ##
      ## In the node form, @var{nodes} is a numeric vector of node indices,
      ## a single node name (character row vector), a cell array of node
      ## names, or an empty array (silent no-op).  By default the selected
      ## nodes' color is set to red (@code{[1 0 0]}).
      ##
      ## In the edge form, @var{s} and @var{t} are equal-length vectors of
      ## node indices, cellstrs of node names, or single character-row
      ## vectors.  The edge matching each @code{(@var{s}(i),@var{t}(i))}
      ## pair is highlighted.  By default the selected edges' color is
      ## set to red.  For undirected graphs, @code{(@var{s}, @var{t})} and
      ## @code{(@var{t}, @var{s})} refer to the same edge.
      ##
      ## In the edge-index form, the literal keyword @qcode{"Edges"}
      ## followed by a numeric vector @var{idx} selects those edges by
      ## their 1-based indices into @code{@var{h}.Edges} (the same order
      ## as @code{G.Edges.EndNodes}).  By default the selected edges'
      ## color is set to red.
      ##
      ## Trailing @var{name}/@var{value} pairs override the default.
      ## Recognised options (case-insensitive):
      ##
      ## @table @code
      ## @item NodeColor
      ## RGB triplet in @code{[0, 1]} or a MATLAB color name.  Node form.
      ## @item Marker
      ## Marker character such as @qcode{"o"}, @qcode{"s"}, @qcode{"d"},
      ## @qcode{"^"}, etc.  Node form.
      ## @item MarkerSize
      ## Positive real scalar.  Node form.
      ## @item EdgeColor
      ## RGB triplet in @code{[0, 1]} or a MATLAB color name.  Edge form.
      ## @item LineWidth
      ## Positive real scalar.  Edge form.
      ## @item LineStyle
      ## One of @qcode{"-"}, @qcode{"--"}, @qcode{":"}, @qcode{"-."},
      ## @qcode{"none"}.  Edge form.
      ## @end table
      ##
      ## @code{highlight} expands the corresponding scalar properties to
      ## per-node or per-edge form as needed.  Nodes/edges not in the
      ## selection retain their current cosmetic values.
      ##
      ## @seealso{GraphPlot, plot, graph, digraph}
      ## @end deftypefn

      if (nargin < 2)
        print_usage ();
      endif

      if (isempty (h.graph_))
        error ("Octave:invalid-input-arg", ...
               "GraphPlot: highlight requires a rendered graph");
      endif

      ## ---------------- Dispatch: 'Edges' index form -----------------
      ##
      ## highlight (h, 'Edges', idx[, name, value, ...]) selects edges by
      ## 1-based index into h.graph_.Edges.EndNodes (the same row order
      ## as G.Edges and the internal edge list).  This is distinct from
      ## the (s, t) form (which resolves node pairs to edges) and from
      ## the node form.
      if (ischar (varargin{1}) && isrow (varargin{1}) ...
          && strcmpi (varargin{1}, "edges"))

        if (numel (varargin) < 2)
          error ("Octave:invalid-input-arg", ...
                 "GraphPlot: highlight: 'Edges' form requires an idx vector");
        endif

        idx_arg = varargin{2};
        rest = varargin(3:end);

        ## Empty idx -> silent no-op; preserve scalar cosmetic props.
        if (isempty (idx_arg))
          return;
        endif

        if (! isnumeric (idx_arg) || ! isreal (idx_arg))
          error ("Octave:invalid-input-arg", ...
                 "GraphPlot: highlight: 'Edges' idx must be a numeric vector");
        endif

        if (! isvector (idx_arg))
          error ("Octave:invalid-input-arg", ...
                 "GraphPlot: highlight: 'Edges' idx must be a vector");
        endif

        if (any (! isfinite (idx_arg(:))))
          error ("Octave:invalid-input-arg", ...
                 "GraphPlot: highlight: 'Edges' idx entries must be finite");
        endif

        if (any (idx_arg(:) <= 0))
          error ("Octave:invalid-input-arg", ...
                 "GraphPlot: highlight: 'Edges' idx entries must be positive");
        endif

        if (any (idx_arg(:) != fix (idx_arg(:))))
          error ("Octave:invalid-input-arg", ...
                 "GraphPlot: highlight: 'Edges' idx entries must be integer-valued");
        endif

        M = h.NumEdges;
        if (any (idx_arg(:) > M))
          error ("Octave:invalid-input-arg", ...
                 "GraphPlot: highlight: 'Edges' idx out of range (1..%d)", M);
        endif

        edge_idx = double (idx_arg(:));

        ## Parse trailing name-value overrides (edge cosmetics only).
        if (mod (numel (rest), 2) != 0)
          error ("Octave:invalid-input-arg", ...
                 "GraphPlot: highlight: name-value options must come in pairs");
        endif

        edge_color = [1 0 0];         # default red
        line_width = [];              # [] = do not touch LineWidth
        line_style = [];              # [] = do not touch LineStyle
        valid_linestyles = {"-", "--", ":", "-.", "none"};

        for ii = 1:2:numel (rest)
          name = rest{ii};
          if (! (ischar (name) && isrow (name)))
            error ("Octave:invalid-input-arg", ...
                   "GraphPlot: highlight: option names must be character vectors");
          endif
          val = rest{ii + 1};
          switch (lower (name))
            case "edgecolor"
              edge_color = __graph_plot_validate_colorspec__ (val, "EdgeColor");
            case "linewidth"
              if (! (isnumeric (val) && isscalar (val) && isreal (val) ...
                     && isfinite (val) && val > 0))
                error ("Octave:invalid-input-arg", ...
                       ["GraphPlot: highlight: LineWidth must be a ", ...
                        "positive real scalar"]);
              endif
              line_width = double (val);
            case "linestyle"
              if (! (ischar (val) && isrow (val)))
                error ("Octave:invalid-input-arg", ...
                       "GraphPlot: highlight: LineStyle must be a character vector");
              endif
              if (! any (strcmp (val, valid_linestyles)))
                error ("Octave:invalid-input-arg", ...
                       ["GraphPlot: highlight: LineStyle value '%s' ", ...
                        "is not supported"], val);
              endif
              line_style = val;
            otherwise
              error ("Octave:invalid-input-arg", ...
                     "GraphPlot: highlight: unknown option '%s'", name);
          endswitch
        endfor

        ## Expand EdgeColor to Mx3 and apply highlight color at edge_idx.
        ec = h.EdgeColor;
        if (size (ec, 1) == 1)
          ec = repmat (ec, M, 1);
        endif
        ec(edge_idx, :) = repmat (edge_color, numel (edge_idx), 1);
        h.EdgeColor = ec;

        if (! isempty (line_width))
          lw = h.LineWidth;
          if (isscalar (lw))
            lw = repmat (lw, M, 1);
          else
            lw = lw(:);
          endif
          lw(edge_idx) = line_width;
          h.LineWidth = lw;
        endif

        if (! isempty (line_style))
          if (iscell (h.LineStyle))
            ls = h.LineStyle(:);
          else
            ls = repmat ({h.LineStyle}, M, 1);
          endif
          ls(edge_idx) = {line_style};
          h.LineStyle = ls;
        endif

        return;
      endif

      ## ---------------- Dispatch: node form vs. edge form ---------
      ##
      ## Argument shapes supported:
      ##   highlight (h, nodes)                     -> node form
      ##   highlight (h, nodes, name, value, ...)   -> node form
      ##   highlight (h, s, t)                      -> edge form
      ##   highlight (h, s, t, name, value, ...)    -> edge form
      ##
      ## We decide edge form vs. node form by looking at varargin{2}
      ## (i.e. the third positional argument after h): if it exists and
      ## is *not* a char row matching a known option name, we are in
      ## edge form (varargin{1}=s, varargin{2}=t).  Otherwise we are in
      ## node form (varargin{1}=nodes, varargin{2..}=name-value pairs).
      node_opts = {"nodecolor", "marker", "markersize"};
      edge_opts = {"edgecolor", "linewidth", "linestyle"};
      all_opts = [node_opts, edge_opts];

      is_edge_form = false;
      if (numel (varargin) >= 2)
        a3 = varargin{2};
        if (! (ischar (a3) && isrow (a3) ...
               && any (strcmp (lower (a3), all_opts))))
          is_edge_form = true;
        endif
      endif

      if (is_edge_form)
        ## ---------------- Edge form: highlight (h, s, t, ...) --------
        s_arg = varargin{1};
        t_arg = varargin{2};
        rest = varargin(3:end);

        ## Empty endpoints -> silent no-op, regardless of name-value
        ## pairs.  This matches the node-form "highlight (h, [])"
        ## convention.
        if (isempty (s_arg) && isempty (t_arg))
          return;
        endif

        ## Resolve (s, t) to edge indices via the shared helper.  It
        ## validates types, length-match, numeric-range, and returns
        ## 0 for name-not-found (which we escalate into a user error).
        try
          edge_idx = __findedge_impl__ (h.graph_, 1, s_arg, t_arg);
        catch err
          ## Re-raise findedge errors under our own identifier so error
          ## regexes in BIST tests can target the wording.  The
          ## findedge error messages already describe the problem
          ## ("invalid node index in s ...", "same length", etc.).
          msg = err.message;
          ## Translate "S and T must have the same length" to our
          ## standardised wording.
          if (! isempty (strfind (msg, "same length")))
            error ("Octave:invalid-input-arg", ...
                   "GraphPlot: highlight: S and T must have the same length");
          endif
          if (! isempty (strfind (msg, "invalid node")))
            error ("Octave:invalid-input-arg", ...
                   "GraphPlot: highlight: invalid node index in S or T");
          endif
          rethrow (err);
        end_try_catch

        ## For char/cellstr endpoints, findedge returns 0 for misses;
        ## escalate the first miss into a clear error.  Distinguish
        ## "node name not found" from "edge not present" by checking
        ## whether the offending endpoint was a string not in the node
        ## names.
        miss = find (edge_idx == 0, 1);
        if (! isempty (miss))
          names = h.graph_.Nodes.Name;
          pick = miss;
          ## Extract the offending endpoint values from s_arg and t_arg.
          sname = "";
          tname = "";
          if (ischar (s_arg) && isrow (s_arg) && pick == 1)
            sname = s_arg;
          elseif (iscell (s_arg) && pick >= 1 && pick <= numel (s_arg))
            sname = s_arg{pick};
          endif
          if (ischar (t_arg) && isrow (t_arg) && pick == 1)
            tname = t_arg;
          elseif (iscell (t_arg) && pick >= 1 && pick <= numel (t_arg))
            tname = t_arg{pick};
          endif
          if (! isempty (sname) && (isempty (names) ...
              || ! any (strcmp (names, sname))))
            error ("Octave:invalid-input-arg", ...
                   "GraphPlot: highlight: node name '%s' not found", ...
                   sname);
          endif
          if (! isempty (tname) && (isempty (names) ...
              || ! any (strcmp (names, tname))))
            error ("Octave:invalid-input-arg", ...
                   "GraphPlot: highlight: node name '%s' not found", ...
                   tname);
          endif
          error ("Octave:invalid-input-arg", ...
                 "GraphPlot: highlight: no edge connects S(%d) and T(%d)", ...
                 miss, miss);
        endif
        edge_idx = edge_idx(:);
        if (isempty (edge_idx))
          return;
        endif

        ## Parse trailing name-value overrides.
        if (mod (numel (rest), 2) != 0)
          error ("Octave:invalid-input-arg", ...
                 "GraphPlot: highlight: name-value options must come in pairs");
        endif

        edge_color = [1 0 0];         # default red
        line_width = [];              # [] = do not touch LineWidth
        line_style = [];              # [] = do not touch LineStyle
        valid_linestyles = {"-", "--", ":", "-.", "none"};

        for ii = 1:2:numel (rest)
          name = rest{ii};
          if (! (ischar (name) && isrow (name)))
            error ("Octave:invalid-input-arg", ...
                   "GraphPlot: highlight: option names must be character vectors");
          endif
          val = rest{ii + 1};
          switch (lower (name))
            case "edgecolor"
              edge_color = __graph_plot_validate_colorspec__ (val, "EdgeColor");
            case "linewidth"
              if (! (isnumeric (val) && isscalar (val) && isreal (val) ...
                     && isfinite (val) && val > 0))
                error ("Octave:invalid-input-arg", ...
                       ["GraphPlot: highlight: LineWidth must be a ", ...
                        "positive real scalar"]);
              endif
              line_width = double (val);
            case "linestyle"
              if (! (ischar (val) && isrow (val)))
                error ("Octave:invalid-input-arg", ...
                       "GraphPlot: highlight: LineStyle must be a character vector");
              endif
              if (! any (strcmp (val, valid_linestyles)))
                error ("Octave:invalid-input-arg", ...
                       ["GraphPlot: highlight: LineStyle value '%s' ", ...
                        "is not supported"], val);
              endif
              line_style = val;
            otherwise
              error ("Octave:invalid-input-arg", ...
                     "GraphPlot: highlight: unknown option '%s'", name);
          endswitch
        endfor

        M = h.NumEdges;

        ## Expand EdgeColor to Mx3 and apply highlight color at edge_idx.
        ec = h.EdgeColor;
        if (size (ec, 1) == 1)
          ec = repmat (ec, M, 1);
        endif
        ec(edge_idx, :) = repmat (edge_color, numel (edge_idx), 1);
        h.EdgeColor = ec;

        ## Expand LineWidth (if override supplied) and apply.
        if (! isempty (line_width))
          lw = h.LineWidth;
          if (isscalar (lw))
            lw = repmat (lw, M, 1);
          else
            lw = lw(:);
          endif
          lw(edge_idx) = line_width;
          h.LineWidth = lw;
        endif

        ## Expand LineStyle (if override supplied) and apply.
        if (! isempty (line_style))
          if (iscell (h.LineStyle))
            ls = h.LineStyle(:);
          else
            ls = repmat ({h.LineStyle}, M, 1);
          endif
          ls(edge_idx) = {line_style};
          h.LineStyle = ls;
        endif

        return;
      endif

      ## ---------------- Node form: highlight (h, nodes, ...) --------
      nodes = varargin{1};
      rest = varargin(2:end);

      ## Resolve node indices (numeric / char / cellstr).  The helper
      ## returns a column vector of valid 1-based indices or [].
      idx = __resolve_node_list__ (h.graph_, nodes, "highlight");
      if (isempty (idx))
        return;
      endif
      idx = idx(:);

      ## Parse trailing name-value overrides.
      if (mod (numel (rest), 2) != 0)
        error ("Octave:invalid-input-arg", ...
               "GraphPlot: highlight: name-value options must come in pairs");
      endif

      node_color = [1 0 0];         # default red
      marker = [];                  # [] = don't touch Marker
      marker_size = [];             # [] = don't touch MarkerSize
      valid_markers = {"+", "o", "*", ".", "x", "s", "square", "d", ...
                       "diamond", "^", "v", ">", "<", "p", "pentagram", ...
                       "h", "hexagram", "none"};

      for ii = 1:2:numel (rest)
        name = rest{ii};
        if (! (ischar (name) && isrow (name)))
          error ("Octave:invalid-input-arg", ...
                 "GraphPlot: highlight: option names must be character vectors");
        endif
        val = rest{ii + 1};
        switch (lower (name))
          case "nodecolor"
            node_color = __graph_plot_validate_colorspec__ (val, "NodeColor");
          case "marker"
            if (! (ischar (val) && isrow (val)))
              error ("Octave:invalid-input-arg", ...
                     "GraphPlot: highlight: Marker must be a character vector");
            endif
            if (! any (strcmp (val, valid_markers)))
              error ("Octave:invalid-input-arg", ...
                     "GraphPlot: highlight: Marker value '%s' is not supported", ...
                     val);
            endif
            marker = val;
          case "markersize"
            if (! (isnumeric (val) && isscalar (val) && isreal (val) ...
                   && isfinite (val) && val > 0))
              error ("Octave:invalid-input-arg", ...
                     "GraphPlot: highlight: MarkerSize must be a positive real scalar");
            endif
            marker_size = double (val);
          otherwise
            error ("Octave:invalid-input-arg", ...
                   "GraphPlot: highlight: unknown option '%s'", name);
        endswitch
      endfor

      N = h.NumNodes;

      ## Expand NodeColor to Nx3 and apply highlight color at idx.
      nc = h.NodeColor;
      if (size (nc, 1) == 1)
        nc = repmat (nc, N, 1);
      endif
      nc(idx, :) = repmat (node_color, numel (idx), 1);
      h.NodeColor = nc;

      ## Expand Marker (if override supplied) and apply.
      if (! isempty (marker))
        if (iscell (h.Marker))
          mk = h.Marker(:);
        else
          mk = repmat ({h.Marker}, N, 1);
        endif
        mk(idx) = {marker};
        h.Marker = mk;
      endif

      ## Expand MarkerSize (if override supplied) and apply.
      if (! isempty (marker_size))
        ms = h.MarkerSize;
        if (isscalar (ms))
          ms = repmat (ms, N, 1);
        else
          ms = ms(:);
        endif
        ms(idx) = marker_size;
        h.MarkerSize = ms;
      endif

    endfunction

    ## ------------ Validated setters for cosmetic properties ------------

    function h = set.NodeColor (h, val)
      ## Accept a single RGB triplet / color name, or an Nx3 matrix with
      ## one row per node (used by highlight() for per-node coloring).
      if (isnumeric (val) && ismatrix (val) && ndims (val) == 2 ...
          && size (val, 2) == 3 && size (val, 1) > 1)
        N = h.NumNodes;
        if (size (val, 1) != N)
          error ("Octave:invalid-input-arg", ...
                 "GraphPlot: NodeColor matrix must have %d rows", N);
        endif
        if (! (isreal (val) && all (isfinite (val(:))) ...
               && all (val(:) >= 0) && all (val(:) <= 1)))
          error ("Octave:invalid-input-arg", ...
                 "GraphPlot: NodeColor entries must be in [0, 1]");
        endif
        h.NodeColor = double (val);
      else
        h.NodeColor = __graph_plot_validate_colorspec__ (val, "NodeColor");
      endif
    endfunction

    function h = set.EdgeColor (h, val)
      ## Accept a single RGB triplet / color name, or an Mx3 matrix with
      ## one row per edge (used by highlight (h, s, t, ...)) for per-edge
      ## coloring.
      if (isnumeric (val) && ismatrix (val) && ndims (val) == 2 ...
          && size (val, 2) == 3 && size (val, 1) > 1)
        M = h.NumEdges;
        if (size (val, 1) != M)
          error ("Octave:invalid-input-arg", ...
                 "GraphPlot: EdgeColor matrix must have %d rows", M);
        endif
        if (! (isreal (val) && all (isfinite (val(:))) ...
               && all (val(:) >= 0) && all (val(:) <= 1)))
          error ("Octave:invalid-input-arg", ...
                 "GraphPlot: EdgeColor entries must be in [0, 1]");
        endif
        h.EdgeColor = double (val);
      else
        h.EdgeColor = __graph_plot_validate_colorspec__ (val, "EdgeColor");
      endif
    endfunction

    function h = set.NodeLabelColor (h, val)
      h.NodeLabelColor = ...
        __graph_plot_validate_colorspec__ (val, "NodeLabelColor");
    endfunction

    function h = set.Marker (h, val)
      valid = {"+", "o", "*", ".", "x", "s", "square", "d", "diamond", ...
               "^", "v", ">", "<", "p", "pentagram", "h", "hexagram", ...
               "none"};
      ## Per-node cellstr of length NumNodes is also accepted.
      if (iscell (val))
        if (! iscellstr (val))
          error ("Octave:invalid-input-arg", ...
                 "GraphPlot: Marker cell must contain only character vectors");
        endif
        N = h.NumNodes;
        if (numel (val) != N)
          error ("Octave:invalid-input-arg", ...
                 "GraphPlot: Marker cell must have %d elements", N);
        endif
        for kk = 1:numel (val)
          if (! any (strcmp (val{kk}, valid)))
            error ("Octave:invalid-input-arg", ...
                   "GraphPlot: Marker value '%s' is not supported", val{kk});
          endif
        endfor
        h.Marker = val(:);
        return;
      endif
      if (! (ischar (val) && isrow (val)))
        error ("Octave:invalid-input-arg", ...
               "GraphPlot: Marker must be a character vector or cellstr");
      endif
      if (! any (strcmp (val, valid)))
        error ("Octave:invalid-input-arg", ...
               "GraphPlot: Marker value '%s' is not supported", val);
      endif
      h.Marker = val;
    endfunction

    function h = set.MarkerSize (h, val)
      ## Per-node vector of length NumNodes is also accepted.
      if (! (isnumeric (val) && isreal (val) && ! isempty (val) ...
             && all (isfinite (val(:))) && all (val(:) > 0)))
        error ("Octave:invalid-input-arg", ...
               "GraphPlot: MarkerSize must be a positive real scalar or vector");
      endif
      if (isscalar (val))
        h.MarkerSize = double (val);
        return;
      endif
      if (! isvector (val))
        error ("Octave:invalid-input-arg", ...
               "GraphPlot: MarkerSize must be a positive real scalar or vector");
      endif
      N = h.NumNodes;
      if (numel (val) != N)
        error ("Octave:invalid-input-arg", ...
               "GraphPlot: MarkerSize must be scalar or have %d elements", N);
      endif
      h.MarkerSize = double (val(:));
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

    function h = set.LineWidth (h, val)
      ## Per-edge vector of length NumEdges is also accepted (used by
      ## highlight (h, s, t, ..., 'LineWidth', ...)).
      if (! (isnumeric (val) && isreal (val) && ! isempty (val) ...
             && all (isfinite (val(:))) && all (val(:) > 0)))
        error ("Octave:invalid-input-arg", ...
               "GraphPlot: LineWidth must be a positive real scalar or vector");
      endif
      if (isscalar (val))
        h.LineWidth = double (val);
        return;
      endif
      if (! isvector (val))
        error ("Octave:invalid-input-arg", ...
               "GraphPlot: LineWidth must be a positive real scalar or vector");
      endif
      M = h.NumEdges;
      if (numel (val) != M)
        error ("Octave:invalid-input-arg", ...
               "GraphPlot: LineWidth must be scalar or have %d elements", M);
      endif
      h.LineWidth = double (val(:));
    endfunction

    function h = set.LineStyle (h, val)
      valid = {"-", "--", ":", "-.", "none"};
      ## Per-edge cellstr of length NumEdges is also accepted (used by
      ## highlight (h, s, t, ..., 'LineStyle', ...)).
      if (iscell (val))
        if (! iscellstr (val))
          error ("Octave:invalid-input-arg", ...
                 "GraphPlot: LineStyle cell must contain only character vectors");
        endif
        M = h.NumEdges;
        if (numel (val) != M)
          error ("Octave:invalid-input-arg", ...
                 "GraphPlot: LineStyle cell must have %d elements", M);
        endif
        for kk = 1:numel (val)
          if (! any (strcmp (val{kk}, valid)))
            error ("Octave:invalid-input-arg", ...
                   "GraphPlot: LineStyle value '%s' is not supported", val{kk});
          endif
        endfor
        h.LineStyle = val(:);
        return;
      endif
      if (! (ischar (val) && isrow (val)))
        error ("Octave:invalid-input-arg", ...
               "GraphPlot: LineStyle must be a character vector or cellstr");
      endif
      if (! any (strcmp (val, valid)))
        error ("Octave:invalid-input-arg", ...
               "GraphPlot: LineStyle value '%s' is not supported", val);
      endif
      h.LineStyle = val;
    endfunction

    function h = set.ArrowSize (h, val)
      if (! (isnumeric (val) && isscalar (val) && isreal (val) ...
             && isfinite (val) && val > 0))
        error ("Octave:invalid-input-arg", ...
               "GraphPlot: ArrowSize must be a positive real scalar");
      endif
      h.ArrowSize = double (val);
    endfunction

    function h = set.ArrowPosition (h, val)
      if (! (isnumeric (val) && isscalar (val) && isreal (val) ...
             && isfinite (val) && val > 0 && val < 1))
        error ("Octave:invalid-input-arg", ...
               "GraphPlot: ArrowPosition must be a real scalar in (0, 1)");
      endif
      h.ArrowPosition = double (val);
    endfunction

    function h = set.EdgeAlpha (h, val)
      if (! (isnumeric (val) && isscalar (val) && isreal (val) ...
             && isfinite (val) && val >= 0 && val <= 1))
        error ("Octave:invalid-input-arg", ...
               "GraphPlot: EdgeAlpha must be a real scalar in [0, 1]");
      endif
      h.EdgeAlpha = double (val);
    endfunction

    function h = set.EdgeFontSize (h, val)
      if (! (isnumeric (val) && isscalar (val) && isreal (val) ...
             && isfinite (val) && val > 0))
        error ("Octave:invalid-input-arg", ...
               "GraphPlot: EdgeFontSize must be a positive real scalar");
      endif
      h.EdgeFontSize = double (val);
    endfunction

    function h = set.EdgeFontName (h, val)
      if (! (ischar (val) && isrow (val)))
        error ("Octave:invalid-input-arg", ...
               "GraphPlot: EdgeFontName must be a character vector");
      endif
      h.EdgeFontName = val;
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

## -------- US-GP08 edge cosmetic properties --------

## Default edge cosmetic properties on an empty GraphPlot.
%!test
%! h = GraphPlot ();
%! assert (h.EdgeColor, [0 0.4470 0.7410], 1e-12);
%! assert (h.LineWidth, 0.5);
%! assert (h.LineStyle, "-");
%! assert (h.ArrowSize, 7);
%! assert (h.ArrowPosition, 0.5);
%! assert (h.EdgeAlpha, 0.5);
%! assert (iscell (h.EdgeLabel));
%! assert (isempty (h.EdgeLabel));
%! assert (h.EdgeLabelMode, "auto");
%! assert (h.EdgeFontSize, 8);
%! assert (h.EdgeFontName, "Helvetica");

## EdgeColor can be set via name-value in the constructor.
%!test
%! G = digraph ([1 2], [2 3]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G, "EdgeColor", [1 0 0]);
%!   assert (h.EdgeColor, [1 0 0]);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## EdgeColor accepts a color-name string and direct assignment.
%!test
%! h = GraphPlot ();
%! h.EdgeColor = "r";
%! assert (h.EdgeColor, [1 0 0]);
%! h.EdgeColor = [0.2 0.3 0.4];
%! assert (h.EdgeColor, [0.2 0.3 0.4], 1e-12);
%! h.EdgeColor = "green";
%! assert (h.EdgeColor, [0 1 0]);

## Invalid EdgeColor values rejected.
%!error <EdgeColor> h = GraphPlot (); h.EdgeColor = [2 0 0];
%!error <EdgeColor> h = GraphPlot (); h.EdgeColor = "bogus";
%!error <EdgeColor> GraphPlot (digraph (2), "EdgeColor", [3 3 3])

## LineWidth set, validate.
%!test
%! G = digraph ([1 2], [2 3]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G, "LineWidth", 2);
%!   assert (h.LineWidth, 2);
%!   h.LineWidth = 1.5;
%!   assert (h.LineWidth, 1.5);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## LineWidth validation.
%!error <LineWidth> h = GraphPlot (); h.LineWidth = 0;
%!error <LineWidth> h = GraphPlot (); h.LineWidth = -1;
%!error <LineWidth> h = GraphPlot (); h.LineWidth = [1 2];
%!error <LineWidth> h = GraphPlot (); h.LineWidth = "big";

## LineStyle set, validate (all MATLAB-style values accepted).
%!test
%! G = digraph ([1 2], [2 3]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G, "LineStyle", "--");
%!   assert (h.LineStyle, "--");
%!   h.LineStyle = ":";
%!   assert (h.LineStyle, ":");
%!   h.LineStyle = "-.";
%!   assert (h.LineStyle, "-.");
%!   h.LineStyle = "none";
%!   assert (h.LineStyle, "none");
%!   h.LineStyle = "-";
%!   assert (h.LineStyle, "-");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## LineStyle validation.
%!error <LineStyle> h = GraphPlot (); h.LineStyle = "**";
%!error <LineStyle> h = GraphPlot (); h.LineStyle = 1;
%!error <LineStyle> h = GraphPlot (); h.LineStyle = {"--"};

## ArrowSize default, set, validate.
%!test
%! G = digraph ([1 2], [2 3]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G, "ArrowSize", 10);
%!   assert (h.ArrowSize, 10);
%!   h.ArrowSize = 4.5;
%!   assert (h.ArrowSize, 4.5);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## ArrowSize must be positive numeric scalar.
%!error <ArrowSize> h = GraphPlot (); h.ArrowSize = 0;
%!error <ArrowSize> h = GraphPlot (); h.ArrowSize = -1;
%!error <ArrowSize> h = GraphPlot (); h.ArrowSize = [1 2];
%!error <ArrowSize> h = GraphPlot (); h.ArrowSize = "big";

## ArrowPosition default, set, validate (must be in (0, 1)).
%!test
%! G = digraph ([1 2], [2 3]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G, "ArrowPosition", 0.25);
%!   assert (h.ArrowPosition, 0.25);
%!   h.ArrowPosition = 0.9;
%!   assert (h.ArrowPosition, 0.9);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## ArrowPosition validation.
%!error <ArrowPosition> h = GraphPlot (); h.ArrowPosition = 0;
%!error <ArrowPosition> h = GraphPlot (); h.ArrowPosition = 1;
%!error <ArrowPosition> h = GraphPlot (); h.ArrowPosition = -0.1;
%!error <ArrowPosition> h = GraphPlot (); h.ArrowPosition = 1.5;
%!error <ArrowPosition> h = GraphPlot (); h.ArrowPosition = [0.2 0.3];
%!error <ArrowPosition> h = GraphPlot (); h.ArrowPosition = "half";

## EdgeAlpha default, set, validate (must be in [0, 1]).
%!test
%! G = digraph ([1 2], [2 3]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G, "EdgeAlpha", 0.3);
%!   assert (h.EdgeAlpha, 0.3);
%!   h.EdgeAlpha = 1;
%!   assert (h.EdgeAlpha, 1);
%!   h.EdgeAlpha = 0;
%!   assert (h.EdgeAlpha, 0);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## EdgeAlpha validation.
%!error <EdgeAlpha> h = GraphPlot (); h.EdgeAlpha = -0.1;
%!error <EdgeAlpha> h = GraphPlot (); h.EdgeAlpha = 1.1;
%!error <EdgeAlpha> h = GraphPlot (); h.EdgeAlpha = [0.2 0.3];
%!error <EdgeAlpha> h = GraphPlot (); h.EdgeAlpha = "opaque";

## EdgeLabel default is empty cell on an unweighted graph.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   assert (iscell (h.EdgeLabel));
%!   assert (isempty (h.EdgeLabel));
%!   assert (h.EdgeLabelMode, "auto");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## EdgeLabel auto-default is weight strings on a weighted digraph.
%!test
%! G = digraph ([1 2 3], [2 3 1], [10 20 30]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   assert (iscell (h.EdgeLabel));
%!   assert (numel (h.EdgeLabel), 3);
%!   assert (iscolumn (h.EdgeLabel));
%!   assert (h.EdgeLabelMode, "auto");
%!   ## Entries come from num2str on each weight.
%!   assert (h.EdgeLabel{1}, num2str (10));
%!   assert (h.EdgeLabel{2}, num2str (20));
%!   assert (h.EdgeLabel{3}, num2str (30));
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Setting EdgeLabel with a cellstr row yields a column and flips mode.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G, "EdgeLabel", {"x", "y", "z"});
%!   assert (h.EdgeLabel, {"x"; "y"; "z"});
%!   assert (h.EdgeLabelMode, "manual");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Numeric EdgeLabel vector converts via num2str.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G, "EdgeLabel", [10 20 30]);
%!   assert (iscell (h.EdgeLabel));
%!   assert (h.EdgeLabel, {"10"; "20"; "30"});
%!   assert (h.EdgeLabelMode, "manual");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Assigning EdgeLabel after construction flips mode to 'manual'.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   assert (h.EdgeLabelMode, "auto");
%!   h.EdgeLabel = {"a"; "b"; "c"};
%!   assert (h.EdgeLabel, {"a"; "b"; "c"});
%!   assert (h.EdgeLabelMode, "manual");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Flipping EdgeLabelMode back to 'auto' regenerates weight labels.
%!test
%! G = digraph ([1 2 3], [2 3 1], [10 20 30]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   h.EdgeLabel = {"aa"; "bb"; "cc"};   # now manual
%!   assert (h.EdgeLabelMode, "manual");
%!   h.EdgeLabelMode = "auto";
%!   assert (numel (h.EdgeLabel), 3);
%!   assert (h.EdgeLabel{1}, num2str (10));
%!   assert (h.EdgeLabelMode, "auto");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## 'auto' on an unweighted graph regenerates empty EdgeLabel.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   h.EdgeLabel = {"a"; "b"; "c"};   # now manual
%!   assert (h.EdgeLabelMode, "manual");
%!   h.EdgeLabelMode = "auto";
%!   assert (iscell (h.EdgeLabel));
%!   assert (isempty (h.EdgeLabel));
%!   assert (h.EdgeLabelMode, "auto");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## EdgeLabelMode validation.
%!test
%! h = GraphPlot ();
%! assert (h.EdgeLabelMode, "auto");
%! h.EdgeLabelMode = "manual";
%! assert (h.EdgeLabelMode, "manual");
%! h.EdgeLabelMode = "AUTO";
%! assert (h.EdgeLabelMode, "auto");

%!error <EdgeLabelMode> h = GraphPlot (); h.EdgeLabelMode = "bogus";
%!error <EdgeLabelMode> h = GraphPlot (); h.EdgeLabelMode = 1;

## Length-mismatched EdgeLabel is rejected.
%!error <EdgeLabel> ...
%!   GraphPlot (digraph ([1 2 3], [2 3 1]), "EdgeLabel", {"a", "b"})
%!error <EdgeLabel> ...
%!   h = GraphPlot (digraph ([1 2], [2 3])); h.EdgeLabel = {"p"};

## Non-cellstr EdgeLabel entries rejected.
%!error <EdgeLabel> ...
%!   h = GraphPlot (digraph ([1 2], [2 3])); h.EdgeLabel = {1};

## EdgeFontSize default, set, validate.
%!test
%! G = digraph ([1 2], [2 3]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G, "EdgeFontSize", 12);
%!   assert (h.EdgeFontSize, 12);
%!   h.EdgeFontSize = 10;
%!   assert (h.EdgeFontSize, 10);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## EdgeFontSize validation.
%!error <EdgeFontSize> h = GraphPlot (); h.EdgeFontSize = 0;
%!error <EdgeFontSize> h = GraphPlot (); h.EdgeFontSize = -1;
%!error <EdgeFontSize> h = GraphPlot (); h.EdgeFontSize = [1 2];
%!error <EdgeFontSize> h = GraphPlot (); h.EdgeFontSize = "big";

## EdgeFontName default, set, validate.
%!test
%! G = digraph ([1 2], [2 3]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G, "EdgeFontName", "Arial");
%!   assert (h.EdgeFontName, "Arial");
%!   h.EdgeFontName = "Times";
%!   assert (h.EdgeFontName, "Times");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## EdgeFontName validation.
%!error <EdgeFontName> h = GraphPlot (); h.EdgeFontName = 1;
%!error <EdgeFontName> h = GraphPlot (); h.EdgeFontName = {"Arial"};

## All edge cosmetic options forwarded via constructor name-value in one call.
%!test
%! G = digraph ([1 2 3], [2 3 1], [5 10 15]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G, ...
%!                  "EdgeColor", [0.5 0.5 0.5], ...
%!                  "LineWidth", 1.5, ...
%!                  "LineStyle", "--", ...
%!                  "ArrowSize", 9, ...
%!                  "ArrowPosition", 0.75, ...
%!                  "EdgeAlpha", 0.7, ...
%!                  "EdgeLabel", {"one", "two", "three"}, ...
%!                  "EdgeFontSize", 10, ...
%!                  "EdgeFontName", "Courier");
%!   assert (h.EdgeColor, [0.5 0.5 0.5], 1e-12);
%!   assert (h.LineWidth, 1.5);
%!   assert (h.LineStyle, "--");
%!   assert (h.ArrowSize, 9);
%!   assert (h.ArrowPosition, 0.75);
%!   assert (h.EdgeAlpha, 0.7);
%!   assert (h.EdgeLabel, {"one"; "two"; "three"});
%!   assert (h.EdgeLabelMode, "manual");
%!   assert (h.EdgeFontSize, 10);
%!   assert (h.EdgeFontName, "Courier");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Edge option names are case-insensitive on input.
%!test
%! G = digraph ([1 2], [2 3]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G, "edgecolor", [0 1 0], "ARROWSIZE", 8, ...
%!                  "EdgeAlpha", 0.4, "linewidth", 2);
%!   assert (h.EdgeColor, [0 1 0]);
%!   assert (h.ArrowSize, 8);
%!   assert (h.EdgeAlpha, 0.4);
%!   assert (h.LineWidth, 2);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Handle-class: edge cosmetic property assignment visible in aliased handles.
%!test
%! h1 = GraphPlot ();
%! h2 = h1;
%! h2.EdgeFontSize = 22;
%! assert (h1.EdgeFontSize, 22);
%! h1.ArrowSize = 13;
%! assert (h2.ArrowSize, 13);
%! h2.EdgeAlpha = 0.2;
%! assert (h1.EdgeAlpha, 0.2);

## Undirected graph still accepts ArrowSize and ArrowPosition (stored
## but cosmetically unused).
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G, "ArrowSize", 12, "ArrowPosition", 0.2);
%!   assert (h.ArrowSize, 12);
%!   assert (h.ArrowPosition, 0.2);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Undirected graph auto EdgeLabel uses weights when present.
%!test
%! G = graph ([1 2 3], [2 3 1], [0.5 1.5 2.5]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   assert (iscell (h.EdgeLabel));
%!   assert (numel (h.EdgeLabel), 3);
%!   assert (h.EdgeLabelMode, "auto");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Edgeless digraph: EdgeLabel default is empty regardless of weighting.
%!test
%! G = digraph (4);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   assert (iscell (h.EdgeLabel));
%!   assert (isempty (h.EdgeLabel));
%!   assert (h.EdgeLabelMode, "auto");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## -------- US-GP09 highlight(h, nodes[, props]) --------

## Basic: highlight(h, 1) turns node 1 red; others unchanged (NodeColor
## becomes an Nx3 matrix).
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   highlight (h, 1);
%!   assert (size (h.NodeColor), [3, 3]);
%!   assert (h.NodeColor(1, :), [1 0 0]);
%!   assert (h.NodeColor(2, :), [0 0.4470 0.7410], 1e-12);
%!   assert (h.NodeColor(3, :), [0 0.4470 0.7410], 1e-12);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Highlight multiple nodes at once via a vector.
%!test
%! G = digraph ([1 2 3 4], [2 3 4 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   highlight (h, [1 3]);
%!   assert (size (h.NodeColor), [4, 3]);
%!   assert (h.NodeColor(1, :), [1 0 0]);
%!   assert (h.NodeColor(3, :), [1 0 0]);
%!   assert (h.NodeColor(2, :), [0 0.4470 0.7410], 1e-12);
%!   assert (h.NodeColor(4, :), [0 0.4470 0.7410], 1e-12);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Column vector of indices also accepted.
%!test
%! G = digraph ([1 2 3 4], [2 3 4 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   highlight (h, [2; 4]);
%!   assert (h.NodeColor(2, :), [1 0 0]);
%!   assert (h.NodeColor(4, :), [1 0 0]);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Node name as a bare char row vector.
%!test
%! G = digraph ([1 2 3], [2 3 1], [], {"alpha", "beta", "gamma"});
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   highlight (h, "beta");
%!   assert (h.NodeColor(2, :), [1 0 0]);
%!   assert (h.NodeColor(1, :), [0 0.4470 0.7410], 1e-12);
%!   assert (h.NodeColor(3, :), [0 0.4470 0.7410], 1e-12);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Cellstr of node names.
%!test
%! G = digraph ([1 2 3], [2 3 1], [], {"alpha", "beta", "gamma"});
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   highlight (h, {"alpha", "gamma"});
%!   assert (h.NodeColor(1, :), [1 0 0]);
%!   assert (h.NodeColor(3, :), [1 0 0]);
%!   assert (h.NodeColor(2, :), [0 0.4470 0.7410], 1e-12);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Override default color with a triplet.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   highlight (h, 2, "NodeColor", [0 0.5 0]);
%!   assert (h.NodeColor(2, :), [0 0.5 0]);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Override default color with a short name.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   highlight (h, 2, "NodeColor", "g");
%!   assert (h.NodeColor(2, :), [0 1 0]);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Override default color with a long name.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   highlight (h, 2, "NodeColor", "magenta");
%!   assert (h.NodeColor(2, :), [1 0 1]);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Custom Marker per-node: Marker becomes a column cellstr.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   highlight (h, 2, "Marker", "s");
%!   assert (iscell (h.Marker));
%!   assert (numel (h.Marker), 3);
%!   assert (h.Marker{1}, "o");
%!   assert (h.Marker{2}, "s");
%!   assert (h.Marker{3}, "o");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Custom MarkerSize per-node: MarkerSize becomes a column vector.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   highlight (h, 2, "MarkerSize", 12);
%!   assert (numel (h.MarkerSize), 3);
%!   assert (h.MarkerSize(1), 4);
%!   assert (h.MarkerSize(2), 12);
%!   assert (h.MarkerSize(3), 4);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Multiple properties in one call: NodeColor, Marker, MarkerSize.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   highlight (h, [1 3], "NodeColor", "y", "Marker", "d", "MarkerSize", 20);
%!   assert (h.NodeColor(1, :), [1 1 0]);
%!   assert (h.NodeColor(3, :), [1 1 0]);
%!   assert (h.Marker{1}, "d");
%!   assert (h.Marker{3}, "d");
%!   assert (h.Marker{2}, "o");
%!   assert (h.MarkerSize(1), 20);
%!   assert (h.MarkerSize(3), 20);
%!   assert (h.MarkerSize(2), 4);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Case-insensitive option names.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   highlight (h, 1, "nodecolor", "c");
%!   assert (h.NodeColor(1, :), [0 1 1]);
%!   highlight (h, 2, "MARKER", "^");
%!   assert (h.Marker{2}, "^");
%!   highlight (h, 3, "MarkerSIZE", 9);
%!   assert (h.MarkerSize(3), 9);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Handle-class semantics: aliased handle sees the update.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h1 = GraphPlot (G);
%!   h2 = h1;
%!   highlight (h1, 2);
%!   assert (h2.NodeColor(2, :), [1 0 0]);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Two highlight calls compose cumulatively.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   highlight (h, 1);
%!   highlight (h, 3, "NodeColor", "g");
%!   assert (h.NodeColor(1, :), [1 0 0]);
%!   assert (h.NodeColor(3, :), [0 1 0]);
%!   assert (h.NodeColor(2, :), [0 0.4470 0.7410], 1e-12);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## highlight works on an undirected graph too.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   highlight (h, [1 2]);
%!   assert (h.NodeColor(1, :), [1 0 0]);
%!   assert (h.NodeColor(2, :), [1 0 0]);
%!   assert (h.NodeColor(3, :), [0 0.4470 0.7410], 1e-12);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Empty index list is a silent no-op (NodeColor stays 1x3).
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   highlight (h, []);
%!   assert (h.NodeColor, [0 0.4470 0.7410], 1e-12);
%!   assert (size (h.NodeColor), [1 3]);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Error: invalid node index (out of range).
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   fail ("highlight (h, 99)", "invalid node index");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Error: node name not found.
%!test
%! G = digraph ([1 2 3], [2 3 1], [], {"alpha", "beta", "gamma"});
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   fail ("highlight (h, 'zeta')", "not found");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Error: odd number of name-value args.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   fail ("highlight (h, 1, 'NodeColor')", "pairs");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Error: unknown option.  Uses a known leading option so dispatch
## stays in node form and the unknown 'Bogus' is rejected as a name-
## value option.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   fail ("highlight (h, 1, 'NodeColor', 'r', 'Bogus', 1)", "unknown option");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Error: invalid color spec (out of range).
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   fail ("highlight (h, 1, 'NodeColor', [2 0 0])", "RGB triplet");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Error: invalid marker value.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   fail ("highlight (h, 1, 'Marker', 'bogus')", "not supported");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Error: non-positive MarkerSize.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   fail ("highlight (h, 1, 'MarkerSize', -1)", "positive");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Calling highlight via free-function entry point (scripts/graph/highlight.m)
## works identically to the classdef method dispatch.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   highlight (h, 2, "NodeColor", "k");
%!   assert (h.NodeColor(2, :), [0 0 0]);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Default construction -> NodeColor remains 1x3 after highlight() with
## empty nodes argument (does not expand).
%!test
%! G = digraph ([1 2], [2 3]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   highlight (h, []);
%!   assert (size (h.NodeColor), [1 3]);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## highlight returns no required output (void); call in statement form.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   highlight (h, 1);  # statement form - must not error
%!   assert (h.NodeColor(1, :), [1 0 0]);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Assigning h.NodeColor = Nx3 (without highlight) is also accepted now.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   M = [1 0 0; 0 1 0; 0 0 1];
%!   h.NodeColor = M;
%!   assert (h.NodeColor, M);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Assigning h.Marker = cellstr of N is also accepted now.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   h.Marker = {"s"; "d"; "o"};
%!   assert (iscell (h.Marker));
%!   assert (h.Marker, {"s"; "d"; "o"});
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Assigning h.MarkerSize = vector of N is also accepted now.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   h.MarkerSize = [5; 10; 15];
%!   assert (h.MarkerSize, [5; 10; 15]);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Existing uniform scalar assignment still works (no regression).
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   h.NodeColor = "r";
%!   assert (h.NodeColor, [1 0 0]);
%!   assert (size (h.NodeColor), [1 3]);
%!   h.Marker = "s";
%!   assert (h.Marker, "s");
%!   h.MarkerSize = 7;
%!   assert (h.MarkerSize, 7);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## ============================================================
## US-GP10: highlight(h, s, t[, props]) - edge endpoint form
## ============================================================

## Edge highlight by scalar (s, t): EdgeColor expands to Mx3,
## matching edge turns red, others stay default blue.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   highlight (h, 1, 2);
%!   assert (size (h.EdgeColor), [3, 3]);
%!   assert (h.EdgeColor(1, :), [1 0 0]);
%!   assert (h.EdgeColor(2, :), [0 0.4470 0.7410], 1e-12);
%!   assert (h.EdgeColor(3, :), [0 0.4470 0.7410], 1e-12);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Edge highlight by vector (s, t): row-vector input.
%!test
%! G = digraph ([1 2 3 4], [2 3 4 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   highlight (h, [1 3], [2 4]);
%!   assert (size (h.EdgeColor), [4, 3]);
%!   assert (h.EdgeColor(1, :), [1 0 0]);
%!   assert (h.EdgeColor(3, :), [1 0 0]);
%!   assert (h.EdgeColor(2, :), [0 0.4470 0.7410], 1e-12);
%!   assert (h.EdgeColor(4, :), [0 0.4470 0.7410], 1e-12);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Edge highlight by column vectors (s, t).
%!test
%! G = digraph ([1 2 3 4], [2 3 4 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   highlight (h, [1; 3], [2; 4]);
%!   assert (h.EdgeColor(1, :), [1 0 0]);
%!   assert (h.EdgeColor(3, :), [1 0 0]);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Edge highlight by node names (char rows for single scalar edge).
%!test
%! G = digraph ([1 2 3], [2 3 1], [], {"alpha", "beta", "gamma"});
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   highlight (h, "alpha", "beta");
%!   assert (h.EdgeColor(1, :), [1 0 0]);
%!   assert (h.EdgeColor(2, :), [0 0.4470 0.7410], 1e-12);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Edge highlight by cellstr endpoints.
%!test
%! G = digraph ([1 2 3 4], [2 3 4 1], [], {"a", "b", "c", "d"});
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   highlight (h, {"a", "c"}, {"b", "d"});
%!   assert (h.EdgeColor(1, :), [1 0 0]);
%!   assert (h.EdgeColor(3, :), [1 0 0]);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## EdgeColor override with RGB triplet.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   highlight (h, 1, 2, "EdgeColor", [0 0.5 0]);
%!   assert (h.EdgeColor(1, :), [0 0.5 0]);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## EdgeColor override with short color name.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   highlight (h, 1, 2, "EdgeColor", "g");
%!   assert (h.EdgeColor(1, :), [0 1 0]);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## EdgeColor override with long color name.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   highlight (h, 2, 3, "EdgeColor", "magenta");
%!   assert (h.EdgeColor(2, :), [1 0 1]);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## LineWidth override: LineWidth becomes a column vector.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   highlight (h, 1, 2, "LineWidth", 3);
%!   assert (numel (h.LineWidth), 3);
%!   assert (h.LineWidth(1), 3);
%!   assert (h.LineWidth(2), 0.5);
%!   assert (h.LineWidth(3), 0.5);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## LineStyle override: LineStyle becomes a column cellstr.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   highlight (h, 1, 2, "LineStyle", "--");
%!   assert (iscell (h.LineStyle));
%!   assert (numel (h.LineStyle), 3);
%!   assert (h.LineStyle{1}, "--");
%!   assert (h.LineStyle{2}, "-");
%!   assert (h.LineStyle{3}, "-");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Multiple edge properties in one highlight call.
%!test
%! G = digraph ([1 2 3 4], [2 3 4 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   highlight (h, [1 3], [2 4], "EdgeColor", "y", "LineWidth", 4, ...
%!              "LineStyle", ":");
%!   assert (h.EdgeColor(1, :), [1 1 0]);
%!   assert (h.EdgeColor(3, :), [1 1 0]);
%!   assert (h.LineWidth(1), 4);
%!   assert (h.LineWidth(3), 4);
%!   assert (h.LineStyle{1}, ":");
%!   assert (h.LineStyle{3}, ":");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Case-insensitive option names in edge form.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   highlight (h, 1, 2, "edgecolor", "c");
%!   assert (h.EdgeColor(1, :), [0 1 1]);
%!   highlight (h, 2, 3, "LINEWIDTH", 2.5);
%!   assert (h.LineWidth(2), 2.5);
%!   highlight (h, 3, 1, "lineSTYLE", "-.");
%!   assert (h.LineStyle{3}, "-.");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Undirected graph: highlight (s, t) and (t, s) match the same edge.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   highlight (h, 2, 1, "EdgeColor", "r");
%!   ## (2, 1) and (1, 2) should reference the same edge.  In the
%!   ## canonical storage of the undirected graph this is edge #1.
%!   assert (h.EdgeColor(1, :), [1 0 0]);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Empty (s, t) is a silent no-op: EdgeColor stays 1x3.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   highlight (h, [], []);
%!   assert (size (h.EdgeColor), [1 3]);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Composition: two edge-highlight calls accumulate.
%!test
%! G = digraph ([1 2 3 4], [2 3 4 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   highlight (h, 1, 2, "EdgeColor", "r");
%!   highlight (h, 3, 4, "EdgeColor", "g");
%!   assert (h.EdgeColor(1, :), [1 0 0]);
%!   assert (h.EdgeColor(3, :), [0 1 0]);
%!   assert (h.EdgeColor(2, :), [0 0.4470 0.7410], 1e-12);
%!   assert (h.EdgeColor(4, :), [0 0.4470 0.7410], 1e-12);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Composition with node highlight: node state stays untouched by
## edge highlight and vice versa.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   highlight (h, 1);                        # node highlight
%!   highlight (h, 2, 3, "EdgeColor", "g");   # edge highlight
%!   assert (h.NodeColor(1, :), [1 0 0]);
%!   assert (h.EdgeColor(2, :), [0 1 0]);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Handle-class semantics: aliased handle sees the edge update.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h1 = GraphPlot (G);
%!   h2 = h1;
%!   highlight (h1, 2, 3);
%!   assert (h2.EdgeColor(2, :), [1 0 0]);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Error: length mismatch between s and t in edge form.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   fail ("highlight (h, [1 2], [2])", "length");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Error: out-of-range node index in edge form.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   fail ("highlight (h, 1, 99)", "invalid node");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Error: node name not found in edge form.
%!test
%! G = digraph ([1 2 3], [2 3 1], [], {"alpha", "beta", "gamma"});
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   fail ("highlight (h, 'alpha', 'omega')", "not found");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Error: (s, t) pair doesn't correspond to any edge.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   ## edge (1, 3) doesn't exist (graph has edges (1,2),(2,3),(3,1))
%!   fail ("highlight (h, 1, 3)", "edge");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Error: odd number of name-value args in edge form.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   fail ("highlight (h, 1, 2, 'EdgeColor')", "pairs");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Error: unknown option in edge form.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   fail ("highlight (h, 1, 2, 'Bogus', 1)", "unknown option");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Error: invalid EdgeColor spec (out of range).
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   fail ("highlight (h, 1, 2, 'EdgeColor', [2 0 0])", "RGB triplet");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Error: non-positive LineWidth.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   fail ("highlight (h, 1, 2, 'LineWidth', -1)", "positive");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Error: unsupported LineStyle.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   fail ("highlight (h, 1, 2, 'LineStyle', '~~')", "supported");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Direct assignment: h.EdgeColor = Mx3 (via extended setter).
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   M = [1 0 0; 0 1 0; 0 0 1];
%!   h.EdgeColor = M;
%!   assert (h.EdgeColor, M);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Direct assignment: h.LineWidth = Mx1 vector (via extended setter).
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   h.LineWidth = [1; 2; 3];
%!   assert (h.LineWidth, [1; 2; 3]);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Direct assignment: h.LineStyle = Mx1 cellstr (via extended setter).
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   h.LineStyle = {"-"; "--"; ":"};
%!   assert (iscell (h.LineStyle));
%!   assert (h.LineStyle, {"-"; "--"; ":"});
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Existing uniform scalar assignment still works for edge cosmetics
## (no regression).
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   h.EdgeColor = "r";
%!   assert (h.EdgeColor, [1 0 0]);
%!   assert (size (h.EdgeColor), [1 3]);
%!   h.LineWidth = 2;
%!   assert (h.LineWidth, 2);
%!   h.LineStyle = "--";
%!   assert (h.LineStyle, "--");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Dispatch test: highlight (h, nodes, NodeColor, val) still routes
## to the node form even though a 3rd positional arg is present.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   highlight (h, 2, "NodeColor", "m");
%!   assert (h.NodeColor(2, :), [1 0 1]);
%!   ## EdgeColor should remain 1x3 (no edge highlight)
%!   assert (size (h.EdgeColor), [1 3]);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Dispatch test: highlight (h, nodes, Marker, val) still routes to
## the node form (3rd arg "Marker" is a known option name).
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   highlight (h, 1, "Marker", "s");
%!   assert (iscell (h.Marker));
%!   assert (h.Marker{1}, "s");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## free-function highlight(h, s, t, ...) dispatches to edge form.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = plot (G);
%!   highlight (h, 1, 2, "EdgeColor", "g");
%!   assert (h.EdgeColor(1, :), [0 1 0]);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## ========== US-GP11: highlight (h, 'Edges', idx, ...) ==========

## Scalar edge index highlighted red by default.
%!test
%! G = digraph ([1 2 3 4 5], [2 3 4 5 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   highlight (h, "Edges", 1);
%!   assert (h.EdgeColor(1, :), [1 0 0]);
%!   assert (h.EdgeColor(2, :), h.EdgeColor(3, :));
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Vector edge indices highlighted red by default.
%!test
%! G = digraph ([1 2 3 4 5], [2 3 4 5 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   highlight (h, "Edges", [1 3 5]);
%!   assert (h.EdgeColor(1, :), [1 0 0]);
%!   assert (h.EdgeColor(3, :), [1 0 0]);
%!   assert (h.EdgeColor(5, :), [1 0 0]);
%!   ## Unselected edges retain default (not red)
%!   assert (h.EdgeColor(2, :) == [1 0 0], [false false false]);
%!   assert (h.EdgeColor(4, :) == [1 0 0], [false false false]);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Column-vector edge indices accepted.
%!test
%! G = digraph ([1 2 3 4 5], [2 3 4 5 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   highlight (h, "Edges", [2; 4]);
%!   assert (h.EdgeColor(2, :), [1 0 0]);
%!   assert (h.EdgeColor(4, :), [1 0 0]);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Case-insensitive 'Edges' keyword: 'edges' and 'EDGES' work too.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   highlight (h, "edges", 2);
%!   assert (h.EdgeColor(2, :), [1 0 0]);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   highlight (h, "EDGES", 3);
%!   assert (h.EdgeColor(3, :), [1 0 0]);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## EdgeColor override (named color + triplet).
%!test
%! G = digraph ([1 2 3 4 5], [2 3 4 5 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   highlight (h, "Edges", [1 3], "EdgeColor", "g");
%!   assert (h.EdgeColor(1, :), [0 1 0]);
%!   assert (h.EdgeColor(3, :), [0 1 0]);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
%!test
%! G = digraph ([1 2 3 4 5], [2 3 4 5 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   highlight (h, "Edges", 2, "EdgeColor", [0.2 0.6 0.9]);
%!   assert (h.EdgeColor(2, :), [0.2 0.6 0.9], 1e-12);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## LineWidth override.
%!test
%! G = digraph ([1 2 3 4 5], [2 3 4 5 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   highlight (h, "Edges", [1 4], "LineWidth", 3);
%!   assert (h.LineWidth(1), 3);
%!   assert (h.LineWidth(4), 3);
%!   assert (h.LineWidth(2) != 3);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## LineStyle override.
%!test
%! G = digraph ([1 2 3 4 5], [2 3 4 5 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   highlight (h, "Edges", [1 3], "LineStyle", "--");
%!   assert (iscell (h.LineStyle));
%!   assert (h.LineStyle{1}, "--");
%!   assert (h.LineStyle{3}, "--");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Multi-property override in a single call.
%!test
%! G = digraph ([1 2 3 4 5], [2 3 4 5 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   highlight (h, "Edges", [2 4], ...
%!              "EdgeColor", [0 1 0], "LineWidth", 2.5, "LineStyle", ":");
%!   assert (h.EdgeColor(2, :), [0 1 0]);
%!   assert (h.EdgeColor(4, :), [0 1 0]);
%!   assert (h.LineWidth(2), 2.5);
%!   assert (h.LineWidth(4), 2.5);
%!   assert (h.LineStyle{2}, ":");
%!   assert (h.LineStyle{4}, ":");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Case-insensitive option names after 'Edges'.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   highlight (h, "Edges", 1, "edgecolor", "m", "linewidth", 2, ...
%!              "LINESTYLE", "-.");
%!   assert (h.EdgeColor(1, :), [1 0 1]);
%!   assert (h.LineWidth(1), 2);
%!   assert (h.LineStyle{1}, "-.");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Empty indices -> silent no-op; EdgeColor stays 1x3.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   highlight (h, "Edges", []);
%!   assert (size (h.EdgeColor), [1 3]);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Undirected graph: edge indexing is into the same stored edge list.
%!test
%! G = graph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   highlight (h, "Edges", [1 2]);
%!   assert (h.EdgeColor(1, :), [1 0 0]);
%!   assert (h.EdgeColor(2, :), [1 0 0]);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Composition: node highlight followed by 'Edges' index highlight
## preserves node changes and vice-versa.
%!test
%! G = digraph ([1 2 3 4 5], [2 3 4 5 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   highlight (h, [1 3], "NodeColor", "g");
%!   highlight (h, "Edges", [2 4], "EdgeColor", "m");
%!   assert (h.NodeColor(1, :), [0 1 0]);
%!   assert (h.NodeColor(3, :), [0 1 0]);
%!   assert (h.EdgeColor(2, :), [1 0 1]);
%!   assert (h.EdgeColor(4, :), [1 0 1]);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Handle-class semantics: highlight mutates in place, no reassignment
## needed.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   h2 = h;
%!   highlight (h, "Edges", 1);
%!   assert (h2.EdgeColor(1, :), [1 0 0]);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Free-function dispatch via scripts/graph/highlight.m for the 'Edges'
## form.
%!test
%! G = digraph ([1 2 3 4 5], [2 3 4 5 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = plot (G);
%!   highlight (h, "Edges", [1 5], "EdgeColor", "b");
%!   assert (h.EdgeColor(1, :), [0 0 1]);
%!   assert (h.EdgeColor(5, :), [0 0 1]);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Regression: node form and (s, t) edge form still dispatch correctly
## when 'Edges' keyword is not present.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   highlight (h, 1);
%!   assert (h.NodeColor(1, :), [1 0 0]);
%!   highlight (h, 2, 3, "EdgeColor", "g");
%!   assert (h.EdgeColor(2, :), [0 1 0]);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Error: missing idx after 'Edges'.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   fail ("highlight (h, 'Edges')", "idx");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Error: non-numeric idx.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   fail ("highlight (h, 'Edges', 'abc')", "numeric");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Error: non-vector idx (matrix).
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   fail ("highlight (h, 'Edges', [1 2; 2 3])", "vector");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Error: edge index out of range (above NumEdges).
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   fail ("highlight (h, 'Edges', 10)", "out of range");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Error: edge index zero / negative.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   fail ("highlight (h, 'Edges', 0)", "positive");
%!   fail ("highlight (h, 'Edges', -1)", "positive");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Error: non-integer idx.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   fail ("highlight (h, 'Edges', 1.5)", "integer");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Error: Inf / NaN idx.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   fail ("highlight (h, 'Edges', Inf)", "finite");
%!   fail ("highlight (h, 'Edges', NaN)", "finite");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Error: odd name-value pairs after idx.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   fail ("highlight (h, 'Edges', 1, 'EdgeColor')", "pairs");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Error: unknown option after 'Edges' form.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   fail ("highlight (h, 'Edges', 1, 'Bogus', 1)", "unknown option");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Error: invalid EdgeColor in 'Edges' form.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   fail ("highlight (h, 'Edges', 1, 'EdgeColor', [2 0 0])", "RGB triplet");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Error: non-positive LineWidth in 'Edges' form.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   fail ("highlight (h, 'Edges', 1, 'LineWidth', -1)", "positive");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Error: unsupported LineStyle in 'Edges' form.
%!test
%! G = digraph ([1 2 3], [2 3 1]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = GraphPlot (G);
%!   fail ("highlight (h, 'Edges', 1, 'LineStyle', '~~')", "supported");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
