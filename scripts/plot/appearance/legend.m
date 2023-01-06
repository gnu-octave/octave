########################################################################
##
## Copyright (C) 2010-2023 The Octave Project Developers
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
## @deftypefn  {} {} legend ()
## @deftypefnx {} {} legend @var{command}
## @deftypefnx {} {} legend (@var{str1}, @var{str2}, @dots{})
## @deftypefnx {} {} legend (@var{charmat})
## @deftypefnx {} {} legend (@{@var{cellstr}@})
## @deftypefnx {} {} legend (@dots{}, @var{property}, @var{value}, @dots{})
## @deftypefnx {} {} legend (@var{hobjs}, @dots{})
## @deftypefnx {} {} legend ("@var{command}")
## @deftypefnx {} {} legend (@var{hax}, @dots{})
## @deftypefnx {} {} legend (@var{hleg}, @dots{})
## @deftypefnx {} {@var{hleg} =} legend (@dots{})
##
## Display a legend for the current axes using the specified strings as labels.
##
## Legend entries may be specified as individual character string arguments,
## a character array, or a cell array of character strings.  When label names
## might be confused with legend properties, or @var{command} arguments,
## the labels should be protected by specifying them as a cell array of
## strings.
##
## If the first argument @var{hax} is an axes handle, then add a legend to this
## axes, rather than the current axes returned by @code{gca}.
##
## If the first argument @var{hleg} is a legend handle, then operate on this
## legend rather than the legend of the current axes.
##
## Legend labels are associated with the axes' children; The first label is
## assigned to the first object that was plotted in the axes, the second label
## to the next object plotted, etc.  To label specific data objects, without
## labeling all objects, provide their graphic handles in the input
## @var{hobjs}.
##
## The following customizations are available using @var{command}:
##
## @table @asis
## @item @qcode{"show"}
##   Show legend on the plot
##
## @item @qcode{"hide"}
##   Hide legend on the plot
##
## @item @qcode{"toggle"}
##   Toggle between @qcode{"hide"} and @qcode{"show"}
##
## @item @qcode{"boxon"}
##   Show a box around legend (default)
##
## @item @qcode{"boxoff"}
##   Hide the box around legend
##
## @item @qcode{"right"}
##   Place label text to the right of the keys (default)
##
## @item @qcode{"left"}
##   Place label text to the left of the keys
##
## @item @qcode{"off"}
##   Delete the legend object
## @end table
##
## The @code{legend} function creates a graphics object which has various
## properties that can be manipulated with @code{get}/@code{set}.
## Alternatively, properties can be set directly when calling @code{legend} by
## including @var{property}/@var{value} pairs.  If using this calling form, the
## labels must be specified as a cell array of strings.  Graphics object
## properties are documented in detail at @ref{Graphics Object Properties}.
##
## Following is a subset of supported legend properties:
## @c The following table is obtained by copying the output of
## @c genpropdoc ("legend", "", {"autoupdate", "box", "location", "numcolumns", "orientation", "string", "textcolor"})
##
## @table @asis
##
## @item @code{autoupdate}: @qcode{"off"} | @{@qcode{"on"}@}
## Control whether the number of legend items is updated automatically when
## objects are added to (or deleted from) the peer axes.  For example:
##
## @example
## @group
## ## Create a single plot with its legend.
## figure ();
## plot (1:10);
## legend ("Slope 1");
## ## Add another plot and specify its displayname so that
## ## the legend is correctly updated.
## hold on;
## plot ((1:10) * 2, "displayname", "Slope 2");
## ## Stop automatic updates for further plots.
## legend ("autoupdate", "off");
## plot ((1:10) * 3);
## @end group
## @end example
##
## @item @code{box}: @qcode{"off"} | @{@qcode{"on"}@}
## Control whether the legend has a surrounding box.
##
## @item @code{location}: @qcode{"best"} | @qcode{"bestoutside"} |
## @qcode{"east"} | @qcode{"eastoutside"} | @qcode{"none"} | @qcode{"north"} |
## @{@qcode{"northeast"}@} | @qcode{"northeastoutside"} |
## @qcode{"northoutside"} | @qcode{"northwest"}| @qcode{"northwestoutside"} |
## @qcode{"south"} | @qcode{"southeast"} | @qcode{"southeastoutside"} |
## @qcode{"southoutside"} | @qcode{"southwest"} | @qcode{"southwestoutside"} |
## @qcode{"west"} | @qcode{"westoutside"}
## Control the location of the legend.
##
## @item @code{numcolumns}: scalar interger, def. @code{1}
## Control the number of columns used in the layout of the legend items.
## For example:
##
## @example
## @group
## figure ();
## plot (rand (30));
## legend ("numcolumns", 3);
## @end group
## @end example
##
## Setting @code{numcolumns} also forces the @code{numcolumnsmode} property
## to be set to @qcode{"manual"}.
##
## @item @code{orientation}: @qcode{"horizontal"} | @{@qcode{"vertical"}@}
## Control whether the legend items are arranged vertically (column-wise) or
## horizontally (row-wise).
##
## @item @code{string}: string | cell array of strings
## List of labels for the legend items.  For example:
##
## @example
## @group
## figure ();
## plot (rand (20));
## ## Let legend choose names automatically
## hl = legend ();
## ## Selectively change some names
## str = get (hl, "string");
## str(1:5:end) = "Garbage";
## set (hl, "string", str);
## @end group
## @end example
##
## @item @code{textcolor}: colorspec, def. @code{[0   0   0]}
## Control the color of the text strings for legend item.
##
## @end table
##
## The full list of supported legend specific properties can be found at
## @ref{Legend Properties, , Legend Properties}.
##
## A legend is implemented as an additional axes object with the @code{tag}
## property set to @qcode{"legend"}.  Properties of the legend object may be
## manipulated directly by using @code{set}.
##
## The optional output value @var{hleg} is a handle to the legend object.
##
## Implementation Note: The legend label text is either provided in the call to
## @code{legend} or is taken from the @code{DisplayName} property of the
## graphics objects.  Only data objects, such as line, patch, and surface, have
## this property whereas axes, figures, etc.@: do not so they are never present
## in a legend.  If no labels or @code{DisplayName} properties are available,
## then the label text is simply @qcode{"data1"}, @qcode{"data2"}, @dots{},
## @nospell{@qcode{"dataN"}}.
##
## The legend @code{FontSize} property is initially set to 90% of the axes
## @code{FontSize} to which it is attached.  Use @code{set} to override this
## if necessary.
## @end deftypefn

function [hleg, hleg_obj, hplot, labels] = legend (varargin)

  ## Use the old legend code to handle gnuplot toolkit
  if (strcmp (graphics_toolkit (), "gnuplot"))
    if (nargout > 0)
      [hleg, hleg_obj, hplot, labels] = __gnuplot_legend__ (varargin{:});
    else
      __gnuplot_legend__ (varargin{:});
    endif
    return;
  endif

  ## FIXME: This function needs to be locked to avoid bug #59439.  Remove this
  ##        lock once that bug is properly fixed.
  mlock ();

  opts = parse_opts (varargin{:});

  hl = opts.legend_handle;

  ## Fix property/value pairs
  pval = opts.propval(:)';

  if (! isempty (opts.action))

    do_set_box = isempty (hl);

    switch (opts.action)
      case "boxoff"
        tmp_pval = {"box", "off"};
        do_set_box = false;

      case "boxon"
        tmp_pval = {"box", "on"};
        do_set_box = false;

      case "hide"
        tmp_pval = {"visible", "off"};

      case "show"
        tmp_pval = {"visible", "on"};

      case "toggle"
        if (! isempty (hl))
          if (strcmp (get (hl, "visible"), "on"))
            tmp_pval = {"visible", "off"};
          else
            tmp_pval = {"visible", "on"};
          endif
        endif

      case "left"
          tmp_pval = {"textposition", "left"};

      case "right"
          tmp_pval = {"textposition", "right"};

      case "off"
        if (! isempty (hl))
          delete (hl);
        endif
        return;

    endswitch

    pval = [tmp_pval, pval];
    if (do_set_box)
      pval = [pval, "box", "on"];
    endif

  endif

  if (isempty (hl))

    hl = axes ("parent", get (opts.axes_handles(1), "parent"), ...
               "tag", "legend", "handlevisibility", "off", ...
               "ydir", "reverse", "position", [.5 .5 .3 .3], ...
               "fontsize", 0.9 * get (opts.axes_handles(1), "fontsize"), ...
               "clim", get (opts.axes_handles(1), "clim"), ...
               "colormap", get (opts.axes_handles(1), "colormap"), ...
               "xtick", [], "ytick", [], "box", "on");

    ## FIXME: Use the axes appdata to store its peer legend handle
    ## rather that adding a public property and change all uses.
    for htmp = opts.axes_handles
      try
        addproperty ("__legend_handle__", htmp, "handle", hl);
      catch
        set (htmp, "__legend_handle__", hl);
      end_try_catch
    endfor

    ## Add and update legend specific properties
    addproperties (hl);
    try
      set (hl, "string", opts.obj_labels, pval{:});
    catch ee
      delete (hl);
      set (opts.axes_handles, "__legend_handle__", []);
      rethrow (ee);
    end_try_catch

    ## Update legend layout
    setappdata (hl, "__axes_handle__", opts.axes_handles,
                    "__next_label_index__", opts.next_idx,
                    "__peer_objects__", opts.obj_handles);
    update_location_cb (hl, [], false);
    update_edgecolor_cb (hl);
    update_numchild_cb (hl);
    update_layout_cb (hl, [], true);

    ## Dummy invisible object that deletes the legend when "newplot" is called
    ht = __go_text__ (opts.axes_handles(1), "tag", "__legend_watcher__",
                      "visible", "off", "handlevisibility", "off",
                      "deletefcn", {@reset_cb, hl});

    ## Listeners to foreign objects properties are stored for later
    ## deletion in "reset_cb"
    hax = opts.axes_handles(1);
    hf = ancestor (hax, "figure");

    add_safe_listener (hl, hf, "colormap", ...
                       @(~, ~) set (hl, "colormap", get (hax, "colormap")));

    add_safe_listener (hl, hax, "position", {@maybe_update_layout_cb, hl});
    add_safe_listener (hl, hax, "tightinset", ...
                       @(h, ~) update_layout_cb (get (h, "__legend_handle__")));
    add_safe_listener (hl, hax, "clim", ...
                       @(hax, ~) set (hl, "clim", get (hax, "clim")));
    add_safe_listener (hl, hax, "colormap", ...
                       @(hax, ~) set (hl, "colormap", get (hax, "colormap")));
    add_safe_listener (hl, hax, "fontsize", ...
                       @(hax, ~) set (hl, "fontsize", 0.9*get (hax, "fontsize")));
    add_safe_listener (hl, hax, "children", {@legend_autoupdate_cb, hl});

    ## Listeners to legend properties
    props = {"fontsize", "fontweight", "fontname", "interpreter", ...
             "textposition", "numcolumnsmode", "numcolumns", "orientation"};

    for ii = 1:numel (props)
      addlistener (hl, props{ii}, {@update_layout_cb, true});
    endfor

    addlistener (hl, "autoupdate", @update_numchild_cb);

    addlistener (hl, "beingdeleted", @delete_legend_cb);

    addlistener (hl, "box", @update_box_cb);

    addlistener (hl, "edgecolor", @update_edgecolor_cb);

    addlistener (hl, "location", @update_location_cb);

    addlistener (hl, "position", @update_position_cb);

    addlistener (hl, "string", @update_string_cb);

    addlistener (hl, "textcolor", ...
                 @(h, ~) set (findobj (h, "type", "text"), ...
                              "color", get (hl, "textcolor")));

    addlistener (hl, "visible", @update_visible_cb);

  else

    ## FIXME: This will trigger the execution of update_layout_cb for each
    ## watched property.  Should we suspend its execution with yet another
    ## appdata bool property for performance?

    ## Update properties
    setappdata (hl, "__peer_objects__", opts.obj_handles);
    if (! isempty (opts.obj_labels))
      set (hl ,"string", opts.obj_labels, pval{:})
    elseif (! isempty (pval))
      set (hl, pval{:});
    endif

  endif

  if (nargout > 0)
    hleg = hl;
    ## These ones are needed for backward compatibility
    hleg_obj = get (hl, "children");
    hplot = getappdata (hl, "__peer_objects__");
    labels = get (hl, "string");
  endif

  set (hl, "handlevisibility", "on");

endfunction

function update_box_cb (hl, ~)

  if (strcmp (get (hl, "box"), "on"))
    if (strcmp (get (hl, "color"), "none"))
      set (hl, "color", "w");
    endif
  else
    set (hl, "color", "none");
  endif

endfunction

function update_location_cb (hl, ~, do_layout = true)

  if (strcmp (get (hl, "location"), "best"))
    warning ("Octave:legend:unimplemented-location",
             ["legend: 'best' not yet implemented for location ", ...
              "specifier, using 'northeast' instead\n"]);
  endif

  if (do_layout)
    update_layout_cb (hl);
  endif

endfunction

function update_edgecolor_cb (hl, ~)

  ecolor = get (hl, "edgecolor");
  set (hl, "xcolor", ecolor, "ycolor", ecolor);

endfunction

function update_position_cb (hl, ~)

  updating = getappdata (hl, "__updating_layout__");
  if (isempty (updating) || ! updating)
    if (! strcmp (get (hl, "location"), "none"))
      set (hl, "location", "none");
    else
      update_layout_cb (hl);
    endif
  endif

endfunction

function update_string_cb (hl, ~)

  ## Check that the number of legend item and the number of labels match
  ## before calling update_layout_cb.
  persistent updating = false;

  if (! updating)
    updating = true;
    unwind_protect
      str = get (hl, "string");
      nstr = numel (str);

      obj = getappdata (hl, "__peer_objects__");
      nobj = numel (obj);

      if (ischar (str) && nobj != 1)
        setappdata (hl, "__peer_objects__", obj(1));
      elseif (iscellstr (str) && nobj != nstr)
        if (nobj > nstr)
          setappdata (hl, "__peer_objects__", obj(1:nstr));
        elseif (nobj == 1)
          set (hl, "string", str{1});
        else
          set (hl, "string", str(1:nobj));
        endif
      endif
      update_layout_cb (hl, [], true);
    unwind_protect_cleanup
      updating = false;
    end_unwind_protect
  endif

endfunction

function update_visible_cb (hl, ~)

  location = get (hl, "location");
  if (strcmp (location(end:-1:end-3), "edis"))
    update_layout_cb (hl);
  endif

endfunction

function reset_cb (ht, ~, hl, deletelegend = true)

  if (ishghandle (hl))
    listeners = getappdata (hl, "__listeners__");
    for ii = 1:numel (listeners)
      if (ishghandle (listeners{ii}{1}))
        dellistener (listeners{ii}{:});
      endif
    endfor

    if (deletelegend)
      delete (hl);
    endif
  endif

endfunction

function delete_legend_cb (hl, ~)

  reset_cb ([], [], hl, false);

  hax = getappdata (hl, "__axes_handle__");
  for h = hax(:)'
    units = get (h, "units");
    set (h, "units", getappdata (hl, "__original_units__"), ...
            "looseinset", getappdata (hl, "__original_looseinset__"), ...
            "units", units, "__legend_handle__", []);
  endfor

endfunction

function add_safe_listener (hl, varargin)

  addlistener (varargin{:});

  lsn = getappdata (hl, "__listeners__");
  lsn = [lsn, {varargin}];
  setappdata (hl, "__listeners__", lsn);

endfunction

function addproperties (hl)

  persistent default = {"north", "northoutside", ...
                        "south", "southoutside", ...
                        "east", "eastoutside", ...
                        "west", "westoutside", ...
                        "{northeast}", "northeastoutside", ...
                        "northwest", "northwestoutside", ...
                        "southeast", "southeastoutside", ...
                        "southwest", "southwestoutside", ...
                        "best", "bestoutside", ...
                        "none"};

  addproperty ("location", hl, "radio", strjoin (default(:), "|"));

  addproperty ("orientation", hl, "radio", "{vertical}|horizontal");

  addproperty ("numcolumns", hl, "double", 1);

  addproperty ("numcolumnsmode", hl, "radio", "{auto}|manual");

  addlistener (hl, "numcolumns", @(h, ~) set (h, "numcolumnsmode", "manual"));

  addproperty ("autoupdate", hl, "radio", "{on}|off");

  addproperty ("string", hl, "textstring", {});

  addproperty ("interpreter", hl, "textinterpreter");

  addproperty ("edgecolor", hl, "color", [.15 .15 .15]);

  addproperty ("textcolor", hl, "color", "k");

  addproperty ("textposition", hl, "radio", "left|{right}");

  addproperty ("itemhitfcn", hl, "axesbuttondownfcn");

endfunction

function maybe_update_layout_cb (h, ~, hl)

  persistent updating = false;

  if (! updating)

    unwind_protect
      updating = true;
      units = get (h, "units");
      set (h, "units", "points");
      pos = get (h, "position");
      set (h, "units", units);
      old_pos = getappdata (hl, "__peer_axes_position__");

      if (! all (pos == old_pos))
        update_layout_cb (hl);
        setappdata (hl, "__peer_axes_position__", pos);
      endif
    unwind_protect_cleanup
      updating = false;
    end_unwind_protect

  endif

endfunction

function update_numchild_cb (hl, ~)

  if (strcmp (get (hl, "autoupdate"), "on"))

    hax = getappdata (hl, "__axes_handle__");
    kids = get (hax, "children");
    if (iscell (kids))
      nkids = numel (cell2mat (get (hax, "children")));
    else
      nkids = numel (get (hax, "children"));
    endif

    setappdata (hl, "__total_num_children__", nkids);

  endif

endfunction

function legend_autoupdate_cb (hax, ~, hl)

  ## Get all current children including eventual peer plotyy axes children
  try
    hax = get (hax, "__plotyy_axes__");
    kids = cell2mat (get (hax, "children"));
  catch
    kids = get (hax, "children");
  end_try_catch

  is_deletion = (getappdata (hl, "__total_num_children__") > numel (kids));
  setappdata (hl, "__total_num_children__", numel (kids));

  ## Remove item for deleted object
  current_obj = getappdata (hl, "__peer_objects__");
  [~, iold, inew] = setxor (current_obj, kids);
  current_obj(iold) = [];

  if (isempty (current_obj))
    delete (hl);
    return;
  endif

  if (! is_deletion && strcmp (get (hl, "autoupdate"), "on"))

    ## We only expect 1 new child
    kids = kids(min (inew));

    ## FIXME: if the latest child is an hggroup, we cannot label it since this
    ## function is called before the hggroup has been properly populated.
    persistent valid_types = {"line", "patch", "scatter", "surface"};
    if (! any (strcmp (get (kids, "type"), valid_types)))
      kids = [];
    endif

  else
    kids = [];
  endif

  if (any (iold) || any (kids))
    setappdata (hl, "__peer_objects__", [current_obj; kids]);
    set (hl, "string", displayname_or_default ([current_obj; kids], hl));
  endif

endfunction

function opts = parse_opts (varargin)

  action = "";
  legend_handle = [];
  axes_handles = [];
  obj_handles = [];
  obj_labels = {};

  nargs = numel (varargin);

  ## Find peer axes
  if (nargs > 0)
    if (! ishghandle (varargin{1}))
      [axes_handles, varargin, nargs] = __plt_get_axis_arg__ ("legend",
                                                              varargin{:});
    elseif (strcmp (get (varargin{1}, "type"), "axes"))
      if (strcmp (get (varargin{1}, "tag"), "legend"))
        legend_handle = varargin{1};
        varargin(1) = [];
        nargs--;
        axes_handles = getappdata (legend_handle, "__axes_handle__");
      else
        [axes_handles, varargin, nargs] = __plt_get_axis_arg__ ("legend",
                                                                varargin{:});
      endif
    endif
    if (isempty (axes_handles))
      axes_handles = gca ();
    endif
  else
    axes_handles = gca ();
  endif

  ## Special handling for plotyy which has two axes objects
  if (isprop (axes_handles, "__plotyy_axes__"))
    axes_handles = [axes_handles get(axes_handles, "__plotyy_axes__").'];
    ## Remove duplicates while preserving order
    [~, n] = unique (axes_handles, "first");
    axes_handles = axes_handles(sort (n));
  endif

  ## Find any existing legend object associated with axes
  if (isempty (legend_handle))
    try
      legend_handle = get (axes_handles, "__legend_handle__");
      if (iscell (legend_handle))
        legend_handle = unique (cell2mat (legend_handle));
      endif
    end_try_catch
  endif

  ## Legend actions
  actions = {"show", "hide", "toggle", "boxon", ...
             "boxoff", "right", "left", "off"};
  if (nargs > 0 && ischar (varargin{1})
      && any (strcmp (varargin{1}, actions)))
    action = varargin{1};
    if (nargs > 1)
      warning ("Octave:legend:ignoring-extra-argument",
               'legend: ignoring extra arguments after "%s"', action);
    endif
    nargs = 0;
  endif

  ## Now remove property-value pairs for compatibility.
  propval = {};
  warn_propval = "";
  persistent legend_props = {"location", "orientation", "numcolumns", ...
                             "numcolumnsmode", "textposition", ...
                             "position", "units", "autoupdate", ...
                             "string", "title", "interpreter", ...
                             "fontname", "fontsize", "fontweight", ...
                             "fontangle", "textcolor", "color", ...
                             "edgecolor", "box", "linewidth", ...
                             "visible", "uicontextmenu", "selected", ...
                             "selectionhighlight", "itemhitfcn", ...
                             "buttondownfcn", "createfcn", "deletefcn" ...
                             "interruptible", "busyaction", ...
                             "pickableparts", "hittest", ...
                             "beingdeleted", "parent", "children", ...
                             "handlevisibility", "tag", "type", ...
                             "userdata"};
  isprp = @(prop) (ischar (prop) && any (strcmpi (legend_props, prop)));
  idx = find (cellfun (isprp, varargin));
  if (! isempty (idx))
    idx = idx(1);
    propval = varargin(idx:end);
    warn_propval = varargin{idx};
    varargin(idx:end) = [];
    nargs = idx-1;
  endif

  ## List plot objects that can be handled
  warn_extra_obj = false;
  persistent valid_types = {"hggroup", "line", "patch", "scatter", "surface"};

  if (nargs > 0 && all (ishghandle (varargin{1})))

    ## List of plot objects to label given as first argument
    obj_handles = varargin{1};
    types = get (obj_handles, "type");
    if (! iscell (types))
      types = {types};
    endif

    idx = cellfun (@(s) any (strcmp (s, valid_types)), types);
    if (! all (idx))
      error ("Octave:legend:bad-object",
             "legend: objects of type \"%s\" can't be labeled",
             types(! idx){1});
    endif
    varargin(1) = [];
    nargs--;
    warn_extra_obj = true;

  elseif (nargs > 0 || isempty (legend_handle))

    ## Find list of plot objects from axes "children"
    if (isscalar (axes_handles))
      obj_handles = flipud (get (axes_handles, "children")(:));
    else
      tmp = get (axes_handles(:), "children");
      obj_handles = [flipud(tmp{1}); flipud(tmp{2})];
    endif

    if (isempty (obj_handles))
      error ("Octave:legend:no-object", "legend: no valid object to label");
    endif

    idx = arrayfun (@(h) any (strcmp (get (h, "type"), valid_types)), ...
                              obj_handles);
    obj_handles(! idx) = [];

    if (isempty (obj_handles))
      error ("Octave:legend:no-object", "legend: no valid object to label");
    endif

  else
    obj_handles = getappdata (legend_handle, "__peer_objects__");
  endif

  nobj = numel (obj_handles);

  ## List labels
  next_idx = 1;
  if (nargs > 0)

    if (iscellstr (varargin{1}))
      obj_labels = varargin{1};
      varargin(1) = [];
      nargs--;
    elseif (ischar (varargin{1}) && rows (varargin{1}) > 1)
      obj_labels = cellstr (varargin{1});
      varargin(1) = [];
      nargs--;
    elseif (all (cellfun (@ischar, varargin)))
      obj_labels = varargin;
      varargin = {};
      nargs = 0;
    endif

    if (nargs > 0)
      print_usage ("legend");
    endif

    nlab = numel (obj_labels);
    if (nlab != nobj)
      if (nobj > nlab)
        obj_handles = obj_handles(1:nlab);

        msg = "legend: ignoring extra objects.";
        if (! isempty (warn_propval))
          msg = [msg ' "' warn_propval '" interpreted as a property ' , ...
                 "name. Use a cell array of strings to specify labels ", ...
                 "that match a legend property name."];
        endif
        if (warn_extra_obj)
          warning ("Octave:legend:object-label-mismatch", msg);
        endif
      else
        obj_labels = obj_labels(1:nobj);
        warning ("Octave:legend:object-label-mismatch",
                 "legend: ignoring extra labels.");
      endif
    endif
  else
    [tmp_labels, next_idx] = displayname_or_default (obj_handles,
                                                     legend_handle);
    if (isempty (legend_handle)
        || ! isequal (tmp_labels, get (legend_handle, "string")))
      obj_labels = tmp_labels;
    endif

  endif

  opts.action = action;
  opts.axes_handles = axes_handles;
  opts.obj_handles = obj_handles;
  opts.obj_labels = obj_labels;
  opts.legend_handle = legend_handle;
  opts.propval = propval;
  opts.next_idx = next_idx;

endfunction

function [labels, next_idx] = displayname_or_default (hplots, hl = [])

  next_idx = 1;
  if (! isempty (hl))
    next_idx = getappdata (hl, "__next_label_index__");
  endif

  ## Use the displayname property
  labels = get (hplots, "displayname");
  if (! iscell (labels))
    labels = {labels};
  endif

  ## Fallback to automatic names for empty labels
  empty_label_idx = cellfun (@isempty, labels);

  if (any (empty_label_idx) && ! isempty (hl))
    ## Empty strings must not be blindly replaced by data%d. If there exist
    ## an old text object that was affected an empty string, keep it as is.
    kids = get (hl, "children");
    htext = kids(strcmp (get (kids, "type"), "text"));
    old_objects = get (htext, "peer_object");
    if (iscell (old_objects))
      old_objects = cell2mat (old_objects);
    endif

    for h = hplots(empty_label_idx).'
      idx = (h == old_objects);
      if (any (idx))
        labels(hplots == h) = get (htext(idx), "string");
        empty_label_idx(hplots == h) = false;
      endif
    endfor

  endif

  if (any (empty_label_idx))
    default = arrayfun (@(ii) sprintf ("data%d", ii), ...
                        [next_idx:(next_idx + sum (empty_label_idx) - 1)], ...
                        "uniformoutput", false)(:);
    labels(empty_label_idx) = default;
  endif

  next_idx += sum (empty_label_idx);

  if (! isempty (hl))
    setappdata (hl, "__next_label_index__", next_idx);
  endif

endfunction

function update_layout_cb (hl, ~, update_item = false)

  updating = getappdata (hl, "__updating_layout__");
  if (! isempty (updating) && updating)
    return;
  endif

  setappdata (hl, "__updating_layout__", true);

  ## Scale limits so that item positions are expressed in points, from
  ## top to bottom and from left to right or reverse depending on textposition
  units = get (hl, "units");
  set (hl, "units", "points");

  unwind_protect

    pos = get (hl, "position");

    if (update_item)
      set (hl, "xlim",  [0, pos(3)], "ylim",  [0, pos(4)]);
      textright = strcmp (get (hl, "textposition"), "right");
      set (hl, "ydir", "reverse", ...
               "xdir", ifelse (textright, "normal", "reverse"));

      ## Create or reuse text and icon graphics objects
      objlist = texticon_objects (hl, textright);
      nitem = rows (objlist);

      ## Prepare the array of text/icon pairs and update their position
      sz = update_texticon_position (hl, objlist);
    else
      sz = getappdata (hl, "__item_bouding_box__");
    endif

    ## Place the legend
    if (! strcmp (get (hl, "location"), "none"))
      update_legend_position (hl, sz);
    else
      ## Custom location: Adapt width and height if necessary.
      [x0, y0, x1, y1] = deal (0, 0, sz(1), sz(2));

      if (sz(1) > pos(3))
        pos(3) = sz(1);
      else
        dx = pos(3)-sz(1);
        x0 -= dx/2;
        x1 += dx/2;
      endif

      if (sz(2) > pos(4))
        pos(4) = sz(2);
      else
        dy = pos(4)-sz(2);
        y0 -= dy/2;
        y1 += dy/2;
      endif

      set (hl, "position",  pos, "xlim", [x0 x1], "ylim", [y0 y1]);
    endif

  unwind_protect_cleanup
    set (hl, "units", units);
    setappdata (hl, "__updating_layout__", false);
  end_unwind_protect

endfunction

function objlist = texticon_objects (hl, textright)

  ## Delete or set invisible obsolete or unused text/icon objects.
  old_kids = get (hl, "children")(:).';
  old_peer_objects = cell2mat (get (old_kids, "peer_object"))(:).';
  unused = ! ishghandle (old_peer_objects);
  delete (old_kids(unused));
  old_kids(unused) = [];
  old_peer_objects(unused) = [];

  new_peer_objects = getappdata (hl, "__peer_objects__")(:).';

  unused = arrayfun (@(h) ! any (h == new_peer_objects), old_peer_objects);
  set (old_kids(unused), "visible", "off");

  ## Text properties
  string = get (hl , "string");
  if (! iscell (string))
    string = {string};
  endif

  txtprops = {"textcolor", "fontsize", "fontweight", "fontname", ...
              "interpreter"};
  txtvals = get (hl, txtprops);
  txtprops{1} = "color";
  txtprops = [txtprops, "horizontalalignment"];
  txtvals = [txtvals, ifelse(textright, "left", "right")];

  ## Create or reuse text/icon objects as needed
  nitem = numel (new_peer_objects);
  objlist = NaN (nitem, 2);

  for ii = 1:nitem

    str = string{ii};
    hplt = new_peer_objects(ii);

    idx = (old_peer_objects == hplt);

    if (any (idx))
      tmp = old_kids(idx);
      idx = strcmp (get (tmp, "type"), "text");

      htxt = tmp(idx);
      hicon = tmp(! idx);

      set (htxt, "visible", "on", "string", str, ...
                 [txtprops(:)'; txtvals(:)']{:});
      set (hicon, "visible", "on");

    else
      [htxt, hicon] = create_item (hl, str, [txtprops(:)'; txtvals(:)'], hplt);
      add_safe_listener (hl, hplt, "displayname", {@update_displayname_cb, hl});
    endif

    set (hplt, "displayname", str);

    objlist(ii,:) = [htxt, hicon];
  endfor

endfunction

function [htxt, hicon] = create_item (hl, str, txtpval, hplt)

  typ = get (hplt, "type");

  ## For unknown hggroups use the first child that can be labeled
  persistent known_creators = {"__contour__", "__errplot__", "__quiver__", ...
                               "__stem__"};
  base_hplt = hplt;

  if (strcmp (typ, "hggroup"))
    creator = getappdata (hplt, "__creator__");
    kids = get (hplt, "children");
    if (any (strcmp (known_creators, creator)))
      typ = creator;
      switch (creator)
        case "__contour__"
          ## Eliminate eventual text objects created by clabel
          kids(strcmp (get (kids, "type"), "text")) = [];
          hplt = [kids(end), kids(1)];
        case {"__errplot__", "__quiver__", "__stem__"}
          hplt = kids(2:-1:1);
        otherwise
          hplt = kids(1);
      endswitch
    else
      types = get (kids, "type");
      if (! iscell (types))
        types = {types};
      endif

      idx = cellfun (@(s) any (strcmp (s, {"line", "patch", "surface"})), ...
                     types);
      hplt = kids(idx)(1);
      typ = types(idx){1};
    endif
  endif

  persistent lprops = {"color", "linestyle", "linewidth"};
  persistent mprops = {"color", "marker", "markeredgecolor", ...
                       "markerfacecolor", "markersize"};
  persistent pprops = {"edgecolor", "facecolor", "cdata", ...
                       "linestyle", "linewidth", ...
                       "marker", "markeredgecolor", ...
                       "markerfacecolor", "markersize"};
  persistent sprops = {"marker", "markeredgecolor", ...
                       "markerfacecolor"};

  switch (typ)
    case {"line", "__errplot__", "__quiver__", "__stem__"}

      ## Main line
      vals = get (hplt(1), lprops);
      hicon = __go_line__ (hl, [lprops; vals]{:}, ...
                           "pickableparts", "all", ...
                           "buttondownfcn", ...
                           {@execute_itemhit, hl, hplt, "icon"});

      addproperty ("markerxdata", hicon, "double", 0);
      addproperty ("markerydata", hicon, "double", 0);

      ## Additional line for the marker
      vals = get (hplt(end), mprops);
      hmarker = __go_line__ (hl, "handlevisibility", "off", ...
                             "xdata", 0, "ydata", 0, [mprops; vals]{:}, ...
                             "pickableparts", "all", ...
                             "buttondownfcn", ...
                             {@execute_itemhit, hl, hplt, "icon"});
      addproperty ("markertruesize", hmarker, "double", NaN);
      update_marker_cb (hmarker);

      ## Listeners
      safe_property_link (hplt(1), hicon, lprops);
      safe_property_link (hplt(end), hmarker, mprops);
      addlistener (hicon, "ydata", ...
                   @(h, ~) set (hmarker, "ydata", get (h, "markerydata")));
      addlistener (hicon, "xdata", ...
                   @(h, ~) set (hmarker, "xdata", get (h, "markerxdata")));
      addlistener (hicon, "visible", ...
                   @(h, ~) set (hmarker, "visible", get (h, "visible")));
      addlistener (hmarker, "markersize", {@update_marker_cb, true});
      addlistener (hmarker, "marker", {@update_marker_cb, false});
      add_safe_listener (hl, hplt(1), "beingdeleted",
                         @(~, ~) delete ([hicon hmarker]))
      if (! strcmp (typ, "__errplot__"))
        setappdata (hicon, "__creator__", typ);
      else
        setappdata (hicon, "__creator__", typ, ...
                    "__format__", get (base_hplt, "format"));
      endif

    case {"patch", "surface"}

      vals = get (hplt, pprops);

      hicon = __go_patch__ (hl, [pprops; vals]{:});

      ## Listeners
      safe_property_link (hplt(1), hicon, pprops);

      setappdata (hicon, "__creator__", typ);

    case "scatter"

      all_sprops = [sprops, "sizedata", "cdata"];

      vals = get (hplt, all_sprops);

      ## sizedata and cdata may be N-by-1 vectors or N-by-3 (RGB) martrices.
      ## Take the average for the icon.
      vals {end-1} = mean (vals {end-1}, 1);
      vals {end} = mean (vals {end}, 1);

      hicon = __go_scatter__ (hl, [all_sprops; vals]{:}, ...
                              "pickableparts", "all", ...
                              "buttondownfcn", ...
                              {@execute_itemhit, hl, hplt, "icon"});

      ## Simple Listeners
      safe_property_link (hplt(1), hicon, sprops);

      ## Listener to sizedata
      lsn = {hplt(1), "sizedata", @(h, ~) set (hicon, "sizedata", ...
                                               mean (get (h, "sizedata")))};
      addlistener (lsn{:});
      addlistener (hicon, "beingdeleted", @(~, ~) dellistener (lsn{:}));

      ## Listener to cdata
      lsn = {hplt(1), "cdata", @(h, ~) set (hicon, "cdata", ...
                                            mean (get (h, "cdata"), 1))};
      addlistener (lsn{:});
      addlistener (hicon, "beingdeleted", @(~, ~) dellistener (lsn{:}));

      setappdata (hicon, "__creator__", typ);

    case "__contour__"

      ## Main patch

      vals = get (hplt(1), pprops);
      hicon = __go_patch__ (hl, [pprops; vals]{:}, ...
                            "pickableparts", "all", ...
                            "buttondownfcn", ...
                            {@execute_itemhit, hl, hplt, "icon"});

      addproperty ("innerxdata", hicon, "any", 0);
      addproperty ("innerydata", hicon, "any", 0);

      ## Additional patch for the inner contour
      vals = get (hplt(end), pprops);
      htmp =  __go_patch__ (hl, "handlevisibility", "off", ...
                            "xdata", 0, "ydata", 0, [pprops; vals]{:}, ...
                            "pickableparts", "all", ...
                            "buttondownfcn", ...
                            {@execute_itemhit, hl, hplt, "icon"});

      ## Listeners
      safe_property_link (hplt(1), hicon, pprops);
      safe_property_link (hplt(end), htmp, pprops);
      addlistener (hicon, "ydata", ...
                   @(h, ~) set (htmp, "ydata", get (h, "innerydata")));
      addlistener (hicon, "xdata", ...
                   @(h, ~) set (htmp, "xdata", get (h, "innerxdata")));
      addlistener (hicon, "visible", ...
                   @(h, ~) set (htmp, "visible", get (h, "visible")));
      add_safe_listener (hl, hplt(1), "beingdeleted",
                         @(~, ~) delete ([hicon htmp]))

      setappdata (hicon, "__creator__", typ);

  endswitch

  htxt = __go_text__ (hl, txtpval{:}, "string", str, ...
                      "pickableparts", "all", ...
                      "buttondownfcn", {@execute_itemhit, hl, hplt, "label"});

  set (htxt, "buttondownfcn", {@execute_itemhit, hl, hplt, "label"});

  addproperty ("peer_object", htxt, "double", base_hplt);
  addproperty ("peer_object", hicon, "double", base_hplt);

endfunction

function execute_itemhit (h, ~, hl, hplt, region)

  fcn = get (hl, "itemhitfcn");

  if (! isempty (fcn))
    evt = struct ("Peer", hplt, "Region", region, ...
                  "SelectionType", get (gcbf (), "selectiontype"), ...
                  "Source", hl, "EventName", "ItemHit");
    fcn (hl, evt)
  endif

endfunction

function safe_property_link (h1, h2, props)

  for ii = 1:numel (props)
    prop = props{ii};
    lsn = {h1, prop, @(h, ~) set (h2, prop, get (h, prop))};
    addlistener (lsn{:});
    addlistener (h2, "beingdeleted", @(~, ~) dellistener (lsn{:}));
  endfor

endfunction

function update_displayname_cb (h, ~, hl)

  updating = getappdata (hl, "__updating_layout__");
  if (! isempty (updating) && updating)
    return;
  endif

  str = get (hl, "string");
  if (! iscell (str))
    str = {str};
  endif

  idx = h == getappdata (hl, "__peer_objects__");
  if (any (idx))
    str{idx} = get (h, "displayname");
    set (hl ,"string", str);
  endif

endfunction

## Enforce maximum size of marker so it doesn't overflow legend key
function update_marker_cb (h, ~, sz_updated = true)
  persistent is_updating = false;

  if (is_updating)
    return;
  endif

  if (sz_updated)
    ## Size was changed
    sz = get (h, "markersize");
    set (h, "markertruesize", sz);  # store true marker size

    if (sz > 8)
      is_updating = true;

      mark = get (h, "marker");
      if (strcmp (mark, '.'))
        set (h, "markersize", min ([sz, 24]));
      else
        set (h, "markersize", 8);
      endif

      is_updating = false;
    endif

  else
    ## Marker style was changed
    sz = get (h, "markertruesize");
    if (sz > 8)
      is_updating = true;

      mark = get (h, "marker");
      if (strcmp (mark, '.'))
        set (h, "markersize", min ([sz, 24]));
      else
        set (h, "markersize", 8);
      endif

      is_updating = false;
    endif
  endif

endfunction

function sz = update_texticon_position (hl, objlist)

  ## margins in points
  persistent hmargin = 3;
  persistent vmargin = 3;
  persistent icon_width = 15;

  units = get (hl, "fontunits");
  set (hl, "fontunits", "points");
  icon_height = 0.7 * get (hl, "fontsize");
  set (hl, "fontunits", units);

  types = get (objlist(:,2), "type");
  ext = get (objlist(:,1), "extent");
  markers = get (objlist(:,2), "marker");

  is_scatter = strcmp (types, "scatter");
  if (! any (is_scatter))
    markersz = get (objlist(:,2), "markersize");
  elseif (rows (objlist) == 1)
    markersz = mean (get (objlist(1,2), "sizedata").^0.5);
  else
    markersz = cell (rows (objlist), 1);
    for ii = 1:rows (objlist)
      if (! is_scatter(ii))
        markersz{ii} = get (objlist(ii,2), "markersize");
      else
        markersz{ii} = mean (get (objlist(ii,2), "sizedata").^0.5);
      endif
    endfor
  endif

  ## Simple case of 1 text/icon pair
  nitem = rows (objlist);
  txticon = zeros (nitem, 4);
  if (nitem == 1)
    ext = abs (ext(:,3:4));
    types = {types};
    markers = {markers};
    markersz = {markersz};
  else
    ext = abs (cell2mat (ext)(:,3:4));
  endif

  autolayout = strcmp (get (hl, "numcolumnsmode"), "auto");
  xmax = ymax = 0;
  iter = 1;

  if (strcmp (get (hl, "orientation"), "vertical"))

    if (autolayout)
      ncol = 1;
    else
      ncol = min (nitem, get (hl, "numcolumns"));
    endif

    nrow = ceil (nitem / ncol);

    rowheights = arrayfun (@(idx) max([icon_height;
                                       ext(idx:nrow:end, 2);
                                       vertcat(markersz(idx:nrow:end){:})]), ...
                           1:nrow);
    x = hmargin;
    for ii = 1:ncol
      y = vmargin;
      for jj = 1:nrow
        if (iter > nitem)
          continue;
        endif

        hg = rowheights(jj);
        dx = 0;
        if (! strcmp (markers{iter}, "none"))
          dx = markersz{iter}/2;
        endif

        ybase = y + hg / 2;
        y0 = ybase - max (icon_height, dx)/2 + dx;
        y1 = ybase + max (icon_height, dx)/2 - dx;

        update_icon_position (objlist(iter,2), [x+dx, x+icon_width-dx], ...
                              [y0, y1]);

        set (objlist(iter,1), "position", [x+icon_width+hmargin, ybase, 0]);

        xmax = max ([xmax, x+icon_width+2*hmargin+ext(iter,1)]);
        y += (vmargin + hg);
        iter++;
      endfor
      ymax = max ([ymax, y]);
      x = xmax + 2*hmargin;
    endfor

  else

    if (autolayout)
      ncol = nitem;
    else
      ncol = min (nitem, get (hl, "numcolumns"));
    endif

    nrow = ceil (nitem / ncol);

    colwidth = arrayfun (@(idx) max (ext(idx:ncol:end, 1)),
                         1:ncol);
    y = vmargin;
    for ii = 1:nrow
      x = hmargin;

      endidx = min (iter+ncol-1, nitem);
      hg = max ([icon_height; ext(iter:endidx,2); ...
                 vertcat(markersz{:})]);

      for jj = 1:ncol
        if (iter > nitem)
          continue;
        endif

        wd = colwidth(jj);

        dx = 0;
        if (! strcmp (markers{iter}, "none"))
          dx = markersz{iter}/2;
        endif

        ybase = y + hg / 2;
        y0 = ybase - max (icon_height, dx)/2 + dx;
        y1 = ybase + max (icon_height, dx)/2 - dx;

        update_icon_position (objlist(iter,2), [x+dx, x+icon_width-dx], ...
                              [y0, y1]);

        set (objlist(iter,1), "position", [x+icon_width+hmargin, ybase, 0]);

        ymax = max ([ymax, ybase+hg/2+vmargin]);
        x += (3*hmargin + icon_width + wd);
        iter++;
      endfor
      xmax = max ([xmax, x-hmargin]);
      y = ymax + vmargin;
    endfor

  endif

  sz = [xmax, ymax];
  setappdata (hl, "__item_bouding_box__", sz);

endfunction

function update_icon_position (hicon, xdata, ydata)

  creator = getappdata (hicon, "__creator__");
  switch (creator)
    case "line"
      set (hicon, "markerxdata", mean (xdata), "markerydata", mean (ydata), ...
           "xdata", xdata, "ydata", [mean(ydata), mean(ydata)]);
    case {"patch", "surface"}
      set (hicon, ...
           "xdata", [xdata, fliplr(xdata)], ...
           "ydata", [ydata; ydata](:).');
    case "__contour__"
      ## Draw two patches
      x0 = xdata(1);
      x1 = xdata(2);
      xm = mean (xdata);
      y0 = ydata(1);
      y1 = ydata(2);
      ym = mean (ydata);

      xdata = [x0, x1, x1, x0];
      ydata = [y0, y0, y1, y1];
      set (hicon, ...
           "innerxdata", (xdata-xm) * 0.6 + xm, ...
           "innerydata", (ydata-ym) * 0.4 + ym, ...
           "xdata", xdata, "ydata", ydata);
    case "__errplot__"
      x0 = xdata(1);
      x1 = xdata(2);
      xm = mean (xdata);
      y0 = ydata(1);
      y1 = ydata(2);
      ym = mean (ydata);

      fmt = getappdata (hicon, "__format__");
      if (strcmp (fmt, "yerr"))
        xdata = [xm, xm, xm-2, xm+2, xm, xm, xm-2, xm+2];
        ydata = [ym, y0, y0, y0, y0, y1, y1, y1];
      elseif (strcmp (fmt, "xerr"))
        xdata = [x0+2, x0+2, x0+2, x1-2, x1-2, x1-2, x1-2];
        ydata = [ym+2, ym-2, ym, ym, ym+2, ym-2, ym];
      elseif (strcmp (fmt, "xyerr"))
        xdata = [x0+2, x0+2, x0+2, x1-2, x1-2, x1-2, x1-2, ...
                 xm, xm, xm-2, xm+2, xm, xm, xm-2, xm+2];
        ydata = [ym+2, ym-2, ym, ym, ym+2, ym-2, ym, ...
                 ym, y0, y0, y0, y0, y1, y1, y1];
      elseif (strncmp (fmt, "box", 3))
        xdata = [x0+2, x1-2, x1-2, x0+2, x0+2];
        ydata = [y0, y0, y1, y1, y0];
      else
        xdata = [x0, x1];
        ydata = [ym, ym];
      endif

      set (hicon, "markerxdata", xm, "markerydata", ym, ...
           "xdata", xdata, "ydata", ydata);

    case "__quiver__"
      ## Draw an arrow
      x0 = xdata(1);
      x1 = xdata(2);
      y0 = mean (ydata);
      xdata = [x0, x1, x1-2, x1, x1-2];
      ydata = [y0, y0, y0+2, y0, y0-2];
      set (hicon, "markerxdata", x0, "markerydata", y0, ...
           "xdata", xdata, "ydata", ydata);
    case "scatter"
      set (hicon, "xdata", mean (xdata), "ydata", mean (ydata));
    case "__stem__"
      xdata(2) -= (get (get (hicon, "peer_object"), "markersize") / 2);
      set (hicon, "markerxdata", xdata(2), "markerydata", mean (ydata), ...
           "xdata", xdata, "ydata", [mean(ydata), mean(ydata)]);
  endswitch

endfunction

function pos = boxposition (axpos, pba, pbam, dam)

  pos = axpos;

  if (strcmp (pbam, "auto") && strcmp (dam, "auto"))
    return;
  endif

  pbratio = pba(1)/pba(2);
  posratio = axpos(3)/axpos(4);

  if (pbratio != posratio)
    if (posratio < pbratio)
      pos(4) = pos(3) / pbratio;
      pos(2) += (axpos(4) - pos(4)) / 2;
    else
      pos(3) = pos(4) * pbratio;
      pos(1) += (axpos(3) - pos(3)) / 2;
    endif
  endif

endfunction

function update_legend_position (hl, sz)

  persistent hmargin = 6;
  persistent vmargin = 6;

  location = get (hl, "location");
  outside = strcmp (location(end-3:end), "side");
  if (outside)
    location = location(1:end-7);
  endif

  if (strcmp (location, "best"))
    orientation = get (hl, "orientation");
    if (outside)
      if (strcmp (orientation, "vertical"))
        location = "northeast";
      else
        location = "south";
      endif
    else
      ## FIXME: implement "best" inside properly
      location = "northeast";
    endif
  endif

  haxes = getappdata (hl, "__axes_handle__");
  hax = haxes(end);
  units = get (hax, "units");

  unwind_protect
    ## Restore the original looseinset first and set units to points.
    li = getappdata (hl, "__original_looseinset__");
    if (isempty (li))
      li = get (hax, "looseinset");
      setappdata (hl, "__original_looseinset__", li);
      setappdata (hl, "__original_units__", units);
    endif

    if (strcmp (get (hl, "visible"), "on"))
      set (hax, "units", getappdata (hl, "__original_units__"),
                "looseinset", li,
                "units", "points");
    else
      ## Return early for invible legends
      set (hax, "units", getappdata (hl, "__original_units__"),
                "looseinset", li,
                "units", units);
      return;
    endif

    [li, axpos, pbam, pba, dam] = get (hax, {"looseinset", "position", ...
                                             "plotboxaspectratiomode", ...
                                             "plotboxaspectratio", ...
                                             "dataaspectratiomode"}){:};

    axpos = boxposition (axpos, pba, pbam, dam);

    lpos = [get(hl, "position")(1:2), sz];

    if (! outside)

      switch (location)
        case "southwest"
          lpos(1) = axpos(1) + hmargin;
          lpos(2) = axpos(2) + vmargin;
        case "west"
          lpos(1) = axpos(1) + hmargin;
          lpos(2) = axpos(2) + axpos(4)/2 - lpos(4)/2;
        case "northwest"
          lpos(1) = axpos(1) + hmargin;
          lpos(2) = axpos(2) + axpos(4) - lpos(4) - vmargin;
        case "north"
          lpos(1) = axpos(1) + axpos(3)/2 - lpos(3)/2;
          lpos(2) = axpos(2) + axpos(4) - lpos(4) - vmargin;
        case "northeast"
          lpos(1) = axpos(1) + axpos(3) - lpos(3) - hmargin;
          lpos(2) = axpos(2) + axpos(4) - lpos(4) - vmargin;
        case "east"
          lpos(1) = axpos(1) + axpos(3) - lpos(3) - hmargin;
          lpos(2) = axpos(2) + axpos(4)/2 - lpos(4)/2;
        case "southeast"
          lpos(1) = axpos(1) + axpos(3) - lpos(3) - hmargin;
          lpos(2) = axpos(2) + vmargin;
        case "south"
          lpos(1) = axpos(1) + axpos(3)/2 - lpos(3)/2;
          lpos(2) = axpos(2) + vmargin;
      endswitch

    else

      ## FIXME: Is there a simpler way to know the size of the box
      ##        enclosing labels than temporarily changing the
      ##        plotboxaspectratiomode
      if (strcmp (pbam, "auto"))
        ti = get (haxes, "tightinset");
      else
        set (haxes, "plotboxaspectratiomode", "auto");
        ti = get (haxes, "tightinset");
        set (haxes, "plotboxaspectratio", pba);
      endif

      if (iscell (ti))
        ti = max (cell2mat (ti));
      endif

      switch (location)
        case "southwest"
          dx = lpos(3) + hmargin + ti(1);
          if (axpos(1) < (dx + hmargin))
            li(1) = min (dx + hmargin, 0.95 * (axpos(1) + axpos(3)));
            set (hax, "looseinset", li);
            axpos = boxposition (get (hax, "position"), pba, pbam, dam);
          endif
          lpos(1) = axpos(1) - dx;
          lpos(2) = axpos(2);
        case "west"
          dx = lpos(3) + hmargin + ti(1);
          if (axpos(1) < (dx + hmargin))
            li(1) = min (dx + hmargin, 0.95 * (axpos(1) + axpos(3)));
            set (hax, "looseinset", li);
            axpos = boxposition (get (hax, "position"), pba, pbam, dam);
          endif
          lpos(1) = axpos(1) - dx;
          lpos(2) = axpos(2) + axpos(4)/2 - lpos(4)/2;
        case "northwest"
          dx = lpos(3) + hmargin + ti(1);
          if (axpos(1) < (dx + hmargin))
            li(1) = min (dx + hmargin, 0.95 * (axpos(1) + axpos(3)));
            set (hax, "looseinset", li);
            axpos = boxposition (get (hax, "position"), pba, pbam, dam);
          endif
          lpos(1) = axpos(1) - dx;
          lpos(2) = axpos(2) + axpos(4) - lpos(4);
        case "north"
          dy = lpos(4) + vmargin + ti(4);
          if (li(4) < (dy + vmargin))
            li(4) = min (dy + vmargin, axpos(4));
            set (hax, "looseinset", li);
            axpos = boxposition (get (hax, "position"), pba, pbam, dam);
          endif
          lpos(1) = axpos(1) + axpos(3)/2 - lpos(3)/2;
          lpos(2) = axpos(2) + axpos(4) + vmargin + ti(4);
        case "northeast"
          dx = lpos(3) + hmargin + ti(3);
          if (li(3) < (dx + hmargin))
            li(3) = min (dx + hmargin, axpos(3));
            set (hax, "looseinset", li);
            axpos = boxposition (get (hax, "position"), pba, pbam, dam);
          endif
          lpos(1) = axpos(1) + axpos(3) + hmargin + ti(3);
          lpos(2) = axpos(2) + axpos(4) - lpos(4);
        case "east"
          dx = lpos(3) + hmargin + ti(3);
          if (li(3) < (dx + hmargin))
            li(3) = min (dx + hmargin, axpos(3));
            set (hax, "looseinset", li);
            axpos = boxposition (get (hax, "position"), pba, pbam, dam);
          endif
          lpos(1) = axpos(1) + axpos(3) + hmargin + ti(3);
          lpos(2) = axpos(2) + axpos(4)/2 - lpos(4)/2;
        case "southeast"
          dx = lpos(3) + hmargin + ti(3);
          if (li(3) < (dx + hmargin))
            li(3) = min (dx + hmargin, axpos(3));
            set (hax, "looseinset", li);
            axpos = boxposition (get (hax, "position"), pba, pbam, dam);
          endif
          lpos(1) = axpos(1) + axpos(3) + hmargin + ti(3);
          lpos(2) = axpos(2);
        case "south"
          dy = lpos(4) + vmargin + ti(2);
          if (li(2) < (dy + vmargin))
            li(2) = min (dy + vmargin, 0.95 * (axpos(2) + axpos(4)));
            set (hax, "looseinset", li);
            axpos = boxposition (get (hax, "position"), pba, pbam, dam);
          endif
          lpos(1) = axpos(1) + axpos(3)/2 - lpos(3)/2;
          lpos(2) = axpos(2) - lpos(4) - vmargin - ti(2);
      endswitch
    endif

    set (hl, "xlim", [0, sz(1)], "ylim", [0, sz(2)], ...
             "position", lpos);

    setappdata (hl, "__peer_axes_position__", axpos);

  unwind_protect_cleanup
    set (hax, "units", units);
  end_unwind_protect

endfunction


%!demo
%! clf;
%! plot (rand (2));
%! title ("legend called with string inputs for labels");
%! h = legend ("foo", "bar");
%! set (h, "fontsize", 20, "location", "northeastoutside");

%!demo
%! clf;
%! plot (rand (2));
%! title ("legend called with cell array of strings");
%! h = legend ({"cellfoo", "cellbar"});
%! set (h, "fontsize", 20, "location", "northeast");

%!demo
%! clf;
%! plot (rand (3));
%! title ("legend () without inputs creates default labels");
%! h = legend ();

%!demo
%! clf;
%! x = 0:1;
%! hline = plot (x,x,";I am Blue;", x,2*x, x,3*x,";I am yellow;");
%! h = legend ();
%! set (h, "location", "northeastoutside");
%! ## Placing legend inside returns axes to original size
%! set (h, "location", "northeast");
%! title ("Blue and Yellow keys, with Orange missing");

%!demo
%! clf;
%! plot (1:10, 1:10, 1:10, fliplr (1:10));
%! title ("incline is blue and decline is orange");
%! legend ({"I am blue", "I am orange"}, "location", "east");
%! legend hide
%! legend show

%!demo
%! clf;
%! plot (1:10, 1:10, 1:10, fliplr (1:10));
%! title ("Legend with keys in horizontal orientation");
%! legend ({"I am blue", "I am orange"}, ...
%!         "location", "east", "orientation", "horizontal");
%! legend boxoff
%! legend boxon

%!demo
%! clf;
%! plot (1:10, 1:10, 1:10, fliplr (1:10));
%! title ("Legend with box off");
%! legend ({"I am blue", "I am orange"}, "location", "east");
%! legend boxoff

%!demo
%! clf;
%! plot (1:10, 1:10, 1:10, fliplr (1:10));
%! title ("Legend with text to the left of key");
%! legend ({"I am blue", "I am orange"}, "location", "east");
%! legend left

%!demo
%! clf;
%! plot (1:10, 1:10, 1:10, fliplr (1:10));
%! title ({"Use properties to place legend text to the left of key", ...
%!         "Legend text color is magenta"});
%! h = legend ({"I am blue", "I am orange"}, "location", "east");
%! legend ("right");
%! set (h, "textposition", "left");
%! set (h, "textcolor", [1, 0, 1]);

%!demo
%! clf;
%! plot (1:10, 1:10, 1:10, fliplr (1:10));
%! title ("Legend is hidden");
%! legend ({"I am blue", "I am orange"}, "location", "east");
%! legend hide

%!demo
%! clf;
%! x = 0:1;
%! plot (x,x,";I am Blue;", x,2*x,";I am Orange;", x,3*x,";I am Yellow;");
%! title ({"Labels are embedded in call to plot", ...
%!         "Legend is hidden and then shown"});
%! legend boxon
%! legend hide
%! legend show

%!demo
%! clf;
%! x = 0:1;
%! plot (x,x,  x,2*x, x,3*x);
%! title ("Labels with interpreted Greek text");
%! h = legend ('\alpha', '\beta=2\alpha', '\gamma=3\alpha');
%! set (h, "interpreter", "tex");

%!demo
%! clf;
%! plot (rand (2));
%! title ("Labels with TeX interpreter turned off");
%! h = legend ("Hello_World", "foo^bar");
%! set (h, "interpreter", "none");

%!demo
%! clf;
%! labels = {};
%! colororder = get (gca, "colororder");
%! for i = 1:5
%!   h = plot (1:100, i + rand (100,1)); hold on;
%!   set (h, "color", colororder(i,:));
%!   labels = {labels{:}, ["Signal ", num2str(i)]};
%! endfor
%! hold off;
%! title ({"Signals with random offset and uniform noise";
%!         "Legend shown below and outside of plot"});
%! xlabel ("Sample Nr [k]"); ylabel ("Amplitude [V]");
%! legend (labels, "location", "southoutside");

%!demo
%! clf;
%! x = linspace (0, 10);
%! plot (x, x);
%! hold on;
%! stem (x, x.^2, "g");
%! title ("First created object gets first label");
%! legend ("linear");
%! hold off;

%!demo
%! clf;
%! x = linspace (0, 10);
%! plot (x, x, x, x.^2);
%! title ("First created object gets first label");
%! legend ("linear");

%!demo
%! clf;
%! x = linspace (0, 10);
%! plot (x, x, x, x.^2);
%! title ("Labels are applied in order of object creation");
%! legend ("linear", "quadratic");

%!demo
%! clf;
%! subplot (2,1,1);
%! rand_2x3_data1 = [0.341447, 0.171220, 0.284370; 0.039773, 0.731725, 0.779382];
%! bar (rand_2x3_data1);
%! ylim ([0, 1.0]);
%! title ("legend() works for bar graphs (hggroups)");
%! legend ({"1st Bar", "2nd Bar", "3rd Bar"}, "location", "northwest");
%! subplot (2,1,2);
%! x = linspace (0, 10, 20);
%! stem (x, 0.5+x.*rand (size (x))/max (x), "markeredgecolor", [0, 0.7, 0]);
%! hold on;
%! stem (x+10/(2*20), x.*(1.0+rand (size (x)))/max (x));
%! xlim ([0, 10+10/(2*20)]);
%! title ("legend() works for stem plots (hggroups)");
%! legend ({"Multicolor", "Unicolor"}, "location", "northwest");

%!demo
%! clf;
%! colormap (cool (64));
%! surf (peaks ());
%! legend ("peaks()");
%! title ("legend() works for surface objects too");

%!demo
%! clf reset;  # needed to undo colormap assignment in previous demo
%! rand_2x3_data2 = [0.44804, 0.84368, 0.23012; 0.72311, 0.58335, 0.90531];
%! bar (rand_2x3_data2);
%! ylim ([0, 1.2]);
%! title ('"left" option places colors to the left of text label');
%! legend ("1st Bar", "2nd Bar", "3rd Bar");
%! legend left;

%!demo
%! clf;
%! x = 0:0.1:7;
%! h = plot (x,sin (x), x,cos (x), x,sin (x.^2/10), x,cos (x.^2/10));
%! title ("Only the sin() objects have keylabels");
%! legend (h([1, 3]), {"sin (x)", "sin (x^2/10)"}, "location", "southwest");

%!demo
%! clf;
%! x = 0:0.1:10;
%! plot (x, sin (x), ";sin (x);");
%! hold on;
%! plot (x, cos (x), ";cos (x);");
%! hold off;
%! title ("legend constructed from multiple plot calls");

%!demo
%! clf;
%! x = 0:0.1:10;
%! plot (x, sin (x), ";sin (x);");
%! hold on;
%! plot (x, cos (x), ";cos (x);");
%! hold off;
%! title ("Specified label text overrides previous labels");
%! legend ({"Sine", "Cosine"}, "location", "northeastoutside");

%!demo
%! clf;
%! x = 0:10;
%! plot (x, rand (11));
%! axis ([0, 10, 0, 1]);
%! xlabel ("Indices");
%! ylabel ("Random Values");
%! title ('Legend "off" deletes the legend');
%! legend (cellstr (num2str ((0:10)')), "location", "northeastoutside");
%! pause (1);
%! legend off;

%!demo
%! clf;
%! x = (1:5)';
%! subplot (2,2,1);
%!  plot (x, rand (numel (x)));
%!  legend (cellstr (num2str (x)), "location", "northwestoutside");
%! subplot (2,2,2);
%!  plot (x, rand (numel (x)));
%!  legend (cellstr (num2str (x)), "location", "northeastoutside");
%! subplot (2,2,3);
%!  plot (x, rand (numel (x)));
%!  legend (cellstr (num2str (x)), "location", "southwestoutside");
%! subplot (2,2,4);
%!  plot (x, rand (numel (x)));
%!  legend (cellstr (num2str (x)), "location", "southeastoutside");
%! ## Legend works on a per axes basis for each subplot

%!demo
%! clf;
%! plot (rand (2));
%! title ("legend() will warn if extra labels are specified");
%! legend ("Hello", "World", "foo", "bar");

%!demo
%! clf;
%! x = 0:10;
%! y1 = rand (size (x));
%! y2 = rand (size (x));
%! [ax, h1, h2] = plotyy (x, y1, x, y2);
%! title ({"plotyy legend test #1", "Blue label to left axis, Orange label to right axis"});
%! drawnow ();
%! legend ("Blue", "Orange", "location", "south");

%!demo
%! clf;
%! x = 0:10;
%! y1 = rand (size (x));
%! y2 = rand (size (x));
%! [ax, h1, h2] = plotyy (x, y1, x, y2);
%! ylabel (ax(1), {"Blue", "Y", "Axis"});
%! title ('plotyy legend test #2: "westoutside" adjusts to ylabel');
%! drawnow ();
%! legend ([h1, h2], {"Blue", "Orange"}, "location", "westoutside");

%!demo
%! clf;
%! x = 0:10;
%! y1 = rand (size (x));
%! y2 = rand (size (x));
%! [ax, h1, h2] = plotyy (x, y1, x, y2);
%! ylabel (ax(2), {"Orange", "Y", "Axis"});
%! title ('plotyy legend test #3: "eastoutside" adjusts to ylabel');
%! drawnow ();
%! legend ([h1, h2], {"Blue", "Orange"}, "location", "eastoutside");

%!demo
%! clf;
%! plot (1:10, 1:10);
%! title ("a very long label can sometimes cause problems");
%! legend ("hello very big world", "location", "northeastoutside");

%!demo  # bug 36408
%! clf;
%! subplot (3,1,1);
%!  plot (rand (1,4));
%!  xlabel xlabel;
%!  ylabel ylabel;
%!  title ("Subplots adjust to the legend placed outside");
%!  legend ({"1"}, "location", "northeastoutside");
%! subplot (3,1,2);
%!  plot (rand (1,4));
%!  xlabel xlabel;
%!  ylabel ylabel;
%!  legend ({"1234567890"}, "location", "eastoutside");
%! subplot (3,1,3);
%!  plot (rand (1,4));
%!  xlabel xlabel;
%!  ylabel ylabel;
%!  legend ({"12345678901234567890"}, "location", "southeastoutside");

%!demo  # bug 36408
%! clf;
%! subplot (3,1,1);
%!  plot (rand (1,4));
%!  title ("Subplots adjust to the legend placed outside");
%!  legend ({"1"}, "location", "northwestoutside");
%! subplot (3,1,2);
%!  plot (rand (1,4));
%!  legend ({"1234567890"}, "location", "westoutside");
%! subplot (3,1,3);
%!  plot (rand (1,4));
%!  legend ({"12345678901234567890"}, "location", "southwestoutside");

%!demo  # bug 36408
%! clf;
%! subplot (3,1,1);
%!  plot (rand (1,4));
%!  set (gca (), "yaxislocation", "right");
%!  xlabel ("xlabel");
%!  ylabel ("ylabel");
%!  title ("Subplots adjust to the legend placed outside");
%!  legend ({"1"}, "location", "northeastoutside");
%! subplot (3,1,2);
%!  plot (rand (1,4));
%!  set (gca (), "yaxislocation", "right");
%!  xlabel ("xlabel");
%!  ylabel ("ylabel");
%!  legend ({"1234567890"}, "location", "eastoutside");
%! subplot (3,1,3);
%!  plot (rand (1,4));
%!  set (gca (), "yaxislocation", "right");
%!  xlabel ("xlabel");
%!  ylabel ("ylabel");
%!  legend ({"12345678901234567890"}, "location", "southeastoutside");

%!demo  # bug 36408
%! clf;
%! subplot (3,1,1);
%!  plot (rand (1,4));
%!  set (gca (), "yaxislocation", "right");
%!  xlabel ("xlabel");
%!  ylabel ("ylabel");
%!  title ("Subplots adjust to the legend placed outside");
%!  legend ({"1"}, "location", "northwestoutside");
%! subplot (3,1,2);
%!  plot (rand (1,4));
%!  set (gca (), "yaxislocation", "right");
%!  xlabel ("xlabel");
%!  ylabel ("ylabel");
%!  legend ({"1234567890"}, "location", "westoutside");
%! subplot (3,1,3);
%!  plot (rand (1,4));
%!  set (gca (), "yaxislocation", "right");
%!  xlabel ("xlabel");
%!  ylabel ("ylabel");
%!  legend ({"12345678901234567890"}, "location", "southwestoutside");

%!demo  # bug 36408;
%! clf;
%! subplot (3,1,1);
%!  plot (rand (1,4));
%!  set (gca (), "xaxislocation", "top");
%!  xlabel ("xlabel");
%!  ylabel ("ylabel");
%!  title ("Subplots adjust to the legend placed outside");
%!  legend ({"1"}, "location", "northwestoutside");
%! subplot (3,1,2);
%!  plot (rand (1,4));
%!  set (gca (), "xaxislocation", "top");
%!  xlabel ("xlabel");
%!  ylabel ("ylabel");
%!  legend ({"1234567890"}, "location", "westoutside");
%! subplot (3,1,3);
%!  plot (rand (1,4));
%!  set (gca (), "xaxislocation", "top");
%!  xlabel ("xlabel");
%!  ylabel ("ylabel");
%!  legend ({"12345678901234567890"}, "location", "southwestoutside");

%!demo  # bug 39697
%! clf;
%! plot (1:10);
%! legend ("Legend Text");
%! title ({"Multi-line", "titles", "are *not* a", "problem"});

%!demo  # bug 61814
%! clf;
%! data = [ [1:5]' , [5:-1:1]', 2.5*ones(5,1) ];
%! hp = plot (data);
%! set (hp(1), "marker", 'x', "markersize", 15);
%! set (hp(2), "marker", 'o', "markersize", 30);
%! set (hp(3), "marker", '.', "markersize", 30);
%! legend ({"data1", "data2", "data3"}, "location", "north");
%! set (hp(2), "marker", '.');
%! set (hp(3), "marker", 'o');
%! title ("Marker sizes do not overflow legend box");

## Test input validation
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   try
%!     legend ();
%!   catch
%!     [~, id] = lasterr ();
%!     assert (id, "Octave:legend:no-object");
%!   end_try_catch
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   axes ();
%!   try
%!     legend ();
%!   catch
%!     [~, id] = lasterr ();
%!     assert (id, "Octave:legend:no-object");
%!   end_try_catch
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   axes ();
%!   light ();
%!   try
%!     legend ();
%!   catch
%!     [~, id] = lasterr ();
%!     assert (id, "Octave:legend:no-object");
%!   end_try_catch
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   axes ();
%!   hli = light ();
%!   try
%!     legend (hli);
%!   catch
%!     [~, id] = lasterr ();
%!     assert (id, "Octave:legend:bad-object");
%!   end_try_catch
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   axes ();
%!   hplot = plot (rand (3));
%!   try
%!     legend (hplot, struct);
%!   catch
%!     [~, id] = lasterr ();
%!     assert (id, "Octave:invalid-fun-call");
%!   end_try_catch
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   axes ();
%!   hplot = plot (rand (3));
%!   try
%!     legend ("a", "b", "c", hplot);
%!   catch
%!     [~, id] = lasterr ();
%!     assert (id, "Octave:invalid-fun-call");
%!   end_try_catch
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Test bugs in previous implementation
%!testif ; any (strcmp (graphics_toolkit (), {"fltk", "qt"})) <*39697>
%! hf = figure ("visible", "off");
%! unwind_protect
%!   axes ("units", "normalized");
%!   plot (1:10);
%!   hl = legend ("Legend Text", "units", "normalized");
%!   title ({'Multi-line', 'titles', 'are a', 'problem'});
%!   pos = get (gca, "position");
%!   axtop = sum (pos(2:2:4));
%!   pos = get (hl, "position");
%!   legtop = sum (pos(2:2:4));
%!   assert (legtop < axtop);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!testif HAVE_FREETYPE <*40333>
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = axes ("units", "normalized", "fontsize", 10);
%!   hold on;  # preserve properties of hax in call to plot()
%!   plot (hax, 1:10);
%!   hl = legend ("Legend Text", "units", "normalized");
%!   pos = get (gca, "position");
%!   set (hf, "position", [0, 0, 200, 200]);
%!   set (hl, "fontsize", 20);
%!   assert (get (gca, "position"), pos, 2*eps);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
