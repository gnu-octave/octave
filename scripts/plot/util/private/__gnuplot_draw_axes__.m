########################################################################
##
## Copyright (C) 2005-2023 The Octave Project Developers
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
## @deftypefn {} {} __gnuplot_draw_axes__ (@var{h}, @var{plot_stream}, @var{enhanced}, @var{bg_is_set}, @var{fg_is_set}, @var{hlgnd})
## Undocumented internal function.
## @end deftypefn

function __gnuplot_draw_axes__ (h, plot_stream, enhanced, bg_is_set,
                                fg_is_set, hlgnd)

  showhiddenhandles = get (0, "showhiddenhandles");
  unwind_protect
    set (0, "showhiddenhandles", "on");
    axis_obj = __get__ (h);
  unwind_protect_cleanup
    set (0, "showhiddenhandles", showhiddenhandles);
  end_unwind_protect

  parent_figure_obj = get (axis_obj.parent);
  gnuplot_term = __gnuplot_get_var__ (axis_obj.parent, "GPVAL_TERM");

  ## Set to false for plotyy axes.
  ymirror = true;
  if (isfield (axis_obj, "__plotyy_axes__"))
    if (all (ishghandle (axis_obj.__plotyy_axes__)))
      ymirror = false;
    else
      h = axis_obj.__plotyy_axes__;
      h = h(ishghandle (h));
      h = h(isprop (h, "__plotyy_axes__"));
      rmappdata (h, "__plotyy_axes__");
    endif
  endif

  nd = __calc_dimensions__ (h);
  if (nd == 2 && (any (get (h, "view") != [0, 90])))
    ## view() only works correctly on 3-D axes in gnuplot (bug #58526).
    nd = 3;
  endif

  if (strcmp (axis_obj.dataaspectratiomode, "manual")
      && strcmp (axis_obj.xlimmode, "manual")
      && strcmp (axis_obj.ylimmode, "manual"))
    ## All can't be "manual"
    axis_obj.plotboxaspectratiomode = "auto";
  endif

  if (strcmp (axis_obj.dataaspectratiomode, "manual")
      && strcmp (axis_obj.xlimmode, "manual")
      && strcmp (axis_obj.ylimmode, "manual")
      && (nd == 2 || all (mod (axis_obj.view, 90) == 0)))
    ## FIXME: adjust plotboxaspectratio to respect other
    fpos = get (axis_obj.parent, "position");
    apos = axis_obj.position;
  endif

  pos = __actual_axis_position__ (h);

  if (strcmp (axis_obj.dataaspectratiomode, "manual"))
    dr = axis_obj.dataaspectratio;
    if (nd == 2 || all (mod (axis_obj.view, 90) == 0))
      dr = dr(1) / dr(2);
    else
      ## FIXME: need to properly implement 3-D
      dr = mean (dr(1:2)) / dr(3);
    endif
  else
    dr = 1;
  endif

  if (strcmp (axis_obj.positionconstraint, "innerposition"))
    if (nd == 2 || all (mod (axis_obj.view, 90) == 0))
      x = [1, 1];
    else
      ## 3-D plots need to be sized down to fit in the window.
      x = 1.0 ./ sqrt ([2, 2.5]);
    endif
    fprintf (plot_stream, "set tmargin screen %.15g;\n",
             pos(2)+pos(4)/2+x(2)*pos(4)/2);
    fprintf (plot_stream, "set bmargin screen %.15g;\n",
             pos(2)+pos(4)/2-x(2)*pos(4)/2);
    fprintf (plot_stream, "set lmargin screen %.15g;\n",
             pos(1)+pos(3)/2-x(1)*pos(3)/2);
    fprintf (plot_stream, "set rmargin screen %.15g;\n",
             pos(1)+pos(3)/2+x(1)*pos(3)/2);
    sz_str = "";
  else  # positionconstraint == outerposition
    fprintf (plot_stream, "unset tmargin;\n");
    fprintf (plot_stream, "unset bmargin;\n");
    fprintf (plot_stream, "unset lmargin;\n");
    fprintf (plot_stream, "unset rmargin;\n");
    fprintf (plot_stream, "set origin %g, %g;\n", pos(1:2));
    if (strcmp (axis_obj.dataaspectratiomode, "manual"))
      sz_str = sprintf ("ratio %g", -dr);
    else
      sz_str = "noratio";
    endif
    sz_str = sprintf ("set size %s %g, %g;\n", sz_str, pos(3:4));
  endif
  if (! isempty (sz_str))
    fputs (plot_stream, sz_str);
  endif

  ## Code above uses axis size for the data aspect ratio, which isn't
  ## quite correct.  The only fine control is to set all axes units equal.
  if (nd == 3
      && strcmp (axis_obj.dataaspectratiomode, "manual")
      && axis_obj.dataaspectratio(1) == axis_obj.dataaspectratio(2))
    if (axis_obj.dataaspectratio(1) == axis_obj.dataaspectratio(3))
      zstr = "z";
    else
      zstr = "";
    endif
    fprintf (plot_stream, "set view equal xy%s;\n", zstr);
  endif

  ## Reset all labels, axis-labels, tick-labels, and title
  ## FIXME: We should have an function to initialize the axis.
  ##        Presently, this is dispersed in this function.
  fputs (plot_stream, "unset label;\n");
  fputs (plot_stream, "unset arrow;\n");
  fputs (plot_stream, "unset xtics;\n");
  fputs (plot_stream, "unset ytics;\n");
  fputs (plot_stream, "unset ztics;\n");
  fputs (plot_stream, "unset x2tics;\n");
  fputs (plot_stream, "unset y2tics;\n");

  if (isempty (axis_obj.title) || isempty (get (axis_obj.title, "string")))
    fputs (plot_stream, "unset title;\n");
  else
    if (nd == 2)
      t = get (axis_obj.title);
      colorspec = get_text_colorspec (t.color);
      [tt, f, s] = __maybe_munge_text__ (enhanced, t, "string", ...
                                         t.interpreter, gnuplot_term);
      fontspec = create_fontspec (f, s, gnuplot_term);
      fprintf (plot_stream, ['set title "%s" %s %s %s;' "\n"],
               tt, fontspec, colorspec, __do_enhanced_option__ (enhanced, t));
    else
      ## Change meaning of "normalized", but it at least gives user some control
      if (! strcmp (get (axis_obj.title, "units"), "normalized"))
        unwind_protect
          set (axis_obj.title, "units", "normalized",
                               "position", [0.5 1.02 0.5]);
        unwind_protect_cleanup
        end_unwind_protect
      endif
      t = get (axis_obj.title);
      axispos = axis_obj.position;
      screenpos = t.position;
      screenpos(1) = axispos(1)+screenpos(1)*axispos(3);
      screenpos(2) = axispos(2)+screenpos(2)*axispos(4);
      fputs (plot_stream, "unset title;\n");
      do_text (plot_stream, gnuplot_term, enhanced, t, h, screenpos);
    endif
  endif

  if (! isempty (axis_obj.xlabel))
    t = get (axis_obj.xlabel);
    angle = t.rotation;
    colorspec = get_text_colorspec (t.color);
    if (isempty (t.string))
      fprintf (plot_stream, "unset xlabel;\n");
      fprintf (plot_stream, "unset x2label;\n");
    else
      [tt, f, s] = __maybe_munge_text__ (enhanced, t, "string", ...
                                         t.interpreter, gnuplot_term);
      fontspec = create_fontspec (f, s, gnuplot_term);
      if (strcmp (axis_obj.xaxislocation, "top"))
        fprintf (plot_stream, 'set x2label "%s" %s %s %s',
                 tt, colorspec, fontspec, __do_enhanced_option__ (enhanced, t));
      else
        fprintf (plot_stream, 'set xlabel "%s" %s %s %s',
                 tt, colorspec, fontspec, __do_enhanced_option__ (enhanced, t));
      endif
      fprintf (plot_stream, " rotate by %f;\n", angle);
      if (strcmp (axis_obj.xaxislocation, "top"))
        fprintf (plot_stream, "unset xlabel;\n");
      else
        fprintf (plot_stream, "unset x2label;\n");
      endif
    endif
  endif

  if (! isempty (axis_obj.ylabel))
    t = get (axis_obj.ylabel);
    angle = t.rotation;
    colorspec = get_text_colorspec (t.color);
    if (isempty (t.string))
      fprintf (plot_stream, "unset ylabel;\n");
      fprintf (plot_stream, "unset y2label;\n");
    else
      [tt, f, s] = __maybe_munge_text__ (enhanced, t, "string", ...
                                         t.interpreter, gnuplot_term);
      fontspec = create_fontspec (f, s, gnuplot_term);
      if (strcmp (axis_obj.yaxislocation, "right"))
        fprintf (plot_stream, 'set y2label "%s" %s %s %s',
                 tt, colorspec, fontspec, __do_enhanced_option__ (enhanced, t));
      else
        fprintf (plot_stream, 'set ylabel "%s" %s %s %s',
                 tt, colorspec, fontspec, __do_enhanced_option__ (enhanced, t));
      endif
      fprintf (plot_stream, " rotate by %f;\n", angle);
      if (strcmp (axis_obj.yaxislocation, "right"))
        fprintf (plot_stream, "unset ylabel;\n");
      else
        fprintf (plot_stream, "unset y2label;\n");
      endif
    endif
  endif

  if (! isempty (axis_obj.zlabel))
    t = get (axis_obj.zlabel);
    angle = t.rotation;
    colorspec = get_text_colorspec (t.color);
    if (isempty (t.string))
      fputs (plot_stream, "unset zlabel;\n");
    else
      [tt, f, s] = __maybe_munge_text__ (enhanced, t, "string", ...
                                         t.interpreter, gnuplot_term);
      fontspec = create_fontspec (f, s, gnuplot_term);
      fprintf (plot_stream, 'set zlabel "%s" %s %s %s',
               tt, colorspec, fontspec, __do_enhanced_option__ (enhanced, t));
      fprintf (plot_stream, " rotate by %f;\n", angle);
    endif
  endif

  if (strcmp (axis_obj.xaxislocation, "top"))
    xaxisloc = "x2";
    xaxisloc_using = "x2";
  else
    xaxisloc = "x";
    xaxisloc_using = "x1";
    if (strcmp (axis_obj.xaxislocation, "origin"))
      fputs (plot_stream, "set xzeroaxis;\n");
    endif
  endif
  if (strcmp (axis_obj.yaxislocation, "right"))
    yaxisloc = "y2";
    yaxisloc_using = "y2";
  else
    yaxisloc = "y";
    yaxisloc_using = "y1";
    if (strcmp (axis_obj.yaxislocation, "origin"))
      fputs (plot_stream, "set yzeroaxis;\n");
    endif
  endif

  have_major_grid = false;
  have_minor_grid = false;
  visible_gls = ! strcmp (axis_obj.gridlinestyle, "none") ...
                && ! strcmp (axis_obj.gridcolor, "none");
  visible_mgls = ! strcmp (axis_obj.minorgridlinestyle, "none") ...
                 && ! strcmp (axis_obj.minorgridcolor, "none");

  if (strcmp (axis_obj.xgrid, "on") && visible_gls)
    have_major_grid = true;
    fprintf (plot_stream, "set grid %stics;\n", xaxisloc);
  else
    fprintf (plot_stream, "set grid no%stics;\n", xaxisloc);
  endif

  if (strcmp (axis_obj.ygrid, "on") && visible_gls)
    have_major_grid = true;
    fprintf (plot_stream, "set grid %stics;\n", yaxisloc);
  else
    fprintf (plot_stream, "set grid no%stics;\n", yaxisloc);
  endif

  if (strcmp (axis_obj.zgrid, "on") && visible_gls)
    have_major_grid = true;
    fputs (plot_stream, "set grid ztics;\n");
  else
    fputs (plot_stream, "set grid noztics;\n");
  endif

  if (strcmp (axis_obj.xminorgrid, "on") && visible_mgls)
    have_minor_grid = true;
    if (strcmp (axis_obj.xscale, "log"))
      m = 10;
    else
      m = 5;
    endif
    fprintf (plot_stream, "set m%stics %d;\n", xaxisloc, m);
    fprintf (plot_stream, "set grid m%stics;\n", xaxisloc);
  else
    fprintf (plot_stream, "set grid nom%stics;\n", xaxisloc);
  endif

  if (strcmp (axis_obj.yminorgrid, "on") && visible_mgls)
    have_minor_grid = true;
    if (strcmp (axis_obj.yscale, "log"))
      m = 10;
    else
      m = 5;
    endif
    fprintf (plot_stream, "set m%stics %d;\n", yaxisloc, m);
    fprintf (plot_stream, "set grid m%stics;\n", yaxisloc);
  else
    fprintf (plot_stream, "set grid nom%stics;\n", yaxisloc);
  endif

  if (strcmp (axis_obj.zminorgrid, "on") && visible_mgls)
    have_minor_grid = true;
    if (strcmp (axis_obj.zscale, "log"))
      m = 10;
    else
      m = 5;
    endif
    fprintf (plot_stream, "set mztics %d;\n", m);
    fputs (plot_stream, "set grid mztics;\n");
  else
    fputs (plot_stream, "set grid nomztics;\n");
  endif

  ## The grid front/back/layerdefault option also controls the
  ## appearance of tics, so it is used even if the grid is absent.
  if (strcmp (axis_obj.layer, "top"))
    fputs (plot_stream, "set grid front;\n");
    fputs (plot_stream, "set border front;\n");
  else
    fputs (plot_stream, "set grid layerdefault;\n");
  endif

  xlogscale = strcmp (axis_obj.xscale, "log");
  ylogscale = strcmp (axis_obj.yscale, "log");
  zlogscale = strcmp (axis_obj.zscale, "log");

  ## Detect logscale and negative lims
  if (xlogscale && all (axis_obj.xlim < 0))
    axis_obj.xsgn = -1;
    if (strcmp (axis_obj.xdir, "reverse"))
      axis_obj.xdir = "normal";
    elseif (strcmp (axis_obj.xdir, "normal"))
      axis_obj.xdir = "reverse";
    endif
    axis_obj.xtick = -flip (axis_obj.xtick);
    axis_obj.xticklabel = flip (axis_obj.xticklabel);
    axis_obj.xlim = -flip (axis_obj.xlim);
  else
    axis_obj.xsgn = 1;
  endif
  if (ylogscale && all (axis_obj.ylim < 0))
    axis_obj.ysgn = -1;
    if (strcmp (axis_obj.ydir, "reverse"))
      axis_obj.ydir = "normal";
    elseif (strcmp (axis_obj.ydir, "normal"))
      axis_obj.ydir = "reverse";
    endif
    axis_obj.ytick = -flip (axis_obj.ytick);
    axis_obj.yticklabel = flip (axis_obj.yticklabel);
    axis_obj.ylim = -flip (axis_obj.ylim);
  else
    axis_obj.ysgn = 1;
  endif
  if (zlogscale && all (axis_obj.zlim < 0))
    axis_obj.zsgn = -1;
    if (strcmp (axis_obj.zdir, "reverse"))
      axis_obj.zdir = "normal";
    elseif (strcmp (axis_obj.zdir, "normal"))
      axis_obj.zdir = "reverse";
    endif
    axis_obj.ztick = -flip (axis_obj.ztick);
    axis_obj.zticklabel = flip (axis_obj.zticklabel);
    axis_obj.zlim = -flip (axis_obj.zlim);
  else
    axis_obj.zsgn = 1;
  endif

  xlim = axis_obj.xlim;
  ylim = axis_obj.ylim;
  zlim = axis_obj.zlim;
  clim = axis_obj.clim;

  do_tics (axis_obj, plot_stream, ymirror, gnuplot_term);

  fputs (plot_stream, "unset logscale;\n");
  if (xlogscale)
    fprintf (plot_stream, "set logscale %s;\n", xaxisloc);
  endif
  if (ylogscale)
    fprintf (plot_stream, "set logscale %s;\n", yaxisloc);
  endif
  if (zlogscale)
    fputs (plot_stream, "set logscale z;\n");
  endif

  xautoscale = strcmp (axis_obj.xlimmode, "auto");
  yautoscale = strcmp (axis_obj.ylimmode, "auto");
  zautoscale = strcmp (axis_obj.zlimmode, "auto");
  cautoscale = strcmp (axis_obj.climmode, "auto");

  fputs (plot_stream, "set clip two;\n");

  kids = axis_obj.children;
  ## Remove the axis labels and title from the children, and
  ## preserved the original order.
  [jnk, k] = setdiff (kids, [axis_obj.xlabel; axis_obj.ylabel; ...
                             axis_obj.zlabel; axis_obj.title]);
  kids = kids(sort (k));

  if (nd == 3)
    fputs (plot_stream, "set parametric;\n");
    fputs (plot_stream, "set style data lines;\n");
    fputs (plot_stream, "set surface;\n");
    fputs (plot_stream, "unset contour;\n");
  endif

  data_idx = 0;
  data = cell ();
  is_image_data = [];
  hidden_removal = NaN;
  view_map = false;

  if (cautoscale)
    ## First pass to get cdata limits, maybe general graphics should do this
    kids1 = kids;
    clim = [Inf -Inf];

    while (! isempty (kids1))
      obj = get (kids1(end));
      kids1 = kids1(1:(end-1));

      switch (obj.type)
        case {"image", "patch", "surface"}
          if (isfield (obj, "cdatamapping")
              && strcmp (obj.cdatamapping, "scaled")
              && isfield (obj, "cdata")
              && ! isempty (obj.cdata))
            clim(1) = min (clim(1), min (obj.cdata(:)));
            clim(2) = max (clim(2), max (obj.cdata(:)));
          endif

        case "hggroup"
          ## Push group children into the kid list.
          if (isempty (kids1))
            kids1 = obj.children;
          elseif (! isempty (obj.children))
            kids1 = [kids1; obj.children];
          endif
      endswitch
    endwhile

    if (clim(1) == Inf)
      clim = axis_obj.clim;
    endif

  elseif (clim(1) == clim(2))
    clim(2)++;
  endif

  if (rows (parent_figure_obj.colormap) != 2)
    kids1 = kids;
    while (! isempty (kids1))
      obj = get (kids1(end));
      kids1 = kids1(1:(end-1));

      switch (obj.type)
        case "hggroup"
          ## Push group children into the kid list.
          if (isempty (kids1))
            kids1 = obj.children;
          elseif (! isempty (obj.children))
            kids1 = [kids1; obj.children];
          endif
      endswitch
    endwhile
  endif

  cmap = axis_obj.colormap;
  cmap_sz = rows (cmap);
  addedcmap = [];

  ximg_data = {};
  ximg_data_idx = 0;

  if (! isempty (hlgnd))
    hlgndntrp = hlgnd.interpreter;
  else
    hlgndntrp = "none";
  endif

  while (! isempty (kids))

    h_obj = kids(end);
    kids = kids(1:(end-1));

    obj = get (h_obj);

    if (isfield (obj, "xdata"))
      obj.xdata = double (obj.xdata);
    endif
    if (isfield (obj, "ydata"))
      obj.ydata = double (obj.ydata);
    endif
    if (isfield (obj, "zdata"))
      obj.zdata = double (obj.zdata);
    endif

    if (strcmp (obj.type, "text"))
      units = obj.units;
      unwind_protect
        set (h_obj, "units", "data");
        obj = get (h_obj);
      unwind_protect_cleanup
        set (h_obj, "units", units);
      end_unwind_protect
    endif

    if (strcmp (obj.visible, "off"))
      continue;
    endif

    if (xlogscale && isfield (obj, "xdata"))
      obj.xdata = axis_obj.xsgn * obj.xdata;
      obj.xdata(obj.xdata<=0) = NaN;
    endif
    if (ylogscale && isfield (obj, "ydata"))
      obj.ydata = axis_obj.ysgn * obj.ydata;
      obj.ydata(obj.ydata<=0) = NaN;
    endif
    if (zlogscale && isfield (obj, "zdata"))
      obj.zdata = axis_obj.zsgn * obj.zdata;
      obj.zdata(obj.zdata<=0) = NaN;
    endif

    switch (obj.type)
      case "image"
        img_data = mapcdata (obj.cdata, obj.cdatamapping, clim, cmap_sz);
        img_xdata = obj.xdata;
        img_ydata = obj.ydata;

        data_idx += 1;
        is_image_data(data_idx) = true;
        parametric(data_idx) = false;
        have_cdata(data_idx) = false;
        have_3d_patch(data_idx) = false;

        if (img_xdata(2) < img_xdata(1))
          img_xdata = img_xdata(2:-1:1);
          img_data = img_data(:,end:-1:1,:);
        elseif (img_xdata(1) == img_xdata(2))
          img_xdata = img_xdata(1) + [0, columns(img_data)-1];
        endif
        if (img_ydata(2) < img_ydata(1))
          img_ydata = img_ydata(2:-1:1);
          img_data = img_data(end:-1:1,:,:);
        elseif (img_ydata(1) == img_ydata(2))
          img_ydata = img_ydata(1) + [0, rows(img_data)-1];
        endif

        x_origin = min (img_xdata);
        y_origin = min (img_ydata);

        [y_dim, x_dim] = size (img_data(:,:,1));
        if (x_dim > 1)
          dx = abs (img_xdata(2)-img_xdata(1))/(x_dim-1);
        else
          x_dim = 2;
          img_data = [img_data, img_data];
          dx = abs (img_xdata(2)-img_xdata(1));
          if (dx < 1)
            ## Correct gnuplot string for 1-D images
            dx       = 0.5;
            x_origin = 0.75;
          endif
        endif
        if (y_dim > 1)
          dy = abs (img_ydata(2)-img_ydata(1))/(y_dim-1);
        else
          y_dim = 2;
          img_data = [img_data; img_data];
          dy = abs (img_ydata(2)-img_ydata(1));
          if (dy < 1)
            ## Correct gnuplot string for 1-D images
            dy       = 0.5;
            y_origin = 0.75;
          endif
        endif

        if (ndims (img_data) == 3)
          data{data_idx} = permute (img_data, [3, 1, 2])(:);
          format = "1:2:3";
          imagetype = "rgbimage";
        else
          data{data_idx} = img_data(:);
          format = "1";
          imagetype = "image";
        endif

        titlespec{data_idx} = 'title ""';
        usingclause{data_idx} = sprintf ("binary array=%dx%d scan=yx origin=(%.15g,%.15g) dx=%.15g dy=%.15g using %s",
            x_dim, y_dim, x_origin, y_origin, dx, dy, format);
        withclause{data_idx} = sprintf ("with %s", imagetype);

      case "line"
        if (strcmp (get (obj.parent, "type"), "hggroup"))
          hg = get (obj.parent, "children");
          if (hg(1) == h_obj && ! isempty (get (obj.parent, "displayname")))
            data_idx += 1;
            is_image_data(data_idx) = false;
            parametric(data_idx) = false;
            have_cdata(data_idx) = false;
            have_3d_patch(data_idx) = false;
            tmpdispname = obj.displayname;
            obj.displayname = get (obj.parent, "displayname");
            tmp = __maybe_munge_text__ (enhanced, obj, "displayname", ...
                                        hlgndntrp, gnuplot_term);
            titlespec{data_idx} = ['title "' tmp '"'];
            obj.displayname = tmpdispname;
            if (! isempty (findobj (obj.parent, "-property", "format", "-depth", 0)))
              ## Place phantom errorbar data for legend
              data{data_idx} = NaN (4,1);
              usingclause{data_idx} = sprintf ("record=1 using ($1):($2):($3):($4)");
              switch (get (obj.parent, "format"))
                case {"box" "boxy" "boxxy"}
                  errbars = "boxxy";
                case "xyerr"
                  errbars = "xyerrorbars";
                case "yerr"
                  errbars = "yerrorbars";
                case "xerr"
                  errbars = "xerrorbars";
                otherwise
                  errbars = "xerrorbars";
              endswitch
              withclause{data_idx} = sprintf ("with %s linestyle %d", ...
                                              errbars, sidx(1));
            else
              ## Place phantom stemseries data for legend
              data{data_idx} = NaN (2,1);
              usingclause{data_idx} = sprintf ("record=1 using ($1):($2)");
              hgobj = get (obj.parent);
              [hgstyle, hgsidx] = do_linestyle_command (hgobj, hgobj.color, ...
                                                        data_idx, plot_stream);
              withclause{data_idx} = sprintf ("with %s linestyle %d",
                                              hgstyle{1}, hgsidx(1));
            endif
          endif
        endif

        if (strcmp (obj.linestyle, "none")
            && (! isfield (obj, "marker")
                || (isfield (obj, "marker")
                    && strcmp (obj.marker, "none"))))
          continue;
        endif
        data_idx += 1;
        is_image_data(data_idx) = false;
        parametric(data_idx) = true;
        have_cdata(data_idx) = false;
        have_3d_patch(data_idx) = false;
        if (isempty (obj.displayname))
          titlespec{data_idx} = 'title ""';
        else
          tmp = __maybe_munge_text__ (enhanced, obj, "displayname", ...
                                      hlgndntrp, gnuplot_term);
          titlespec{data_idx} = ['title "' tmp '"'];
        endif
        usingclause{data_idx} = sprintf ("record=%d", numel (obj.xdata));
        if (nd == 3)
          xdat = obj.xdata(:);
          ydat = obj.ydata(:);
          if (! isempty (obj.zdata))
            zdat = obj.zdata(:);
          else
            zdat = zeros (size (xdat));
          endif
          data{data_idx} = [xdat, ydat, zdat]';
          usingclause{data_idx} = sprintf ("record=%d using ($1):($2):($3)",
                                           numel (xdat));
          ## fputs (plot_stream, "set parametric;\n");
        else
          xdat = obj.xdata(:);
          ydat = obj.ydata(:);
          data{data_idx} = [xdat, ydat]';
          usingclause{data_idx} = ...
            sprintf ("record=%d using ($1):($2) axes %s%s",
                     rows (xdat), xaxisloc_using, yaxisloc_using);
        endif

        [style, sidx] = do_linestyle_command (obj, obj.color, data_idx,
                                              plot_stream);

        if (isempty (style{1}))
          style{1} = "points";
          data{data_idx} = {};
        endif
        withclause{data_idx} = sprintf ("with %s linestyle %d",
                                        style{1}, sidx(1));

        if (length (style) > 1)
          data_idx += 1;
          is_image_data(data_idx) = is_image_data(data_idx - 1);
          parametric(data_idx) = parametric(data_idx - 1);
          have_cdata(data_idx) = have_cdata(data_idx - 1);
          have_3d_patch(data_idx) = have_3d_patch(data_idx - 1);
          titlespec{data_idx} = 'title ""';
          usingclause{data_idx} = usingclause{data_idx - 1};
          data{data_idx} = data{data_idx - 1};
          withclause{data_idx} = sprintf ("with %s linestyle %d",
                                          style{2}, sidx(2));
        endif
        if (length (style) > 2)
          data_idx += 1;
          is_image_data(data_idx) = is_image_data(data_idx - 1);
          parametric(data_idx) = parametric(data_idx - 1);
          have_cdata(data_idx) = have_cdata(data_idx - 1);
          have_3d_patch(data_idx) = have_3d_patch(data_idx - 1);
          titlespec{data_idx} = 'title ""';
          usingclause{data_idx} = usingclause{data_idx - 1};
          data{data_idx} = data{data_idx - 1};
          withclause{data_idx} = sprintf ("with %s linestyle %d",
                                          style{3}, sidx(3));
        endif

      case "patch"
        persistent do_interp_warning = true;

        if (strcmp (get (obj.parent, "type"), "hggroup"))
          obj.displayname = get (obj.parent, "displayname");
        endif

        [nr, nc] = size (obj.xdata);

        if (! isempty (obj.cdata))
          cdat = obj.cdata;
        else
          cdat = [];
        endif

        data_3d_idx = NaN;
        for i = 1:nc
          xcol = obj.xdata(:,i);
          ycol = obj.ydata(:,i);
          if (nd == 3)
            if (! isempty (obj.zdata))
              zcol = obj.zdata(:,i);
            else
              zcol = zeros (size (xcol));
            endif
          endif

          if (! isnan (xcol) && ! isnan (ycol))
            ## Is the patch closed or not
            if (! strcmp (obj.facecolor, "none"))
              hidden_removal = true;
              if (nd == 3)
                if (numel (xcol) > 3)
                  error ("__gnuplot_draw_axes__: gnuplot (as of v4.2) only supports 3-D filled triangular patches");
                else
                  if (isnan (data_3d_idx))
                    data_idx += 1;
                    data_3d_idx = data_idx;
                    is_image_data(data_idx) = false;
                    parametric(data_idx) = false;
                    have_cdata(data_idx) = true;
                    have_3d_patch(data_idx) = true;
                    withclause{data_3d_idx} = sprintf ("with pm3d");
                    usingclause{data_3d_idx} =  "using 1:2:3:4";
                    data{data_3d_idx} = [];
                  endif
                  local_idx = data_3d_idx;
                  ccdat = NaN;
                endif
              else
                data_idx += 1;
                local_idx = data_idx;
                is_image_data(data_idx) = false;
                parametric(data_idx) = false;
                have_cdata(data_idx) = false;
                have_3d_patch(data_idx) = false;
              endif

              if (i > 1 || isempty (obj.displayname))
                titlespec{local_idx} = 'title ""';
              else
                tmp = __maybe_munge_text__ (enhanced, obj, "displayname", ...
                                            hlgndntrp, gnuplot_term);
                titlespec{local_idx} = ['title "' tmp '"'];
              endif
              if (isfield (obj, "facecolor"))
                if ((strcmp (obj.facecolor, "flat")
                    || strcmp (obj.facecolor, "interp"))
                    && isfield (obj, "cdata"))
                  if (ndims (obj.cdata) == 2
                      && (columns (obj.cdata) == nc
                          && (rows (obj.cdata) == 1
                              || rows (obj.cdata) == 3)))
                    ccol = cdat(:, i);
                  elseif (ndims (obj.cdata) == 2
                      && (rows (obj.cdata) == nc
                          && (columns (obj.cdata) == 1
                              || columns (obj.cdata) == 3)))
                    ccol = cdat(i, :);
                  elseif (ndims (obj.cdata) == 3)
                    ccol = permute (cdat (:, i, :), [1, 3, 2]);
                  else
                    ccol = cdat;
                  endif
                  if (strcmp (obj.facecolor, "flat"))
                    if (isequal (size (ccol), [1, 3]))
                      ## RGB Triplet
                      color = ccol;
                    else
                      ccdat = mapcdata (ccol, obj.cdatamapping, clim, cmap_sz);
                      if (nd == 3 && numel (xcol) == 3)
                        color = cmap(ccdat(1), :);
                      else
                        color = cmap(ccdat, :);
                      endif
                    endif
                  elseif (strcmp (obj.facecolor, "interp"))
                    if (nd == 3 && numel (xcol) == 3)
                      ccdat = ccol;
                      if (! isvector (ccdat))
                        tmp = cmap_sz + rows (addedcmap) + ...
                             [1 : rows(ccdat)];
                        addedcmap = [addedcmap; ccdat];
                        ccdat = tmp(:);
                      else
                        ccdat = mapcdata (ccdat(:), obj.cdatamapping, clim, cmap_sz);
                      endif
                    else
                      if (do_interp_warning && sum (diff (ccol)))
                        warning ('"interp" not supported, using 1st entry of cdata');
                        do_interp_warning = false;
                      endif
                      r = mapcdata (ccol, obj.cdatamapping, clim, cmap_sz);
                      color = cmap(r(1),:);
                    endif
                  endif
                elseif (isnumeric (obj.facecolor))
                  color = obj.facecolor;
                else
                  color = [0, 1, 0];
                endif
              else
                color = [0, 1, 0];
              endif

              if (nd == 3 && numel (xcol) == 3)
                if (isnan (ccdat))
                  ccdat = (cmap_sz + rows (addedcmap) + 1) * ones (3, 1);
                  addedcmap = [addedcmap; reshape(color, 1, 3)];
                elseif (numel (ccdat) == 1)
                  ccdat = ccdat * ones (size (zcol));
                elseif (numel (ccdat) < 1)
                  ccdat = zcol;
                endif
                data{data_3d_idx} = [data{data_3d_idx}, ...
                                     [[xcol; xcol(end)], [ycol; ycol(end)], ...
                                     [zcol; zcol(end)], [ccdat; ccdat(end)]]'];
              else
                if (isscalar (obj.facealpha))
                  colorspec = sprintf ('lc rgb "#%02x%02x%02x" fillstyle transparent solid %f',
                                       round (255*color), obj.facealpha);
                else
                  colorspec = sprintf ('lc rgb "#%02x%02x%02x"',
                                       round (255*color));
                endif

                withclause{data_idx} = sprintf ("with filledcurve %s",
                                              colorspec);
                data{data_idx} = [xcol, ycol]';
                usingclause{data_idx} = sprintf ("record=%d using ($1):($2)",
                                                 numel (xcol));
              endif
            endif
          endif

          ## patch outline
          if (!(strcmp (obj.edgecolor, "none")
                 && (strcmp (obj.marker, "none")
                     || (strcmp (obj.markeredgecolor, "none")
                         && strcmp (obj.markerfacecolor, "none")))))

            data_idx += 1;
            is_image_data(data_idx) = false;
            parametric(data_idx) = false;
            have_cdata(data_idx) = false;
            have_3d_patch(data_idx) = false;
            titlespec{data_idx} = 'title ""';
            usingclause{data_idx} = sprintf ("record=%d", numel (obj.xdata));

            if (isfield (obj, "markersize"))
              mdat = obj.markersize / 3;
            endif

            if (isfield (obj, "edgecolor"))
              ## FIXME: This is the wrong thing to do as edgecolor,
              ## markeredgecolor and markerfacecolor can have different values
              ## and we should treat them separately.  However, the code below
              ## allows the scatter functions to work as expected, where only
              ## one of these values is set.
              if (strcmp (obj.edgecolor, "none"))
                if (strcmp (obj.markeredgecolor, "none"))
                  ec = obj.markerfacecolor;
                else
                  ec = obj.markeredgecolor;
                endif
              else
                ec = obj.edgecolor;
              endif

              if ((strcmp (ec, "flat") || strcmp (ec, "interp"))
                  && isfield (obj, "cdata"))
                if (ndims (obj.cdata) == 2
                    && (columns (obj.cdata) == nc
                        && (rows (obj.cdata) == 1
                            || rows (obj.cdata) == 3)))
                  ccol = cdat(:, i);
                elseif (ndims (obj.cdata) == 2
                        && (rows (obj.cdata) == nc
                            && (columns (obj.cdata) == 1
                                || columns (obj.cdata) == 3)))
                  ccol = cdat(i, :);
                elseif (ndims (obj.cdata) == 3)
                  ccol = permute (cdat (:, i, :), [1, 3, 2]);
                else
                  ccol = cdat;
                endif
                if (strcmp (ec, "flat"))
                  if (isequal (size (ccol), [1, 3]))
                    color = ccol;
                  else
                    if (columns (ccol) != 3)
                      ccol = mapcdata (ccol, obj.cdatamapping, clim, cmap_sz);
                    endif
                    if (isscalar (ccol))
                      ccol = repmat (ccol, numel (xcol), 1);
                    endif
                    color = "flat";
                    have_cdata(data_idx) = true;
                  endif
                elseif (strcmp (ec, "interp"))
                  if (do_interp_warning && numel (ccol) == 3)
                    warning ('"interp" not supported, using 1st entry of cdata');
                    do_interp_warning = false;
                    color = mapcdata (ccol(:), obj.cdatamapping, clim, cmap_sz);
                  else
                    if (isscalar (ccol))
                      ccol = repmat (ccol, numel (xcol), 1);
                    endif
                    color = "interp";
                    have_cdata(data_idx) = true;
                  endif
                  ccol = mapcdata (ccol, obj.cdatamapping, clim, cmap_sz);
                endif
              elseif (isnumeric (ec))
                color = ec;
              else
                color = [0, 0, 0];
              endif
            else
              color = [0, 0, 0];
            endif

            lt = gnuplot_linestyletype (obj);

            if (isfield (obj, "linewidth"))
              lw = sprintf ("linewidth %f", obj.linewidth);
            else
              lw = "";
            endif

            [pt, pt2, obj] = gnuplot_pointtype (obj);
            if (! isempty (pt))
              pt = sprintf ("pointtype %s", pt);
            endif
            if (! isempty (pt2))
              pt2 = sprintf ("pointtype %s", pt2);
            endif

            if (ischar (color))
              if (columns (ccol) == 1)
                colorspec = "palette";
              elseif (columns (ccol) == 3)
                colorspec = "lc rgb variable";
                ccol = 255*ccol*double ([0x00_00_01; 0x00_01_00; 0x01_00_00]);
              endif
            else
              colorspec = sprintf ('lc rgb "#%02x%02x%02x"',
                                   uint8 (255*color));
            endif

            sidx = 1;
            if (isempty (lt))
              style = "";
            else
              style = "lines";
            endif
            tmpwith = {};

            facesame = true;
            if (! isequal (pt, pt2) && isfield (obj, "markerfacecolor")
                && ! strcmp (obj.markerfacecolor, "none"))
              if (strcmp (obj.markerfacecolor, "auto")
                  || ! isnumeric (obj.markerfacecolor)
                  || (isnumeric (obj.markerfacecolor)
                      && isequal (color, obj.markerfacecolor)))
                style = [style "points"];
                if (isfield (obj, "markersize"))
                  if (length (mdat) == nc)
                    m = mdat(i);
                  else
                    m = mdat;
                  endif
                  ps = sprintf ("pointsize %f", m / 3);
                else
                  ps = "";
                endif

                tmpwith{sidx} = sprintf ("with %s %s %s %s %s %s",
                                         style, lw, pt2, lt, ps,
                                         colorspec);
              else
                facesame = false;
                if (! isempty (style))
                  tmpwith{sidx} = sprintf ("with %s %s %s %s",
                                           style, lw, lt,
                                           colorspec);
                  sidx += 1;
                endif
                if (isnumeric (obj.markerfacecolor))
                  colorspec = sprintf ('lc rgb "#%02x%02x%02x"',
                                       round (255*obj.markerfacecolor));
                endif
                style = "points";
                if (isfield (obj, "markersize"))
                  if (length (mdat) == nc)
                    m = mdat(i);
                  else
                    m = mdat;
                  endif
                  ps = sprintf ("pointsize %f", m / 3);
                else
                  ps = "";
                endif
                tmpwith{sidx} = sprintf ("with %s %s %s %s %s %s",
                                         style, lw, pt2, lt, ps,
                                         colorspec);
              endif
            endif

            if (isfield (obj, "markeredgecolor")
                && ! strcmp (obj.markeredgecolor, "none"))
              if (facesame && ! isempty (pt)
                  && (strcmp (obj.markeredgecolor, "auto")
                      || ! isnumeric (obj.markeredgecolor)
                      || (isnumeric (obj.markeredgecolor)
                          && isequal (color, obj.markeredgecolor))))
                if (sidx == 1 && ((length (style) == 5
                         && strncmp (style, "lines", 5))
                        || isempty (style)))
                  style = [style, "points"];
                  if (isfield (obj, "markersize"))
                    if (length (mdat) == nc)
                      m = mdat(i);
                    else
                      m = mdat;
                    endif
                    ps = sprintf ("pointsize %f", m / 3);
                  else
                    ps = "";
                  endif
                  tmpwith{sidx} = sprintf ("with %s %s %s %s %s %s",
                                           style, lw, pt, lt, ps,
                                           colorspec);
                endif
              else
                if (! isempty (style))
                  if (length (tmpwith) < sidx || isempty (tmpwith{sidx}))
                    tmpwith{sidx} = sprintf ("with %s %s %s %s",
                                             style, lw, lt,
                                             colorspec);
                  endif
                  sidx += 1;
                endif

                if (! isempty (pt))
                  if (strcmp (obj.markeredgecolor, "auto"))
                    colorspec = sprintf ('lc rgb "#%02x%02x%02x"',
                                         round (255*color));
                  elseif (isnumeric (obj.markeredgecolor))
                    colorspec = sprintf ('lc rgb "#%02x%02x%02x"',
                                         round (255*obj.markeredgecolor));
                  endif
                  style = "points";
                  if (isfield (obj, "markersize"))
                    if (length (mdat) == nc)
                      m = mdat(i);
                    else
                      m = mdat;
                    endif
                    ps = sprintf ("pointsize %f", m / 3);
                  else
                    ps = "";
                  endif
                  tmpwith{sidx} = sprintf ("with %s %s %s %s %s %s",
                                           style, lw, pt, lt, ps,
                                           colorspec);
                endif
              endif
            endif

            if (! isempty (tmpwith))
              withclause{data_idx} = tmpwith{1};
            else
              if (! isempty (style))
                withclause{data_idx} = sprintf ("with %s %s %s %s %s",
                                                style, lw, pt, lt,
                                                colorspec);
              else
                withclause{data_idx} = "";
              endif
            endif
            if (nd == 3)
              if (ischar (color))
                if (! isnan (xcol) && ! isnan (ycol) && ! isnan (zcol))
                  data{data_idx} = [[xcol; xcol(1)], [ycol; ycol(1)], ...
                                    [zcol; zcol(1)], [ccol; ccol(1)]]';
                else
                  data{data_idx} = [xcol, ycol, zcol, ccol]';
                endif
                usingclause{data_idx} = sprintf ("record=%d using ($1):($2):($3):($4)", columns (data{data_idx}));
              else
                if (! isnan (xcol) && ! isnan (ycol) && ! isnan (zcol))
                  data{data_idx} = [[xcol; xcol(1)], [ycol; ycol(1)], ...
                                    [zcol; zcol(1)]]';
                else
                  data{data_idx} = [xcol, ycol, zcol]';
                endif
                usingclause{data_idx} = sprintf ("record=%d using ($1):($2):($3)", columns (data{data_idx}));
              endif
            else
              if (ischar (color))
                if (! isnan (xcol) && ! isnan (ycol))
                  data{data_idx} = [[xcol; xcol(1)], [ycol; ycol(1)], ...
                                    [ccol; ccol(1)]]';
                else
                  data{data_idx} = [xcol, ycol, ccol]';
                endif
                usingclause{data_idx} = sprintf ("record=%d using ($1):($2):($3)", columns (data{data_idx}));
              else
                if (! isnan (xcol) && ! isnan (ycol))
                  data{data_idx} = [[xcol; xcol(1)], [ycol; ycol(1)]]';
                else
                  data{data_idx} = [xcol, ycol]';
                endif
                usingclause{data_idx} = sprintf ("record=%d using ($1):($2)", columns (data{data_idx}));
              endif
            endif

            if (length (tmpwith) > 1)
              data_idx += 1;
              is_image_data(data_idx) = is_image_data(data_idx - 1);
              parametric(data_idx) = parametric(data_idx - 1);
              have_cdata(data_idx) = have_cdata(data_idx - 1);
              have_3d_patch(data_idx) = have_3d_patch(data_idx - 1);
              titlespec{data_idx} = 'title ""';
              usingclause{data_idx} = usingclause{data_idx - 1};
              data{data_idx} = data{data_idx - 1};
              withclause{data_idx} = tmpwith{2};
            endif
            if (length (tmpwith) > 2)
              data_idx += 1;
              is_image_data(data_idx) = is_image_data(data_idx - 1);
              parametric(data_idx) = parametric(data_idx - 1);
              have_cdata(data_idx) = have_cdata(data_idx - 1);
              have_3d_patch(data_idx) = have_3d_patch(data_idx - 1);
              titlespec{data_idx} = 'title ""';
              usingclause{data_idx} = usingclause{data_idx - 1};
              data{data_idx} = data{data_idx - 1};
              withclause{data_idx} = tmpwith{3};
            endif
          endif
        endfor

      case "surface"
        view_map = true;
        tspec = 'title ""';
        if (! isempty (obj.displayname))
          ## Place phantom line data for approximate legend symbol
          data_idx += 1;
          is_image_data(data_idx) = false;
          parametric(data_idx) = false;
          have_cdata(data_idx) = false;
          have_3d_patch(data_idx) = false;
          tmp = __maybe_munge_text__ (enhanced, obj, "displayname", ...
                                      hlgndntrp, gnuplot_term);
          titlespec{data_idx} = ['title "' tmp '"'];
          data{data_idx} = NaN (3,1);
          usingclause{data_idx} = sprintf ("record=1 using ($1):($2):($3)");
          withclause{data_idx} = sprintf ('with line linewidth 10 linecolor rgb "#%02x%02x%02x"',
                                          round (255*cmap(end/2,:)));
        endif

        xdat = obj.xdata;
        ydat = obj.ydata;
        zdat = obj.zdata;
        cdat = mapcdata (obj.cdata, obj.cdatamapping, clim, cmap_sz);
        err = false;
        if (! size_equal (zdat, cdat))
          err = true;
        endif
        if (isvector (xdat) && isvector (ydat) && ismatrix (zdat))
          if (rows (zdat) == length (ydat)
              && columns (zdat) == length (xdat))
            [xdat, ydat] = meshgrid (xdat, ydat);
          else
            err = true;
          endif
        elseif (ismatrix (xdat) && ismatrix (ydat) && ismatrix (zdat))
          if (! size_equal (xdat, ydat, zdat))
            err = true;
          endif
        else
          err = true;
        endif
        if (err)
          error ("__gnuplot_draw_axes__: invalid grid data");
        endif
        xlen = columns (zdat);
        ylen = rows (zdat);

        if (! strcmp (obj.facecolor, "none"))
          data_idx += 1;
          is_image_data(data_idx) = false;
          parametric(data_idx) = false;
          have_cdata(data_idx) = true;
          have_3d_patch(data_idx) = false;

          titlespec{data_idx} = tspec;
          tspec = 'title ""';

          flat_interp_face = (strcmp (obj.facecolor, "flat")
                              || strcmp (obj.facecolor, "interp"));

          if (xlen == columns (xdat) && xlen == columns (ydat)
              && ylen == rows (xdat) && ylen == rows (ydat))
            len = 4 * xlen;
            zz = zeros (ylen, len);
            if (! flat_interp_face)
              addedcmap = [addedcmap; obj.facecolor];
            endif
            k = 1;
            for kk = 1:4:len
              zz(:,kk)   = xdat(:,k);
              zz(:,kk+1) = ydat(:,k);
              zz(:,kk+2) = zdat(:,k);
              if (flat_interp_face)
                zz(:,kk+3) = cdat(:,k);
              else
                zz(:,kk+3) = cmap_sz + rows (addedcmap);
              endif
              k += 1;
            endfor
            data{data_idx} = zz.';
          endif

          doing_interp_color = strcmp (obj.facecolor, "interp");
          if (doing_interp_color)
            interp_str = "interpolate 0, 0";
          else
            ## No interpolation of facecolors.
            interp_str = "";
          endif
          usingclause{data_idx} = sprintf ("record=%dx%d using ($1):($2):($3):($4)", ylen, xlen);

          fputs (plot_stream, "unset pm3d\n");
          fputs (plot_stream, "set style increment default;\n");
          hidden_removal = true;
          withclause{data_idx} = sprintf ("with pm3d");

          if (doing_interp_color)
            ## "depthorder" interferes with interpolation of colors.
            dord = "scansautomatic";
          else
            dord = "depthorder";
          endif

          if (isscalar (obj.facealpha))
            fprintf (plot_stream,
                     "set style fill transparent solid %f;\n",
                     obj.facealpha);
          endif
          fprintf (plot_stream,
                   "set pm3d explicit at s %s %s corners2color c3;\n",
                   interp_str, dord);
        endif

        draw_surf_line = ! strcmp (obj.linestyle, "none") && ! strcmp (obj.edgecolor, "none");
        draw_surf_marker = (! strcmp (obj.marker, "none")
                             && ! (strcmp (obj.markeredgecolor, "none")
                                   && strcmp (obj.markerfacecolor, "none")));
        if (draw_surf_line || draw_surf_marker)
          flat_interp_edge = (strcmp (obj.edgecolor, "flat")
                              || strcmp (obj.edgecolor, "interp"));
          flat_marker = (strcmp (obj.markeredgecolor, "flat") || strcmp (obj.markerfacecolor, "flat")
                         || strcmp (obj.markeredgecolor, "auto") || strcmp (obj.markerfacecolor, "auto"));
          [style, sidx] = do_linestyle_command (obj, obj.edgecolor,
                                                data_idx,
                                                plot_stream);
          domeshcolumn = (strcmp (obj.meshstyle, "column")
                          || strcmp (obj.meshstyle, "both"));
          domeshrow = (strcmp (obj.meshstyle, "row")
                       || strcmp (obj.meshstyle, "both"));
          num_pass = 0;
          num_cols = 0;
          if (domeshcolumn)
            num_pass += xlen;
            num_cols = xlen;
          endif
          if (domeshrow)
            num_pass += ylen;
          endif

          for np = 1:num_pass
            for i_stl = 1:length (style)
              has_ccol = ((strncmp (style{i_stl}, "lines", 5)
                           && flat_interp_edge)
                          || (strncmp (style{i_stl}, "points", 6)
                              && flat_marker));
              if (has_ccol)
                ccol = ":($4)";
                N_tup = 4;
              else
                ccol = "";
                N_tup = 3;
              endif
              sopt = sprintf ("ls %d", sidx (i_stl));

              if (np <= num_cols)
                k = np;
                yrec = ylen;
                zz = zeros (ylen, N_tup);
                zz(:,1) = xdat(:,k);
                zz(:,2) = ydat(:,k);
                zz(:,3) = zdat(:,k);
                if (has_ccol)
                  zz(:,4) = cdat(:,k);
                endif
              else
                j = np - num_cols;
                yrec = xlen;
                zz = zeros (xlen, N_tup);
                zz(:,1) = xdat(j,:)';
                zz(:,2) = ydat(j,:)';
                zz(:,3) = zdat(j,:)';
                if (has_ccol)
                  zz(:,4) = cdat(j,:)';
                endif
              endif

              zz = zz.';

              data_idx += 1;
              is_image_data(data_idx) = false;
              parametric(data_idx) = false;
              if (has_ccol)
                have_cdata(data_idx) = true;
              else
                have_cdata(data_idx) = false;
              endif
              have_3d_patch(data_idx) = false;
              titlespec{data_idx} = tspec;
              usingclause{data_idx} = sprintf ("record=%dx1 using ($1):($2):($3)%s",
                                               yrec, ccol);
              if (isempty (style{i_stl}))
                style{i_stl} = "points";
                data{data_idx} = {};
              else
                data{data_idx} = zz;
              endif
              withclause{data_idx} = sprintf ("with %s %s",
                                              style{i_stl}, sopt);
            endfor
          endfor
        endif

      case "text"
        do_text (plot_stream, gnuplot_term, enhanced, obj, h);

      case "hggroup"
        ## Push group children into the kid list.
        if (isempty (kids))
          kids = obj.children;
        elseif (! isempty (obj.children))
          kids = [kids; obj.children];
        endif

      case "light"
        ## ignore it

      otherwise
        error ("__gnuplot_draw_axes__: unknown object class, %s", obj.type);
    endswitch

  endwhile

  ## This is needed to prevent warnings for rotations in 3-D plots, while
  ## allowing colorbars with contours.
  if (nd == 2 || (data_idx > 1 && ! view_map))
    fputs (plot_stream, "set pm3d implicit;\n");
  else
    fputs (plot_stream, "set pm3d explicit;\n");
  endif

  if (! isnan (hidden_removal) && hidden_removal)
    fputs (plot_stream, "set hidden3d front nooffset;\n");
  else
    fputs (plot_stream, "unset hidden3d;\n");
  endif

  have_data = (! (isempty (data) || all (cellfun ("isempty", data))));

  ## Note we don't use the [xy]2range of gnuplot as we don't use the
  ## dual axis plotting features of gnuplot.
  if (isempty (xlim))
    return;
  endif
  if (strcmp (axis_obj.xdir, "reverse"))
    xlim = flip (xlim);
  endif

  fprintf (plot_stream, "set xrange [%.15e:%.15e];\n", xlim);
  if (strcmp (axis_obj.xaxislocation, "top"))
    fprintf (plot_stream, "set x2range [%.15e:%.15e];\n", xlim);
  endif

  if (isempty (ylim))
    return;
  endif
  if (strcmp (axis_obj.ydir, "reverse"))
    ylim = flip (ylim);
  endif
  fprintf (plot_stream, "set yrange [%.15e:%.15e];\n", ylim);
  if (strcmp (axis_obj.yaxislocation, "right"))
    fprintf (plot_stream, "set y2range [%.15e:%.15e];\n", ylim);
  endif

  if (nd == 3)
    if (isempty (zlim))
      return;
    endif
    if (strcmp (axis_obj.zdir, "reverse"))
      zlim = flip (zlim);
    endif
    fprintf (plot_stream, "set zrange [%.15e:%.15e];\n", zlim);
  endif

  if (! any (isinf (clim)))
    if (rows (addedcmap) > 0)
      fprintf (plot_stream, "set cbrange [1:%.15e];\n",
               cmap_sz + rows (addedcmap));
    else
      fprintf (plot_stream, "set cbrange [1:%.15e];\n", max ([cmap_sz 2]));
    endif
  endif

  if (strcmp (axis_obj.visible, "off"))
    fputs (plot_stream, "unset border; unset tics\n");
  else
    fputs (plot_stream, "unset border\n");
    axis_idx = data_idx;
    if (strcmp (axis_obj.box, "on"))
      if (nd == 3)
        do_border_tick_3d (axis_obj, plot_stream);
      else
        axis_idx = do_border_2d (axis_obj, plot_stream, axis_idx);
      endif
    else
      if (nd == 3)
        do_border_tick_3d (axis_obj, plot_stream);
      else
        axis_idx = do_border_2d (axis_obj, plot_stream, axis_idx);
        if (isempty (axis_obj.xtick))
        elseif (strcmp (axis_obj.xaxislocation, "top"))
          fprintf (plot_stream, "set x2tics %s nomirror\n", axis_obj.tickdir);
        else # xaxislocation == "bottom" or "origin"
          fprintf (plot_stream, "set xtics %s nomirror\n", axis_obj.tickdir);
        endif
        if (isempty (axis_obj.ytick))
        elseif (strcmp (axis_obj.yaxislocation, "right"))
          fprintf (plot_stream, "set y2tics %s nomirror\n", axis_obj.tickdir);
        else # yaxislocation == "left" or "origin"
          fprintf (plot_stream, "set ytics %s nomirror\n",  axis_obj.tickdir);
        endif
      endif
    endif
  endif

  if (! have_major_grid && ! have_minor_grid)
    fputs (plot_stream, "unset grid;\n");
  else
    if (exist ("axis_idx", "var"))
      grid_idx = axis_idx;
    else
      grid_idx = data_idx;
    endif
    if (have_major_grid)
      grid_idx += 1;
      grid_obj.linestyle = axis_obj.gridlinestyle;
      grid_obj.linewidth = axis_obj.linewidth;
      grid_obj.alpha = axis_obj.gridalpha;
      [style, sidx_major] = do_linestyle_command (grid_obj, axis_obj.gridcolor,
                                                  grid_idx, plot_stream);
    else
      sidx_major = 0;
    endif
    if (have_minor_grid)
      grid_idx += 1;
      grid_obj.linestyle = axis_obj.minorgridlinestyle;
      grid_obj.linewidth = axis_obj.linewidth;
      grid_obj.alpha = axis_obj.minorgridalpha;
      [style, sidx_minor] = do_linestyle_command (grid_obj,
                                                  axis_obj.minorgridcolor,
                                                  grid_idx, plot_stream);
    else
      sidx_minor = 0;
    endif
    fprintf (plot_stream, "set grid linestyle %d, linestyle %d;\n",
             sidx_major, sidx_minor);
  endif

  if (! isempty (hlgnd) && strcmp (hlgnd.visible, "on")
      && ! isempty (hlgnd.children)
      && any (strcmp (get ([getappdata(hlgnd.children, "handle"){:}], "visible"),
                      "on")))

    if (strcmp (hlgnd.box, "on"))
      box = "box";
    else
      box = "nobox";
    endif
    if (strcmp (hlgnd.orientation, "vertical"))
      horzvert = "vertical";
    else
      horzvert = "horizontal";
    endif
    if (strcmp (hlgnd.textposition, "right"))
      reverse = "reverse Left";
    else
      reverse = "noreverse Right";
    endif
    inout = "inside";
    keypos = hlgnd.location;
    if (ischar (keypos))
      keypos = lower (keypos);
      keyout = strfind (keypos, "outside");
      if (! isempty (keyout))
        inout = "outside";
        keypos = keypos(1:keyout-1);
      endif
    endif
    switch (keypos)
      case "north"
        pos = "center top";
      case "south"
        pos = "center bottom";
      case "east"
        pos = "right center";
      case "west"
        pos = "left center";
      case "northeast"
        pos = "right top";
      case "northwest"
        pos = "left top";
      case "southeast"
        pos = "right bottom";
      case "southwest"
        pos = "left bottom";
      case "best"
        pos = "";
        warning ("legend: 'Best' not yet implemented for location specifier.\n");
        ## Least conflict with data in plot.
        ## Least unused space outside plot.
      otherwise
        pos = "";
    endswitch
    [fontname, fontsize] = get_fontname_and_size (hlgnd);
    fontspacespec = [create_spacingspec(fontname, fontsize, gnuplot_term),...
                     ' ', create_fontspec(fontname, fontsize, gnuplot_term)];
    textcolors = get (findobj (hlgnd.children, "type", "text"), "color");
    if (iscell (textcolors))
      textcolors = cell2mat (textcolors);
      textcolors = unique (textcolors, "rows");
    endif
    if (rows (textcolors) > 1)
      ## Gnuplot is unable to assign arbitrary colors to each text entry
      ## for the key/legend.  But, the text color can be set to match the
      ## color of the plot object.
      colorspec = "textcolor variable";
    else
      colorspec = get_text_colorspec (textcolors);
    endif
    fprintf (plot_stream, "set key %s %s;\nset key %s %s %s %s %s %s;\n",
             inout, pos, box, reverse, horzvert, fontspacespec, colorspec,
             __do_enhanced_option__ (enhanced, hlgnd));
  else
    fputs (plot_stream, "unset key;\n");
  endif
  fputs (plot_stream, "set style data lines;\n");

  cmap = [cmap; addedcmap];
  cmap_sz += rows (addedcmap);
  if (cmap_sz == 1)        # bug #48083, illegal one-element colormap
    cmap = [cmap; cmap];
    cmap_sz = 2;
  endif
  if (length (cmap) > 0)
    fprintf (plot_stream,
             "set palette positive color model RGB maxcolors %i;\n",
             cmap_sz);
    fprintf (plot_stream,
             ['set palette file "-" binary record=%d using 1:2:3:4;' "\n"],
             cmap_sz);
    fwrite (plot_stream, [1:cmap_sz; cmap.'], "float32");
    fwrite (plot_stream, "\n");
  endif

  fputs (plot_stream, "unset colorbox;\n");

  if (have_data)
    for i = 1:data_idx
      ## Images can be obscured by background or foreground image
      if (is_image_data (i))
        if (bg_is_set)
          fputs (plot_stream, ['if (GPVAL_TERM eq "qt") unset obj 1;' "\n"]);
          bg_is_set = false;
        endif
        if (fg_is_set)
          fputs (plot_stream, "unset obj 2; \\\n");
          fg_is_set = false;
        endif
        break;
      endif
    endfor
    if (nd == 2)
      plot_cmd = "plot";
    else
      plot_cmd = "splot";
      ## Wrap view correctly to match Matlab
      if (axis_obj.view(2) <= 90)
        rot_x = 90 - axis_obj.view(2);
      else
        rot_x = axis_obj.view(2) - 90;
      endif
      rot_x = mod (rot_x, 360);
      while (rot_x < 0)
        rot_x += 360;
      endwhile
      if (axis_obj.view(2) <= 90)
        rot_z = axis_obj.view(1);
      else
        rot_z = axis_obj.view(1) + 180;
      endif
      rot_z = mod (rot_z, 360);
      while (rot_z < 0)
        rot_z += 360;
      endwhile
      fputs (plot_stream, "set ticslevel 0;\n");
      if (view_map && rot_x == 0 && rot_z == 0)
        fputs (plot_stream, "set view map;\n");
      else
        fprintf (plot_stream, "set view %.15g, %.15g;\n", rot_x, rot_z);
      endif
    endif
    if (have_3d_patch (1))
      fputs (plot_stream, "set pm3d depthorder\n");
      ## FIXME: Must leave strings ending in '\', CHAR in double quotes.
      ## Otherwise, fprintf routine tries to do escape processing and fails.
      fprintf (plot_stream, "%s \"-\" %s %s %s \\\n", plot_cmd,
               usingclause{1}, titlespec{1}, withclause{1});
    elseif (is_image_data (1))
      fprintf (plot_stream, "%s \"-\" %s %s %s \\\n", plot_cmd,
               usingclause{1}, titlespec{1}, withclause{1});
    else
      fprintf (plot_stream, "%s \"-\" binary format='%%float64' %s %s %s \\\n",
               plot_cmd, usingclause{1}, titlespec{1}, withclause{1});
    endif
    for i = 2:data_idx
      if (have_3d_patch (i))
        fprintf (plot_stream, ", \"-\" %s %s %s \\\n",
                 usingclause{i}, titlespec{i}, withclause{i});
      elseif (is_image_data (i))
        fprintf (plot_stream, "%s \"-\" %s %s %s \\\n", ",",
                 usingclause{i}, titlespec{i}, withclause{i});
      else
        fprintf (plot_stream, ", \"-\" binary format='%%float64' %s %s %s \\\n",
                 usingclause{i}, titlespec{i}, withclause{i});
      endif
    endfor
    fputs (plot_stream, ";\n");
    for i = 1:data_idx
      if (have_3d_patch (i))
        ## Can't write 3d patch data as binary as can't plot more than
        ## a single patch at a time and have to plot all patches together
        ## so that the gnuplot depth ordering is done correctly
        for j = 1 : 4 : columns (data{i})
          if (j != 1)
            fputs (plot_stream, "\n\n");
          endif
          fprintf (plot_stream, "%.15g %.15g %.15g %.15g\n", data{i}(:,j).');
          fprintf (plot_stream, "%.15g %.15g %.15g %.15g\n\n", data{i}(:,j+1).');
          fprintf (plot_stream, "%.15g %.15g %.15g %.15g\n", data{i}(:,j+2).');
          fprintf (plot_stream, "%.15g %.15g %.15g %.15g\n", data{i}(:,j+3).');
        endfor
        fputs (plot_stream, "e\n");
      elseif (is_image_data(i))
        fwrite (plot_stream, data{i}, "float32");
      else
        __gnuplot_write_data__ (plot_stream, data{i}, nd, parametric(i),
                                have_cdata(i));
      endif
    endfor
    fputs (plot_stream, "\n");
  else
    fputs (plot_stream, "plot \"-\";\nInf Inf\ne\n");
  endif

  ## Needed to allow mouse rotation if gnuplot was put in map view.
  if (view_map && rot_x == 0 && rot_z == 0)
    fputs (plot_stream, "set view 0,0;\n");
  endif

  ## Undo the aspect ratio lock imposed by the gnuplot command sequence:
  ##   "set view equal xy; set view map"
  ## See bug #40686: "Incorrect colorbar size"
  if (nd == 3
      && strcmp (axis_obj.dataaspectratiomode, "manual")
      && axis_obj.dataaspectratio(1) == axis_obj.dataaspectratio(2))
    fprintf (plot_stream, "set size noratio;\n");
  endif

  if (bg_is_set)
    fputs (plot_stream, ['if (GPVAL_TERM eq "qt") unset obj 1;' "\n"]);
    bg_is_set = false;
  endif

  fflush (plot_stream);

endfunction

function x = flip (x)

  if (rows (x) == 1)
    x = fliplr (x);
  elseif (columns (x) == 1 || ischar (x))
    x = flipud (x);
  else
    x = flipud (fliplr (x));
  endif

endfunction

function spacing_spec = create_spacingspec (f, s, gp_term)

  ## The gnuplot default font size is 10, and default spacing is 1.25.
  ## gnuplot has a concept of a figure global font, and sizes everything
  ## appropriate to that, including the legend spacing.
  ##
  ## This means that if an alternative size is used, gnuplot will use an
  ## inappropriate spacing in the legend by default.
  ##
  ## FIXME: Are fractional spacing specifications allowed?  Or should this
  ##        number be rounded?
  spc = s / 10 * 1.25;
  spacing_spec = sprintf ("spacing %d", spc);

endfunction

function fontspec = create_fontspec (f, s, gp_term)

  if (isempty (f) || strcmp (f, "*") || strcmp (gp_term, "tikz"))
    fontspec = sprintf ('font ",%d"', s);
  else
    fontspec = sprintf ('font "%s,%d"', f, s);
  endif

endfunction

function idx = do_border_2d (obj, plot_stream, idx)

  fprintf (plot_stream, "set border 0\n");
  fprintf (plot_stream, "unset arrow\n");

  if (strcmp (obj.box, "on") || strcmp (obj.xaxislocation, "bottom"))
    arrow (1, obj.xcolor, obj.linewidth, [0,0,0], [1,0,0]);
  endif
  if (strcmp (obj.box, "on") || strcmp (obj.xaxislocation, "top"))
    arrow (2, obj.xcolor, obj.linewidth, [0,1,0], [1,1,0]);
  endif
  if (strcmp (obj.box, "on") || strcmp (obj.yaxislocation, "left"))
    arrow (3, obj.ycolor, obj.linewidth, [0,0,0], [0,1,0]);
  endif
  if (strcmp (obj.box, "on") || strcmp (obj.yaxislocation, "right"))
    arrow (4, obj.ycolor, obj.linewidth, [1,0,0], [1,1,0]);
  endif

  if (strcmp (obj.xaxislocation, "origin"))
    idx = zeroaxis (idx, obj.xcolor, "x");
  endif
  if (strcmp (obj.yaxislocation, "origin"))
    idx = zeroaxis (idx, obj.ycolor, "y");
  endif

  function idx = zeroaxis (idx, lc, ax)
    idx = idx + 1;
    [~, ltidx] = do_linestyle_command (obj, lc, idx, plot_stream);
    fprintf (plot_stream, "set %szeroaxis ls %d ", ax, ltidx);
    fprintf (plot_stream, "lw %.3f\n", obj.linewidth);
  endfunction

  function arrow (idx, lc, lw, from, to)
    fprintf (plot_stream, "set arrow %d ", idx);
    fprintf (plot_stream, "nohead nofilled front ");
    fprintf (plot_stream, "lc rgb ""#%02x%02x%02x"" ", round (255 * lc));
    fprintf (plot_stream, "linewidth %.3f ", obj.linewidth);
    fprintf (plot_stream, "from graph %d,%d,%d ", from);
    fprintf (plot_stream, "to graph %d,%d,%d\n", to);
  endfunction

endfunction

function idx = do_border_tick_3d (obj, plot_stream, idx)

  ## axis location has no effect

  if (strcmp (obj.box, "on"))
    fputs (plot_stream, "set border 0xFFF;\n");
    mirrorstr = "mirror";
  else
    fputs (plot_stream, "set border 0x15;\n");
    mirrorstr = "nomirror";
  endif

  tick ('x', obj.xcolor, obj.tickdir, mirrorstr);
  tick ('y', obj.ycolor, obj.tickdir, mirrorstr);
  tick ('z', obj.zcolor, obj.tickdir, mirrorstr);

  function tick (axischar, color, tickdir, mirrorstr)

    if (isnumeric (color))
      if (length (color) == 3)
        colorspec = sprintf ('rgb "#%02x%02x%02x"', round (255*color));
      else
        colorspec = sprintf ("palette %d", round (color));
      endif
    else
      colorspec = sprintf ('"%s"', color);
    endif
    fprintf (plot_stream, "set %ctics %s %s textcolor %s\n",
             axischar, tickdir, mirrorstr, colorspec);

  endfunction

endfunction

function [style, ltidx] = do_linestyle_command (obj, linecolor, idx,
                                                plot_stream)

  idx = idx + 8;
  style = {};
  ltidx = [];

  fprintf (plot_stream, "set style line %d default;\n", idx);
  fprintf (plot_stream, "set style line %d", idx);

  found_style = false;
  if (isnumeric (linecolor))
    color = linecolor;
    if (isfield (obj, "alpha")
        &&  __gnuplot_has_feature__ ("alphablend_linecolor"))
      alphastr = sprintf ("%02x", round (255*(1-obj.alpha)));
    else
      alphastr = "";
    endif
    fprintf (plot_stream, ' linecolor rgb "#%s%02x%02x%02x"',
             alphastr, round (255*color));
  else
    color = [0, 0, 0];
    flat_interp_edge = (strcmp (obj.edgecolor, "flat")
                        || strcmp (obj.edgecolor, "interp"));
    if (flat_interp_edge)
        fprintf (plot_stream, " palette");
    endif
  endif

  lt = gnuplot_linestyletype (obj);
  if (! isempty (lt))
    fprintf (plot_stream, " %s", lt);
  endif

  if (isfield (obj, "linewidth"))
    fprintf (plot_stream, " linewidth %f", obj.linewidth);
    found_style = true;
  endif

  [pt, pt2, obj] = gnuplot_pointtype (obj);

  if (! isempty (pt))
    found_style = true;
  endif

  sidx = 1;
  if (isempty (lt))
    style{sidx} = "";
  else
    style{sidx} = "lines";
  endif
  ltidx(sidx) = idx;

  facesame = true;
  if (! isequal (pt, pt2) && isfield (obj, "markerfacecolor")
      && ! strcmp (obj.markerfacecolor, "none"))
    if (strcmp (obj.markerfacecolor, "auto")
        || (isnumeric (obj.markerfacecolor)
            && isequal (color, obj.markerfacecolor)))
      if (! isempty (pt2))
        fprintf (plot_stream, " pointtype %s", pt2);
        style{sidx} = [style{sidx} "points"];
      endif
      if (isfield (obj, "markersize"))
        fprintf (plot_stream, " pointsize %f", obj.markersize / 3);
      endif
    else
      facesame = false;
      if (! found_style)
        fputs (plot_stream, " default");
      endif
      fputs (plot_stream, ";\n");
      if (! isempty (style{sidx}))
        sidx += 1;
        idx += 1;
      else
        fputs (plot_stream, ";\n");
      endif
      fprintf (plot_stream, "set style line %d default;\n", idx);
      fprintf (plot_stream, "set style line %d", idx);
      if (isnumeric (obj.markerfacecolor))
        fprintf (plot_stream, ' linecolor rgb "#%02x%02x%02x"',
                 round (255*obj.markerfacecolor));
      else
        fprintf (plot_stream, " palette");
      endif
      if (! isempty (pt2))
        style{sidx} = "points";
        ltidx(sidx) = idx;
        fprintf (plot_stream, " pointtype %s", pt2);
      endif
      if (isfield (obj, "markersize"))
        fprintf (plot_stream, " pointsize %f", obj.markersize / 3);
      endif
    endif
  endif
  if (! isempty (pt) && isfield (obj, "markeredgecolor")
      && ! strcmp (obj.markeredgecolor, "none"))
    if (facesame && (strcmp (obj.markeredgecolor, "auto")
        || (isnumeric (obj.markeredgecolor)
            && isequal (color, obj.markeredgecolor))))
      if (sidx == 1 && ((length (style{sidx}) == 5
          && strncmp (style{sidx}, "lines", 5)) || isempty (style{sidx})))
        style{sidx} = [style{sidx} "points"];
        fprintf (plot_stream, " pointtype %s", pt);
        if (isfield (obj, "markersize"))
          fprintf (plot_stream, " pointsize %f", obj.markersize / 3);
        endif
      endif
    else
      if (! found_style)
        fputs (plot_stream, " default");
      endif
      fputs (plot_stream, ";\n");
      if (! isempty (style{sidx}))
        sidx += 1;
        idx += 1;
      else
        fputs (plot_stream, ";\n");
      endif
      fprintf (plot_stream, "set style line %d default;\n", idx);
      fprintf (plot_stream, "set style line %d", idx);
      if (isnumeric (obj.markeredgecolor) || strcmp (obj.markeredgecolor, "auto"))
        if (isnumeric (obj.markeredgecolor))
          edgecolor = obj.markeredgecolor;
        else
          edgecolor = obj.color;
        endif
        fprintf (plot_stream, ' linecolor rgb "#%02x%02x%02x"',
                 round (255*edgecolor));
      else
        fprintf (plot_stream, " palette");
      endif
      style{sidx} = "points";
      ltidx(sidx) = idx;
      fprintf (plot_stream, " pointtype %s", pt);
      if (isfield (obj, "markersize"))
        fprintf (plot_stream, " pointsize %f", obj.markersize / 3);
      endif
    endif
  endif

  if (! found_style && isempty (style{1}))
    fputs (plot_stream, " default");
  endif

  fputs (plot_stream, ";\n");

endfunction

function lt = gnuplot_linestyletype (obj)

  if (isfield (obj, "linestyle"))
    if (__gnuplot_has_feature__ ("dashtype"))
      opt = "dashtype";
      switch (obj.linestyle)
        case "-"
          lt = "solid";
        case "--"
          lt = "'_ '";
        case ":"
          lt = "'. '";
        case "-."
          lt = "'-. '";
        case "none"
          lt = "";
        otherwise
          lt = "";
      endswitch
    else
      opt = "linetype";
      switch (obj.linestyle)
        case "-"
          lt = "1";
        case "--"
          lt = "2";
        case ":"
          lt = "3";
        case "-."
          lt = "6";
        case "none"
          lt = "";
        otherwise
          lt = "";
      endswitch
    endif
    if (! isempty (lt))
      lt = sprintf ("%s %s", opt, lt);
    endif
  else
    lt = "";
  endif

endfunction

function [pt, pt2, obj] = gnuplot_pointtype (obj)

  if (isfield (obj, "marker"))
    switch (obj.marker)
      case "+"
        pt = pt2 = "1";
      ## FIXME: It's not clear how to add support for these markers in gnuplot
      #{
      case "|"
        pt = "1";
        pt2 = "1";
      case "_"
        pt = "1";
        pt2 = "1";
      #}
      case "o"
        pt = "6";
        pt2 = "7";
      case "*"
        pt = pt2 = "3";
      case "."
        pt = pt2 = "7";
        if (isfield (obj, "markersize"))
          obj.markersize /= 3;
        else
          obj.markersize = 5;
        endif
      case "x"
        pt = pt2 = "2";
      case {"square", "s"}
        pt = "4";
        pt2 = "5";
      case {"diamond", "d"}
        pt = "12";
        pt2 = "13";
      case "^"
        pt = "8";
        pt2 = "9";
      case "v"
        pt = "10";
        pt2 = "11";
      case ">"
        ## FIXME: Should be triangle pointing right, use triangle pointing up
        pt = "8";
        pt2 = "9";
      case "<"
        ## FIXME: Should be triangle pointing left, use triangle pointing down
        pt = "10";
        pt2 = "11";
      case {"pentagram", "p"}
        ## FIXME: Should be pentagram, using pentagon
        pt = "14";
        pt2 = "15";
      case {"hexagram", "h"}
        ## FIXME: Should be 6 pt start, using "*" instead
        pt = pt2 = "3";
      case "none"
        pt = pt2 = "-1";
      otherwise
        pt = pt2 = "";
    endswitch
  else
    pt = pt2 = "";
  endif

endfunction

function __gnuplot_write_data__ (plot_stream, data, nd, parametric, cdata)

  ## DATA is already transposed.

  ## Convert NA elements to normal NaN values because fprintf writes
  ## "NA" and that confuses gnuplot.
  data(isna (data)) = NaN;

  if (nd == 2)
    fwrite (plot_stream, data, "float64");
  elseif (nd == 3)
    if (parametric)
      fwrite (plot_stream, data, "float64");
    else
      nr = rows (data);
      if (cdata)
        for j = 1:4:nr
          fwrite (plot_stream, data(j:j+3,:), "float64");
        endfor
      else
        for j = 1:3:nr
          fwrite (plot_stream, data(j:j+2,:), "float64");
        endfor
      endif
    endif
  endif

endfunction

function do_tics (obj, plot_stream, ymirror, gnuplot_term)

  obj.xticklabel = ticklabel_to_cell (obj.xticklabel);
  obj.yticklabel = ticklabel_to_cell (obj.yticklabel);
  obj.zticklabel = ticklabel_to_cell (obj.zticklabel);

  if (strcmp (obj.xminorgrid, "on"))
    obj.xminortick = "on";
  endif
  if (strcmp (obj.yminorgrid, "on"))
    obj.yminortick = "on";
  endif
  if (strcmp (obj.zminorgrid, "on"))
    obj.zminortick = "on";
  endif

  [fontname, fontsize] = get_fontname_and_size (obj);
  fontspec = create_fontspec (fontname, fontsize, gnuplot_term);

  ## A Gnuplot tic scale of 69 is equivalent to Octave's 0.5.
  ticklength = sprintf ("scale %4.1f", (69/0.5)*obj.ticklength(1));

  if (strcmp (obj.xaxislocation, "top"))
    do_tics_1 (obj.xtickmode, obj.xtick, obj.xminortick, obj.xticklabelmode,
               obj.xticklabel, obj.xcolor, "x2", plot_stream, true,
               "border", obj.tickdir, ticklength, fontname, fontspec,
               obj.ticklabelinterpreter, obj.xscale, obj.xsgn, gnuplot_term);
    do_tics_1 ("manual", [], "off", obj.xticklabelmode, obj.xticklabel,
               obj.xcolor, "x", plot_stream, true, "border",
               "", "", fontname, fontspec, obj.ticklabelinterpreter,
               obj.xscale, obj.xsgn, gnuplot_term);
  elseif (strcmp (obj.xaxislocation, "origin"))
    do_tics_1 (obj.xtickmode, obj.xtick, obj.xminortick, obj.xticklabelmode,
               obj.xticklabel, obj.xcolor, "x", plot_stream, true,
               "axis", obj.tickdir, ticklength, fontname, fontspec,
               obj.ticklabelinterpreter, obj.xscale, obj.xsgn, gnuplot_term);
    do_tics_1 ("manual", [], "off", obj.xticklabelmode, obj.xticklabel,
               obj.xcolor, "x2", plot_stream, true, "axis",
               "", "", fontname, fontspec, obj.ticklabelinterpreter,
               obj.xscale, obj.xsgn, gnuplot_term);
  else
    do_tics_1 (obj.xtickmode, obj.xtick, obj.xminortick, obj.xticklabelmode,
               obj.xticklabel, obj.xcolor, "x", plot_stream, true,
               "border", obj.tickdir, ticklength, fontname, fontspec,
               obj.ticklabelinterpreter, obj.xscale, obj.xsgn, gnuplot_term);
    do_tics_1 ("manual", [], "off", obj.xticklabelmode, obj.xticklabel,
               obj.xcolor, "x2", plot_stream, true, "border",
               "", "", fontname, fontspec, obj.ticklabelinterpreter,
               obj.xscale, obj.xsgn, gnuplot_term);
  endif
  if (strcmp (obj.yaxislocation, "right"))
    do_tics_1 (obj.ytickmode, obj.ytick, obj.yminortick, obj.yticklabelmode,
               obj.yticklabel, obj.ycolor, "y2", plot_stream, ymirror,
               "border", obj.tickdir, ticklength, fontname, fontspec,
               obj.ticklabelinterpreter, obj.yscale, obj.ysgn, gnuplot_term);
    do_tics_1 ("manual", [], "off", obj.yticklabelmode, obj.yticklabel,
               obj.ycolor, "y", plot_stream, ymirror, "border",
               "", "", fontname, fontspec, obj.ticklabelinterpreter,
               obj.yscale, obj.ysgn, gnuplot_term);
  elseif (strcmp (obj.yaxislocation, "origin"))
    do_tics_1 (obj.ytickmode, obj.ytick, obj.yminortick, obj.yticklabelmode,
               obj.yticklabel, obj.ycolor, "y", plot_stream, ymirror,
               "axis", obj.tickdir, ticklength, fontname, fontspec,
               obj.ticklabelinterpreter, obj.yscale, obj.ysgn, gnuplot_term);
    do_tics_1 ("manual", [], "off", obj.yticklabelmode, obj.yticklabel,
               obj.ycolor, "y2", plot_stream, ymirror, "axis",
               "", "", fontname, fontspec, obj.ticklabelinterpreter,
               obj.yscale, obj.ysgn, gnuplot_term);
  else
    do_tics_1 (obj.ytickmode, obj.ytick, obj.yminortick, obj.yticklabelmode,
               obj.yticklabel, obj.ycolor, "y", plot_stream, ymirror,
               "border", obj.tickdir, ticklength, fontname, fontspec,
               obj.ticklabelinterpreter, obj.yscale, obj.ysgn, gnuplot_term);
    do_tics_1 ("manual", [], "off", obj.yticklabelmode, obj.yticklabel,
               obj.ycolor, "y2", plot_stream, ymirror, "border",
               "", "", fontname, fontspec, obj.ticklabelinterpreter,
               obj.yscale, obj.ysgn, gnuplot_term);
  endif
  do_tics_1 (obj.ztickmode, obj.ztick, obj.zminortick, obj.zticklabelmode,
             obj.zticklabel, obj.zcolor, "z", plot_stream, true,
             "border", obj.tickdir, ticklength, fontname, fontspec,
             obj.ticklabelinterpreter, obj.zscale, obj.zsgn, gnuplot_term);

endfunction

function do_tics_1 (ticmode, tics, mtics, labelmode, labels, color, ax,
                    plot_stream, mirror, axispos, tickdir, ticklength,
                    fontname, fontspec, interpreter, scale, sgn, gnuplot_term)
  persistent warned_latex = false;

  ## Avoid emitting anything if the tics are empty, because this undoes the
  ## effect of the previous unset xtics and thereby adds back in the tics.
  if (isempty (tics))
    return;
  endif

  if (mirror)
    mirror = "mirror";
  else
    mirror = "nomirror";
  endif
  if (strcmp (interpreter, "tex"))
    for n = 1 : numel (labels)
      labels{n} = __tex2enhanced__ (labels{n}, fontname, false, false, ...
                                    gnuplot_term);
    endfor
  elseif (strcmp (interpreter, "latex"))
    if (! warned_latex)
      do_warn = (warning ("query", "Octave:text_interpreter")).state;
      if (strcmp (do_warn, "on"))
        warning ("Octave:text_interpreter",
                 "latex markup not supported for tick marks");
        warned_latex = true;
      endif
    endif
  endif
  if (strcmp (scale, "log"))
    num_mtics = 10;
    if (any (strcmp (gnuplot_term, {"cairolatex", "eepic", "epslatex", ...
        "latex", "pslatex", "pstex", "pstricks", "texdraw", "tikz"})))
      fmt = "$10^{%T}$";
    else
      fmt = "10^{%T}";
    endif
    if (sgn < 0)
      fmt = ["-" fmt];
    endif
  else
    fmt = "%g";
    num_mtics = 5;
  endif
  colorspec = get_text_colorspec (color);
  fprintf (plot_stream, ['set format %s "%s";' "\n"], ax, fmt);
  if (strcmp (ticmode, "manual") && isempty (tics))
    fprintf (plot_stream, "unset %stics;\nunset m%stics;\n", ax, ax);
    return;
  else
    k = 1;
    ntics = numel (tics);
    labels(end+1:1) = {""};
    labels = repmat (labels(:), ceil (ntics/numel (labels)), 1);
    fprintf (plot_stream, "set %stics %s %s %s %s (", ax,
             tickdir, ticklength, axispos, mirror);
    labels = strrep (labels, "%", "%%");
    for i = 1:ntics
      fprintf (plot_stream, ' "%s" %.15f', labels{k++}, tics(i));
      if (i < ntics)
        fputs (plot_stream, ", ");
      endif
    endfor
    fprintf (plot_stream, ") %s %s;\n", colorspec, fontspec);
  endif
  if (strcmp (mtics, "on"))
    fprintf (plot_stream, "set m%stics %d;\n", ax, num_mtics);
  else
    fprintf (plot_stream, "unset m%stics;\n", ax);
  endif

endfunction

function ticklabel = ticklabel_to_cell (ticklabel)

  if (ischar (ticklabel))
    ticklabel = cellstr (ticklabel);
  elseif (iscellstr (ticklabel))
    ticklabel = ticklabel;
  else
    error ("__gnuplot_draw_axes__: unsupported type of ticklabel");
  endif

endfunction

function colorspec = get_text_colorspec (color)
  colorspec = sprintf ('textcolor rgb "#%02x%02x%02x"', round (255*color));
endfunction

function [f, s, fnt, it, bld] = get_fontname_and_size (t)

  if (isempty (t.fontname) || strcmp (t.fontname, "*"))
    if (ispc ())
      ## FIXME: Should really test for "windows" terminal which is the
      ## only terminal to have a problem with a null font specification.
      ## See Bug #49135.
      fnt = "Arial";
    else
      fnt = "";
    endif
  else
    fnt = t.fontname;
  endif

  f = fnt;
  it = false;
  bld = false;
  if (! isempty (t.fontweight) && strcmp (t.fontweight, "bold"))
    if (! isempty (t.fontangle)
        && (strcmp (t.fontangle, "italic")
            || strcmp (t.fontangle, "oblique")))
      if (__gnuplot_has_feature__ ("fontspec_5"))
        f = [f ":Bold:Italic"];
      else
        f = [f "-bolditalic"];
      endif

      it = true;
      bld = true;
    else
      if (__gnuplot_has_feature__ ("fontspec_5"))
        f = [f ":Bold"];
      else
        f = [f "-bold"];
      endif

      bld = true;
    endif
  elseif (! isempty (t.fontangle)
          && (strcmp (t.fontangle, "italic")
              || strcmp (t.fontangle, "oblique")))
    if (__gnuplot_has_feature__ ("fontspec_5"))
      f = [f ":Italic"];
    else
      f = [f "-italic"];
    endif

    it = true;
  endif

  if (isempty (t.fontsize))
    s = 10;
  else
    s = t.fontsize;
  endif

endfunction

function [str, f, s] = __maybe_munge_text__ (enhanced, obj, fld, ntrp, ...
                                             gnuplot_term)
  persistent warned_latex = false;

  if (strcmp (fld, "string"))
    [f, s, fnt, it, bld] = get_fontname_and_size (obj);
  else
    f = "Helvetica";
    s = 10;
    fnt = f;
    it = false;
    bld = false;
  endif

  ## The text object may be multiline, and may be of any class
  str = getfield (obj, fld);
  if (ischar (str) && rows (str) > 1)
    str = cellstr (str);
  elseif (isnumeric (str))
    str = cellstr (num2str (str(:)));
  endif
  if (iscellstr (str))
    for n = 1:numel (str)
      if (isnumeric (str{n}))
        str{n} = num2str (str{n});
      endif
    endfor
    str = sprintf ("%s\n", str{:})(1:end-1);
  endif

  if (enhanced)
    str = regexprep (str, '(?<!\\)@', '\\@');
  endif

  if (enhanced)
    if (strcmp (ntrp, "tex"))
      if (iscellstr (str))
        for n = 1:numel (str)
          str{n} = __tex2enhanced__ (str{n}, fnt, it, bld, gnuplot_term);
        endfor
      else
        str = __tex2enhanced__ (str, fnt, it, bld, gnuplot_term);
      endif
    elseif (strcmp (ntrp, "latex"))
      if (! warned_latex)
        do_warn = (warning ("query", "Octave:text_interpreter")).state;
        if (strcmp (do_warn, "on"))
          warning ("Octave:text_interpreter",
                   "latex markup not supported for text objects");
          warned_latex = true;
        endif
      endif
    endif
  endif

endfunction

function str = __tex2enhanced__ (str, fnt, it, bld, gnuplot_term)
  persistent sym = __setup_sym_table__ ();
  persistent flds = fieldnames (sym);

  if (any (strcmp (gnuplot_term, {"postscript", "epscairo"})))
    symtype = 1;
  else
    symtype = 2;
  endif

  [s, e, m] = regexp (str, "\\\\([a-zA-Z]+|0)", "start", "end", "matches");

  for i = length (s) : -1 : 1
    ## special case for "\0"  and replace with empty set equivalent
    if (strncmp (m{i}, '\0', 2))
      str = [str(1:s(i) - 1) sym.emptyset{symtype} str(s(i) + 2:end)];
    else
      f = m{i}(2:end);
      if (isfield (sym, f))
        g = sym.(f){symtype};
        ## FIXME: The symbol font doesn't seem to support bold or italic
        ##if (bld)
        ##  if (it)
        ##    g = strrep (g, '/Symbol', '/Symbol-bolditalic');
        ##  else
        ##    g = strrep (g, '/Symbol', '/Symbol-bold');
        ##  endif
        ##elseif (it)
        ##  g = strrep (g, '/Symbol', '/Symbol-italic');
        ##endif
        str = [str(1:s(i) - 1) g str(e(i) + 1:end)];
      elseif (strncmp (f, "rm", 2))
        bld = false;
        it = false;
        str = [str(1:s(i) - 1) '{/' fnt ' ' str(s(i) + 3:end) '}'];
      elseif (strncmp (f, "it", 2) || strncmp (f, "sl", 2))
        it = true;
        if (__gnuplot_has_feature__ ("fontspec_5"))
          if (bld)
            str = [str(1:s(i)-1) '{/' fnt ':Bold:Italic ' str(s(i)+3:end) '}'];
          else
            str = [str(1:s(i)-1) '{/' fnt ':Italic ' str(s(i)+3:end) '}'];
          endif
        else
          if (bld)
            str = [str(1:s(i)-1) '{/' fnt '-bolditalic ' str(s(i)+3:end) '}'];
          else
            str = [str(1:s(i)-1) '{/' fnt '-italic ' str(s(i)+3:end) '}'];
          endif
        endif
      elseif (strncmp (f, "bf", 2))
        bld = true;
        if (__gnuplot_has_feature__ ("fontspec_5"))
          if (it)
            str = [str(1:s(i)-1) '{/' fnt ':Bold:Italic ' str(s(i)+3:end) '}'];
          else
            str = [str(1:s(i)-1) '{/' fnt ':Bold ' str(s(i)+3:end) '}'];
          endif
        else
          if (it)
            str = [str(1:s(i)-1) '{/' fnt '-bolditalic ' str(s(i)+3:end) '}'];
          else
            str = [str(1:s(i)-1) '{/' fnt '-bold ' str(s(i)+3:end) '}'];
          endif
        endif
      elseif (strcmp (f, "color"))
        ## FIXME: Ignore \color but remove trailing {} block as well
        d = strfind (str(e(i) + 1:end),'}');
        if (isempty (d))
          warning ('syntax error in \color argument');
        else
          str = [str(1:s(i) - 1) str(e(i) + d + 1:end)];
        endif
      elseif (strcmp (f, "fontname"))
        b1 = strfind (str(e(i) + 1:end),'{');
        b2 = strfind (str(e(i) + 1:end),'}');
        if (isempty (b1) || isempty (b2))
          warning ('syntax error in \fontname argument');
        else
          str = [str(1:s(i) - 1), '/', str(e(i)+b1(1) + 1:e(i)+b2(1)-1), ...
                 '{}', str(e(i) + b2(1) + 1:end)];
        endif
      elseif (strcmp (f, "fontsize"))
        b1 = strfind (str(e (i) + 1:end),'{');
        b2 = strfind (str(e (i) + 1:end),'}');
        if (isempty (b1) || isempty (b2))
          warning ('syntax error in \fontname argument');
        else
          str = [str(1:s(i) - 1), '/=', str(e(i)+b1(1) + 1:e(i)+b2(1)-1), ...
                 '{}', str(e(i) + b2(1) + 1:end)];
        endif
      else
        ## Last desperate attempt to treat the symbol.  Look for things
        ## like \pix, that should be translated to the symbol Pi and x
        for j = 1 : length (flds)
          if (strncmp (flds{j}, f, length (flds{j})))
            g = sym.(flds{j}){symtype};
            ## FIXME: The symbol font doesn't seem to support bold or italic
            ##if (bld)
            ##  if (it)
            ##    g = strrep (g, '/Symbol', '/Symbol-bolditalic');
            ##  else
            ##    g = strrep (g, '/Symbol', '/Symbol-bold');
            ##  endif
            ##elseif (it)
            ##  g = strrep (g, '/Symbol', '/Symbol-italic');
            ##endif
            str = [str(1:s(i) - 1) g str(s(i) + length (flds{j}) + 1:end)];
            break;
          endif
        endfor
      endif
    endif
  endfor

  ## Prepend @ to things like _0^x or _{-100}^{100} for alignment.
  ## But need to put the shorter of the two arguments first.
  ## Careful of nested {} and unprinted characters when defining
  ## shortest..  Don't have to worry about things like ^\theta as they
  ## are already converted.

  ## FIXME: This is a mess.  Is it worth it just for a "@" character?

  [s, m] = regexp (str,'[_\^]','start','matches');
  i = 1;
  p = 0;
  while (i < length (s))
    if (i < length (s))
      if (str(s(i) + p + 1) == "{")
        s1 = strfind (str(s(i) + p + 2:end),'{');
        si = 1;
        l1 = strfind (str(s(i) + p + 1:end),'}');
        li = 1;
        while (li <= length (l1) && si <= length (s1))
          if (l1(li) < s1(si))
            if (li == si)
              break;
            endif
            li += 1;
          else
            si += 1;
          endif
        endwhile
        l1 = l1(min (length (l1), si));
        if (s(i) + l1 + 1 == s(i+1))
          if (str(s(i + 1) + p + 1) == "{")
            s2 = strfind (str(s(i + 1) + p + 2:end),'{');
            si = 1;
            l2 = strfind (str(s(i + 1) + p + 1:end),'}');
            li = 1;
            while (li <= length (l2) && si <= length (s2))
              if (l2(li) < s2(si))
                if (li == si)
                  break;
                endif
                li += 1;
              else
                si += 1;
              endif
            endwhile
            l2 = l2(min (length (l2), si));
            if (length_string (str(s(i)+p+2:s(i)+p+l1-1)) <=
                length_string (str(s(i+1)+p+2:s(i+1)+p+l2-1)))
              ## Shortest already first!
              str = [str(1:s(i)+p-1) "@" str(s(i)+p:end)];
            else
              ## Have to swap sub/super-script to get shortest first.
              str = [str(1:s(i)+p-1), "@", str(s(i+1)+p:s(i+1)+p+l2), ...
                     str(s(i)+p:s(i)+p+l1), str(s(i+1)+p+l2+1:end)];
            endif
          else
            ## Have to swap sub/super-script to get shortest first.
            str = [str(1:s(i)+p-1), "@", str(s(i+1)+p:s(i+1)+p+1), ...
                   str(s(i)+p:s(i)+p+l1), str(s(i+1)+p+2:end)];
          endif
          i += 2;
          p += 1;
        else
          i += 1;
        endif
      else
        if (s(i+1) == s(i) + 2)
          ## Shortest already first!
          str = [str(1:s(i)+p-1) "@" str(s(i)+p:end)];
          p += 1;
          i += 2;
        else
          i += 1;
        endif
      endif
    else
      i += 1;
    endif
  endwhile

endfunction

function l = length_string (s)

  l = length (s) - length (strfind (s,'{')) - length (strfind (s,'}'));
  m = regexp (s, '/([\w-]+|[\w-]+=\d+)', 'matches');
  if (! isempty (m))
    l -= sum (cellfun ("length", m));
  endif

endfunction

function sym = __setup_sym_table__ ()

  ## Setup the translation table for TeX to gnuplot enhanced mode.
  sym.forall = {'{/Symbol \042}', '∀'};
  sym.exists = {'{/Symbol \044}', '∃'};
  sym.ni = {'{/Symbol \047}', '∋'};
  sym.cong = {'{/Symbol \100}', '≅'};
  sym.Delta = {'{/Symbol D}', 'Δ'};
  sym.Phi = {'{/Symbol F}', 'Φ'};
  sym.Gamma = {'{/Symbol G}', 'Γ'};
  sym.vartheta = {'{/Symbol J}', 'ϑ'};
  sym.Lambda = {'{/Symbol L}', 'Λ'};
  sym.Pi = {'{/Symbol P}', 'Π'};
  sym.Theta = {'{/Symbol Q}', 'Θ'};
  sym.Sigma = {'{/Symbol S}', 'Σ'};
  sym.varsigma = {'{/Symbol V}', 'ς'};
  sym.Omega = {'{/Symbol W}', 'Ω'};
  sym.Xi = {'{/Symbol X}', 'Ξ'};
  sym.Psi = {'{/Symbol Y}', 'Ψ'};
  sym.perp = {'{/Symbol \136}', '⊥'};
  sym.alpha = {'{/Symbol a}', 'α'};
  sym.beta = {'{/Symbol b}', 'β'};
  sym.chi = {'{/Symbol c}', 'χ'};
  sym.delta = {'{/Symbol d}', 'δ'};
  sym.epsilon = {'{/Symbol e}', 'ε'};
  sym.phi = {'{/Symbol f}', 'ϕ'};
  sym.gamma = {'{/Symbol g}', 'γ'};
  sym.eta = {'{/Symbol h}', 'η'};
  sym.iota = {'{/Symbol i}', 'ι'};
  sym.varphi = {'{/Symbol j}', 'φ'};              # Not in OpenGL
  sym.kappa = {'{/Symbol k}', 'κ'};
  sym.lambda = {'{/Symbol l}', 'λ'};
  sym.mu = {'{/Symbol m}', 'μ'};
  sym.nu = {'{/Symbol n}', 'ν'};
  sym.o = {'{/Symbol o}', 'ο'};
  sym.pi = {'{/Symbol p}', 'π'};
  sym.theta = {'{/Symbol q}', 'θ'};
  sym.rho = {'{/Symbol r}', 'ρ'};
  sym.sigma = {'{/Symbol s}', 'σ'};
  sym.tau = {'{/Symbol t}', 'τ'};
  sym.upsilon = {'{/Symbol u}', 'υ'};
  sym.varpi = {'{/Symbol v}', 'ϖ'};
  sym.omega = {'{/Symbol w}', 'ω'};
  sym.xi = {'{/Symbol x}', 'ξ'};
  sym.psi = {'{/Symbol y}', 'ψ'};
  sym.zeta = {'{/Symbol z}', 'ζ'};
  sym.sim = {'{/Symbol \176}', '∼'};
  sym.Upsilon = {'{/Symbol \241}', 'Υ'};
  sym.prime = {'{/Symbol \242}', '′'};
  sym.leq = {'{/Symbol \243}', '≤'};
  sym.infty = {'{/Symbol \245}', '∞'};
  sym.clubsuit = {'{/Symbol \247}', '♣'};
  sym.diamondsuit = {'{/Symbol \250}', '♢'};
  sym.heartsuit = {'{/Symbol \251}', '♡'};
  sym.spadesuit = {'{/Symbol \252}', '♠'};
  sym.leftrightarrow = {'{/Symbol \253}', '↔'};
  sym.leftarrow = {'{/Symbol \254}', '←'};
  sym.uparrow = {'{/Symbol \255}', '↑'};
  sym.rightarrow = {'{/Symbol \256}', '→'};
  sym.downarrow = {'{/Symbol \257}', '↓'};
  sym.circ = {'{/Symbol \260}', '∘'};
  ## degree symbol, not circ as in FLTK
  sym.deg = {'{/Symbol \260}', '°'};
  sym.ast = {'{/Symbol *}', '∗'};
  sym.pm = {'{/Symbol \261}', '±'};
  sym.geq = {'{/Symbol \263}', '≥'};
  sym.times = {'{/Symbol \264}', '×'};
  sym.propto = {'{/Symbol \265}', '∝'};
  sym.partial = {'{/Symbol \266}', '∂'};
  sym.bullet = {'{/Symbol \267}', '∙'};
  sym.div = {'{/Symbol \270}', '÷'};
  sym.neq = {'{/Symbol \271}', '≠'};
  sym.equiv = {'{/Symbol \272}', '≡'};
  sym.approx = {'{/Symbol \273}', '≈'};
  sym.ldots = {'{/Symbol \274}', '…'};
  sym.mid = {'{/Symbol \275}', '∣'};
  sym.aleph = {'{/Symbol \300}', 'ℵ'};
  sym.Im = {'{/Symbol \301}', 'ℑ'};
  sym.Re = {'{/Symbol \302}', 'ℜ'};
  sym.wp = {'{/Symbol \303}', '℘'};
  sym.otimes = {'{/Symbol \304}', '⊗'};
  sym.oplus = {'{/Symbol \305}', '⊕'};
  ## empty set, not circled slash division operator as in FLTK.
  sym.oslash = {'{/Symbol \306}', '⊘'};
  sym.emptyset = {'{/Symbol \306}', '∅'};
  sym.cap = {'{/Symbol \307}', '∩'};
  sym.cup = {'{/Symbol \310}', '∪'};
  sym.supset = {'{/Symbol \311}', '⊃'};
  sym.supseteq = {'{/Symbol \312}', '⊇'};
  sym.subset = {'{/Symbol \314}', '⊂'};
  sym.subseteq = {'{/Symbol \315}', '⊑'};
  sym.in = {'{/Symbol \316}', '∈'};
  sym.notin = {'{/Symbol \317}', '∋'};            # Not in OpenGL
  sym.angle = {'{/Symbol \320}', '∠'};
  sym.bigtriangledown = {'{/Symbol \321}', '▽'};  # Not in OpenGL
  sym.langle = {'{/Symbol \341}', '〈'};
  sym.rangle = {'{/Symbol \361}', '〉'};
  sym.nabla = {'{/Symbol \321}', '∇'};
  sym.prod = {'{/Symbol \325}', '∏'};             # Not in OpenGL
  sym.surd = {'{/Symbol \326}', '√'};
  sym.cdot = {'{/Symbol \327}', '⋅'};
  sym.neg = {'{/Symbol \330}', '¬'};
  sym.wedge = {'{/Symbol \331}', '∧'};
  sym.vee = {'{/Symbol \332}', '∨'};
  sym.Leftrightarrow = {'{/Symbol \333}', '⇔'};   # Not in OpenGL
  sym.Leftarrow = {'{/Symbol \334}', '⇐'};
  sym.Uparrow = {'{/Symbol \335}', '⇑'};          # Not in OpenGL
  sym.Rightarrow = {'{/Symbol \336}', '⇒'};
  sym.Downarrow = {'{/Symbol \337}', '⇓'};        # Not in OpenGL
  sym.diamond = {'{/Symbol \340}', '⋄'};          # Not in OpenGL
  sym.copyright = {'{/Symbol \343}', '©'};
  sym.lfloor = {'{/Symbol \353}', '⌊'};
  sym.lceil = {'{/Symbol \351}', '⌈'};
  sym.rfloor = {'{/Symbol \373}', '⌋'};
  sym.rceil = {'{/Symbol \371}', '⌉'};
  sym.int = {'{/Symbol \362}', '∫'};

endfunction

function retval = __do_enhanced_option__ (enhanced, obj)

  retval = "";
  if (enhanced)
    if (strcmp (obj.interpreter, "none"))
      retval = "noenhanced";
    else
      retval = "enhanced";
    endif
  endif

endfunction

function do_text (stream, gpterm, enhanced, obj, hax, screenpos)

  [label, f, s] = __maybe_munge_text__ (enhanced, obj, "string", ...
                                        obj.interpreter, gpterm);
  fontspec = create_fontspec (f, s, gpterm);
  lpos = obj.position;
  halign = obj.horizontalalignment;
  valign = obj.verticalalignment;
  angle = obj.rotation;
  units = obj.units;
  color = obj.color;
  if (nargin > 5)
    units = "screen";
    lpos = screenpos;
  elseif (strcmp (units, "normalized"))
    units = "graph";
  elseif (strcmp (get (hax, "yaxislocation"), "right")
          && strcmp (units, "data"))
    units = "second";
  else
    units = "";
  endif

  if (isnumeric (color))
    colorspec = get_text_colorspec (color);
  endif

  if (ischar (obj.string))
    num_lines = rows (obj.string);
    num_lines += numel (strfind (obj.string, "\n"));
  else
    num_lines = numel (obj.string);
  endif
  switch (valign)
    ## Text offset in characters.  Relies on gnuplot for font metrics.
    case "top"
      dy = -0.5;
    case "cap"
      dy = -0.5;
    case "middle"
      dy = 0.5 * (num_lines - 1);
    case "baseline"
      dy = 0.5 + (num_lines - 1);
    case "bottom"
      dy = 0.5 + (num_lines - 1);
  endswitch
  ## Gnuplot's Character units are different for x/y and vary with
  ## fontsize.  The aspect ratio of 1:1.7 was determined by experiment
  ## to work for eps/ps/etc.  For the MacOS aqua terminal a value of 2.5
  ## is needed.  However, the difference is barely noticeable.
  dx_and_dy = [(-dy * sind (angle)), (dy * cosd (angle))] .* [1.7 1];

  ## FIXME: Multiline text produced the gnuplot
  ##        "warning: ft_render: skipping glyph"
  if (__calc_dimensions__ (hax) == 3)
    zstr = sprintf (",%.15e", lpos(3));
  else
    zstr = "";
  endif
  fprintf (stream,
           ['set label "%s" at %s %.15e,%.15e%s %s rotate by %f offset character %f,%f %s %s front %s;' "\n"],
           label, units, lpos(1), lpos(2), zstr, halign, angle, dx_and_dy,
           fontspec, __do_enhanced_option__ (enhanced, obj), colorspec);

endfunction

function cdata = mapcdata (cdata, mode, clim, cmap_sz)

  if (ndims (cdata) == 3)
    ## True Color, clamp data to 8-bit
    clim = double (clim);
    cdata = double (cdata);
    clim_rng = clim(2) - clim(1);
    if (clim_rng != 0)
      cdata = 255 * (cdata - clim(1)) / clim_rng;
      cdata(cdata < 0) = 0;  cdata(cdata > 255) = 255;
    else
      cdata(:) = fix (255 / 2);
    endif
  else
    if (strcmp (mode, "scaled"))
      clim = double (clim);
      cdata = double (cdata);
      clim_rng = clim(2) - clim(1);
      if (clim_rng != 0)
        cdata = 1 + fix (cmap_sz * (cdata - clim(1)) / clim_rng);
      else
        cdata(:) = 1 + fix (cmap_sz / 2);
      endif
    else
      if (islogical (cdata) || isinteger (cdata))
        cdata += 1;
      else
        cdata = fix (cdata);
      endif
    endif
    cdata = max (1, min (cdata, cmap_sz));
  endif

endfunction
