## Copyright (C) 2005, 2007 John W. Eaton
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## Undocumented internal function.

## Author: jwe

function __go_draw_axes__ (h, plot_stream, enhanced, mono)

  if (nargin == 4)

    axis_obj = get (h);

    parent_figure_obj = get (axis_obj.parent);

    persistent have_newer_gnuplot ...
      = compare_versions (__gnuplot_version__ (), "4.0", ">");

    ## Set axis properties here?
    pos = [0, 0, 1, 1];
    if (strcmp (axis_obj.activepositionproperty, "outerposition"))
      ymirror = true;
      if (! isempty (axis_obj.outerposition))
	pos = axis_obj.outerposition;
      endif
    else
      ymirror = false;
      if (! isempty (axis_obj.position))
	pos = axis_obj.position;
	fprintf (plot_stream, "set tmargin 0;\n");
	fprintf (plot_stream, "set bmargin 0;\n");
	fprintf (plot_stream, "set lmargin 0;\n");
	fprintf (plot_stream, "set rmargin 0;\n");
      endif
    endif

    if (! strcmp (axis_obj.__colorbar__, "none"))
      [pos, cbox_orient, cbox_size, cbox_origin, cbox_mirror] = ...
	  gnuplot_postion_colorbox (pos, axis_obj.__colorbar__);
    endif

    fprintf (plot_stream, "set origin %.15g, %.15g;\n", pos(1), pos(2));
    fprintf (plot_stream, "set size %.15g, %.15g;\n", pos(3), pos(4));

    if (strcmpi (axis_obj.dataaspectratiomode, "manual"))
      r = axis_obj.dataaspectratio;
      fprintf (plot_stream, "set size ratio %.15g;\n", -r(2)/r(1));
    else
      fputs (plot_stream, "set size noratio;\n");
    endif

    fputs (plot_stream, "set pm3d;\n");
    fputs (plot_stream, "unset label;\n");

    if (! isempty (axis_obj.title))
      t = get (axis_obj.title);
      if (isempty (t.string))
	fputs (plot_stream, "unset title;\n");
      else
	[tt, f, s] = __maybe_munge_text__ (enhanced, t, "string", 
					   have_newer_gnuplot);
	if (strcmp (f, "*"))
	  fontspec = "";
	else
	  fontspec = sprintf ("font \"%s,%d\"", f, s);
	endif
	fprintf (plot_stream, "set title \"%s\" %s;\n",
		 undo_string_escapes (tt), fontspec);
      endif
    endif

    if (! isempty (axis_obj.xlabel))
      t = get (axis_obj.xlabel);
      angle = t.rotation;
      colorspec = get_text_colorspec (axis_obj.xcolor, mono);
      if (isempty (t.string))
	fprintf (plot_stream, "unset xlabel;\n");
	fprintf (plot_stream, "unset x2label;\n");
      else
	[tt, f, s] = __maybe_munge_text__ (enhanced, t, "string",
					   have_newer_gnuplot);
	if (strcmp (f, "*"))
	  fontspec = "";
	else
	  fontspec = sprintf ("font \"%s,%d\"", f, s);
	endif
	if (strcmpi (axis_obj.xaxislocation, "top"))
	  fprintf (plot_stream, "set x2label \"%s\" %s %s",
		   undo_string_escapes (tt), colorspec, fontspec);
	else
	  fprintf (plot_stream, "set xlabel \"%s\" %s %s",
		   undo_string_escapes (tt), colorspec, fontspec);
	endif
	if (have_newer_gnuplot)
	  ## Rotation of xlabel not yet support by gnuplot as of 4.2, but
	  ## there is no message about it.
	  fprintf (plot_stream, " rotate by %f", angle);
	endif
	fputs (plot_stream, ";\n");
	if (strcmpi (axis_obj.xaxislocation, "top"))
	  fprintf (plot_stream, "unset xlabel;\n");
	else
	  fprintf (plot_stream, "unset x2label;\n");
	endif
      endif
    endif

    if (! isempty (axis_obj.ylabel))
      t = get (axis_obj.ylabel);
      angle = t.rotation;
      colorspec = get_text_colorspec (axis_obj.ycolor, mono);
      if (isempty (t.string))
	fprintf (plot_stream, "unset ylabel;\n");
	fprintf (plot_stream, "unset y2label;\n");
      else
	[tt, f, s] = __maybe_munge_text__ (enhanced, t, "string",
					   have_newer_gnuplot);
	if (strcmp (f, "*"))
	  fontspec = "";
	else
	  fontspec = sprintf ("font \"%s,%d\"", f, s);
	endif
	if (strcmpi (axis_obj.yaxislocation, "right"))
	  fprintf (plot_stream, "set y2label \"%s\" %s %s",
		   undo_string_escapes (tt), colorspec, fontspec);
	else
	  fprintf (plot_stream, "set ylabel \"%s\" %s %s",
		   undo_string_escapes (tt), colorspec, fontspec);
	endif
	if (have_newer_gnuplot)
	  fprintf (plot_stream, " rotate by %f;\n", angle);
	endif
	fputs (plot_stream, ";\n");
	if (strcmpi (axis_obj.yaxislocation, "right"))
	  fprintf (plot_stream, "unset ylabel;\n");
	else
	  fprintf (plot_stream, "unset y2label;\n");
	endif
      endif
    endif

    if (! isempty (axis_obj.zlabel))
      t = get (axis_obj.zlabel);
      angle = t.rotation;
      colorspec = get_text_colorspec (axis_obj.zcolor, mono);
      if (isempty (t.string))
	fputs (plot_stream, "unset zlabel;\n");
      else
	[tt, f, s] = __maybe_munge_text__ (enhanced, t, "string",
					   have_newer_gnuplot);
	if (strcmp (f, "*"))
	  fontspec = "";
	else
	  fontspec = sprintf ("font \"%s,%d\"", f, s);
	endif
	fprintf (plot_stream, "set zlabel \"%s\" %s %s",
		 undo_string_escapes (tt), colorspec, fontspec);
	if (have_newer_gnuplot)
	  ## Rotation of zlabel not yet support by gnuplot as of 4.2, but
	  ## there is no message about it.
	  fprintf (plot_stream, " rotate by %f;\n", angle);
	endif
	fputs (plot_stream, ";\n");
      endif
    endif

    if (strcmpi (axis_obj.xaxislocation, "top"))
      xaxisloc = "x2";
      xaxisloc_using = "x2";
    else
      xaxisloc = "x";
      xaxisloc_using = "x1";
    endif
    if (strcmpi (axis_obj.yaxislocation, "right"))
      yaxisloc = "y2";
      yaxisloc_using = "y2";
    else
      yaxisloc = "y";
      yaxisloc_using = "y1";
    endif

    have_grid = false;

    if (strcmpi (axis_obj.xgrid, "on"))
      have_grid = true;
      fprintf (plot_stream, "set grid %stics;\n", xaxisloc);
    else
      fprintf (plot_stream, "set grid no%stics;\n", xaxisloc);
    endif

    if (strcmpi (axis_obj.ygrid, "on"))
      have_grid = true;
      fprintf (plot_stream, "set grid %stics;\n", yaxisloc);
    else
      fprintf (plot_stream, "set grid no%stics;\n", yaxisloc);
    endif

    if (strcmpi (axis_obj.zgrid, "on"))
      have_grid = true;
      fputs (plot_stream, "set grid ztics;\n");
    else
      fputs (plot_stream, "set grid noztics;\n");
    endif

    if (strcmpi (axis_obj.xminorgrid, "on"))
      have_grid = true;
      fprintf (plot_stream, "set m%stics 5;\n", xaxisloc);
      fprintf (plot_stream, "set grid m%stics;\n", xaxisloc);
    else
      fprintf (plot_stream, "set grid nom%stics;\n", xaxisloc);
    endif

    if (strcmpi (axis_obj.yminorgrid, "on"))
      have_grid = true;
      fprintf (plot_stream, "set m%stics 5;\n", yaxisloc);
      fprintf (plot_stream, "set grid m%stics;\n", yaxisloc);
    else
      fprintf (plot_stream, "set grid nom%stics;\n", yaxisloc);
    endif

    if (strcmpi (axis_obj.zminorgrid, "on"))
      have_grid = true;
      fputs (plot_stream, "set mztics 5;\n");
      fputs (plot_stream, "set grid mztics;\n");
    else
      fputs (plot_stream, "set grid nomztics;\n");
    endif

    ## The grid front/back/layerdefault option also controls the
    ## appearance of tics, so it is used even if the grid is absent.
    if (strcmpi (axis_obj.layer, "top"))
      fputs (plot_stream, "set grid front;\n");
    else
      fputs (plot_stream, "set grid layerdefault;\n");
    endif
    if (! have_grid)
      fputs (plot_stream, "unset grid;\n");
    endif

    do_tics (axis_obj, plot_stream, ymirror, mono);

    xlogscale = strcmpi (axis_obj.xscale, "log");
    if (xlogscale)
      fprintf (plot_stream, "set logscale %s;\n", xaxisloc);
    else
      fprintf (plot_stream, "unset logscale %s;\n", xaxisloc);
    endif

    ylogscale = strcmpi (axis_obj.yscale, "log");
    if (ylogscale)
      fprintf (plot_stream, "set logscale %s;\n", yaxisloc);
    else
      fprintf (plot_stream, "unset logscale %s;\n", yaxisloc);
    endif

    zlogscale = strcmpi (axis_obj.zscale, "log");
    if (zlogscale)
      fputs (plot_stream, "set logscale z;\n");
    else
      fputs (plot_stream, "unset logscale z;\n");
    endif

    xautoscale = strcmpi (axis_obj.xlimmode, "auto");
    yautoscale = strcmpi (axis_obj.ylimmode, "auto");
    zautoscale = strcmpi (axis_obj.zlimmode, "auto");
    cautoscale = strcmpi (axis_obj.climmode, "auto");

    kids = axis_obj.children;

    nd = __calc_dimensions__ (axis_obj);

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

    xlim = axis_obj.xlim;
    ylim = axis_obj.ylim;
    zlim = axis_obj.zlim;
    clim = axis_obj.clim;

    if (! cautoscale && clim(1) == clim(2))
      clim(2)++;
    endif

    [view_cmd, view_fcn, view_zoom] = image_viewer ();
    use_gnuplot_for_images = (ischar (view_fcn)
			      && strcmp (view_fcn, "gnuplot_internal"));

    ximg_data = {};
    ximg_data_idx = 0;

    for i = 1:length (kids)

      obj = get (kids(i));

      switch (obj.type)
	case "image"
	  img_data = obj.cdata;
	  img_xdata = obj.xdata;
	  img_ydata = obj.ydata;

	  if (use_gnuplot_for_images)

	    fputs (plot_stream, "set border front;\n");
	    data_idx++;
	    is_image_data(data_idx) = true;
	    parametric(data_idx) = false;
	    have_cdata(data_idx) = false;

	    [y_dim, x_dim] = size (img_data(:,:,1));
	    if (x_dim > 1)
	      dx = abs (img_xdata(2)-img_xdata(1))/(x_dim-1);
	    else
	      dx = 1;
	    endif
	    if (y_dim > 1)
	      dy = abs (img_ydata(2)-img_ydata(1))/(y_dim-1);
	    else
	      dy = 1;
	    endif
	    x_origin = min (img_xdata);
	    y_origin = min (img_ydata);

	    if (ndims (img_data) == 3)
	      data{data_idx} = permute (img_data, [3, 1, 2])(:);
	      format = "1:2:3";
	      imagetype = "rgbimage";
	    else
	      data{data_idx} = img_data(:);
	      format = "1";
	      imagetype = "image";
	    endif

	    titlespec{data_idx} = "title \"\"";
	    usingclause{data_idx} = sprintf ("binary array=%dx%d scan=yx origin=(%.15g,%.15g) dx=%.15g dy=%.15g using %s",
		x_dim, y_dim, x_origin, y_origin, dx, dy, format);
	    withclause{data_idx} = sprintf ("with %s", imagetype);

	  else
	    ximg_data{++ximg_data_idx} = img_data;
	  endif

	case "line"
	  if (strncmp (obj.linestyle, "none", 4)
	      && (! isfield (obj, "marker")
		  || (isfield (obj, "marker")
		      && strncmp (obj.marker, "none", 4))))
	    continue;
	  endif
	  data_idx++;
	  is_image_data(data_idx) = false;
	  parametric(data_idx) = true;
	  have_cdata(data_idx) = false;
	  if (isempty (obj.keylabel))
	    titlespec{data_idx} = "title \"\"";
	  else
	    tmp = undo_string_escapes (__maybe_munge_text__ (enhanced, obj, "keylabel", have_newer_gnuplot));
	    titlespec{data_idx} = strcat ("title \"", tmp, "\"");
	  endif
	  [style, typ, with] = do_linestyle_command (obj, data_idx,
						     mono, plot_stream);
	  usingclause{data_idx} = "";
	  if (have_newer_gnuplot || isnan (typ))
	    withclause{data_idx} = sprintf ("with %s linestyle %d",
					    style, data_idx);
	  else
	    withclause{data_idx} = sprintf ("with %s linetype %d",
					    style, typ);
	  endif
	  if (nd == 3)
	    xdat = obj.xdata(:);
	    ydat = obj.ydata(:);
	    if (! isempty (obj.zdata))
	      zdat = obj.zdata(:);
	    else
	      zdat = zeros (size (xdat));
	    endif
	    data{data_idx} = [xdat, ydat, zdat]';
	    usingclause{data_idx} = "using ($1):($2):($3)";
	    ## fputs (plot_stream, "set parametric;\n");
	  else
	    xdat = obj.xdata(:);
	    ydat = obj.ydata(:);
	    ldat = obj.ldata;
	    yerr = xerr = false;
	    if (! isempty (ldat))
	      yerr = true;
	      ldat = ldat(:);
	    endif
	    udat = obj.udata;
	    if (! isempty (udat))
	      udat = udat(:);
	    endif
	    xldat = obj.xldata;
	    if (! isempty (xldat))
	      xerr = true;
	      xldat = xldat(:);
	    endif
	    xudat = obj.xudata;
	    if (! isempty (xudat))
	      xudat = xudat(:);
	    endif
	    if (yerr)
	      if (isempty (ldat))
		ylo = ydat;
	      else
		ylo = ydat-ldat;
	      endif
	      if (isempty (udat))
		yhi = ydat;
	      else
		yhi = ydat+udat;
	      endif
	      if (xerr)
		if (isempty (xldat))
		  xlo = xdat;
		else
		  xlo = xdat-xldat;
		endif
		if (isempty (xudat))
		  xhi = xdat;
		else
		  xhi = xdat+xudat;
		endif
		data{data_idx} = [xdat, ydat, xlo, xhi, ylo, yhi]';
		usingclause{data_idx} = "using ($1):($2):($3):($4):($5):($6)";
		withclause{data_idx} = "with xyerrorbars";
	      else
		data{data_idx} = [xdat, ydat, ylo, yhi]';
		usingclause{data_idx} = "using ($1):($2):($3):($4)";
		withclause{data_idx} = "with yerrorbars";
	      endif
	    elseif (xerr)
	      if (isempty (xldat))
		xlo = xdat;
	      else
		xlo = xdat-xldat;
	      endif
	      if (isempty (xudat))
		xhi = xdat;
	      else
		xhi = xdat+xudat;
	      endif
	      data{data_idx} = [xdat, ydat, xlo, xhi]';
	      usingclause{data_idx} = "using ($1):($2):($3):($4)";
	      withclause{data_idx} = "with xerrorbars";
	    else
	      data{data_idx} = [xdat, ydat]';
	      usingclause{data_idx} = sprintf ("using ($1):($2) axes %s%s",
					      xaxisloc_using, yaxisloc_using);
	    endif
	  endif
	  if (! (have_newer_gnuplot || isempty (with)))
	    if (isempty (withclause{data_idx}))
	      withclause{data_idx} = sprintf ("with %s", with);
	    else
	      withclause{data_idx} = sprintf ("%s %s", withclause{data_idx},
					      with);
	    endif
	  endif

       case "patch"
         cmap = parent_figure_obj.colormap;
	 [nr, nc] = size (obj.xdata);

	 if (! isempty (obj.cdata))
	   cdat = obj.cdata;
	 else
	   cdat = [];
	 endif

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
	     if (strncmp (obj.facecolor, "none", 4)) 
	       if (isnan (hidden_removal))
		 hidden_removal = false;
	       endif
	     else
	       hidden_removal = true;
	       if (nd == 3)
		 error ("gnuplot (as of v4.2) only supports 2D filled patches");
	       endif

	       data_idx++;
	       is_image_data(data_idx) = false;
	       parametric(data_idx) = false;
	       have_cdata(data_idx) = false;
	       if (i > 1 || isempty (obj.keylabel))
		 titlespec{data_idx} = "title \"\"";
	       else
		 tmp = undo_string_escapes (__maybe_munge_text__ (enhanced, obj, "keylabel", have_newer_gnuplot));
		 titlespec{data_idx} = strcat ("title \"", tmp, "\"");
	       endif
	       usingclause{data_idx} = "";
               if (isfield (obj, "facecolor"))
		 if ((strncmp (obj.facecolor, "flat", 4)
		     || strncmp (obj.facecolor, "interp", 6))
		     && isfield (obj, "cdata"))
		   if (ndims (obj.cdata) == 2
		       && (size (obj.cdata, 2) == nc
			   && (size (obj.cdata, 1) == 1
			       || size (obj.cdata, 1) == 3)))
		     ccol = cdat (:, i);
		   elseif (ndims (obj.cdata) == 2
		       && (size (obj.cdata, 1) == nc
			   && (size (obj.cdata, 2) == 1
			       || size (obj.cdata, 2) == 3)))
		     ccol = cdat (i, :);
		   elseif (ndims (obj.cdata) == 3)
		     ccol = permute (cdat (:, i, :), [1, 3, 2]);
		   else
		     ccol = cdat;
		   endif
		   if (strncmp (obj.facecolor, "flat", 4))
		     if (numel(ccol) == 3)
		       color = ccol;
		     else
		       r = 1 + round ((size (cmap, 1) - 1)
				      * (ccol - clim(1))/(clim(2) - clim(1)));
		       r = max (1, min (r, size (cmap, 1)));
		       color = cmap(r, :);
		     endif
		   elseif (strncmp (obj.facecolor, "interp", 6))
		     warning ("\"interp\" not supported, using 1st entry of cdata")
		     r = 1 + round ((size (cmap, 1) - 1) * ccol(1));
		     r = max (1, min (r, size (cmap, 1)));
		     color = cmap(r,:);
		   endif
		 elseif (isnumeric (obj.facecolor))
		   color = obj.facecolor;
		 else
		   color = [0, 1, 0];
		 endif
               else
		 color = [0, 1, 0];
               endif

	       if (have_newer_gnuplot)
		 if (mono)
		   colorspec = "";
		 else
		   colorspec = sprintf ("lc rgb \"#%02x%02x%02x\"",
					round (255*color));
		 endif
		 withclause{data_idx} = sprintf ("with filledcurve %s",
						 colorspec);
	       else
		 if (isequal (color, [0,0,0]))
		   typ = -1;
		 elseif (isequal (color, [1,0,0]))
		   typ = 1;
		 elseif (isequal (color, [0,1,0]))
		   typ = 2;
		 elseif (isequal (color, [0,0,1]))
		   typ = 3;
		 elseif (isequal (color, [1,0,1]))
		   typ = 4;
		 elseif (isequal (color, [0,1,1]))
		   typ = 5;
		 elseif (isequal (color, [1,1,1]))
		   typ = -1;
		 elseif (isequal (color, [1,1,0]))
		   typ = 7;
		 else
		   typ = -1;
		 endif
		 withclause{data_idx} = sprintf ("with filledcurve lt %d", typ);
	       endif
	       data{data_idx} = [xcol, ycol]';
	       usingclause{data_idx} = "using ($1):($2)";
	     endif
	   endif

           ## patch outline
	   if (! strncmp (obj.edgecolor, "none", 4))

	     data_idx++;
             is_image_data(data_idx) = false;
             parametric(data_idx) = false;
	     have_cdata(data_idx) = false;
             titlespec{data_idx} = "title \"\"";
	     usingclause{data_idx} = "";

	     if (isfield (obj, "markersize"))
	       mdat = obj.markersize;
	     endif

             if (isfield (obj, "edgecolor"))
	       if ((strncmp (obj.edgecolor, "flat", 4)
		    || strncmp (obj.edgecolor, "interp", 6))
		   && isfield (obj, "cdata"))
		 if (ndims (obj.cdata) == 2
		     && (size (obj.cdata, 2) == nc
			 && (size (obj.cdata, 1) == 1
			     || size (obj.cdata, 1) == 3)))
		   ccol = cdat (:, i);
		 elseif (ndims (obj.cdata) == 2
			 && (size (obj.cdata, 1) == nc
			     && (size (obj.cdata, 2) == 1
				 || size (obj.cdata, 2) == 3)))
		   ccol = cdat (i, :);
		 elseif (ndims (obj.cdata) == 3)
		   ccol = permute (cdat (:, i, :), [1, 3, 2]);
		 else
		   ccol = cdat;
		 endif
		 if (strncmp (obj.edgecolor, "flat", 4))
		   if (numel(ccol) == 3)
		     color = ccol;
		   else
		     r = 1 + round ((size (cmap, 1) - 1)
				    * (ccol - clim(1))/(clim(2) - clim(1)));
		     r = max (1, min (r, size (cmap, 1)));
		     color = cmap(r, :);
		   endif
		 elseif (strncmp (obj.edgecolor, "interp", 6))
		   warning ("\"interp\" not supported, using 1st entry of cdata")
		   r = 1 + round ((size (cmap, 1) - 1) * ccol(1));
		   r = max (1, min (r, size (cmap, 1)));
		   color = cmap(r,:);
		 endif
	       elseif (isnumeric (obj.edgecolor))
		 color = obj.edgecolor;
	       else
		 color = [0, 0, 0];
	       endif
             else
	       color = [0, 0, 0];
             endif

	     if (isfield (obj, "linestyle"))
	       switch (obj.linestyle)
		 case "-"
		   lt = "lt 1";
		 case "--"
		   lt = "lt 2";
		 case ":"
		   lt = "lt 3";
		 case "-."
		   lt = "lt 6";
		 case "none"
		   lt = "";
		 otherwise
		   lt = "";
	       endswitch
	     else
	       lt = "";
	     endif

	     if (isfield (obj, "marker"))
	       if (isfield (obj, "marker"))
		 switch (obj.marker)
		   case "+"
		     pt = "pt 1";
		   case "o"
		     pt = "pt 6";
		   case "*"
		     pt = "pt 3";
		   case "."
		     pt = "pt 0";
		   case "x"
		     pt = "pt 2";
		   case {"square", "s"}
		     pt = "pt 5";
		   case {"diamond", "d"}
		     pt = "pt 13";
		   case "^"
		     pt = "pt 9";
		   case "v"
		     pt = "pt 11";
		   case ">"
		     pt = "pt 8";
		   case "<"
		     pt = "pt 10";
		   case {"pentagram", "p"}
		     pt = "pt 4";
		   case {"hexagram", "h"}
		     pt = "pt 12";
		   case "none"
		     pt = "";
		   otherwise
		     pt = "";
		 endswitch
	       endif
	     else
	       pt = "";
	     endif

	     style = "lines";
	     if (isempty (lt))
	       if (! isempty (pt))
		 style = "points";
	       endif
	     elseif (! isempty (pt))
	       style = "linespoints";
	     endif

	     if (isfield (obj, "markersize"))
	       if (length (mdat) == nc)
		 m = mdat(i);
	       else
		 m = mdat;
	       endif
	       if (! strcmpi (style, "lines"))
		 if (have_newer_gnuplot)
		   ps = sprintf("pointsize %f", m);
		 else
		   ps = sprintf("ps %f", m);
		 endif
	       else
		 ps = "";
	       endif
	     else
	       ps = "";
	     endif

	     if (have_newer_gnuplot)
	       if (mono)
		 colorspec = "";
	       else
		 colorspec = sprintf ("lc rgb \"#%02x%02x%02x\"",
				      round (255*color));
	       endif
	       withclause{data_idx} = sprintf ("with %s %s %s %s %s",
					       style, pt, lt, ps, colorspec);
	     else
	       if (isequal (color, [0,0,0]))
		 typ = -1;
	       elseif (isequal (color, [1,0,0]))
		 typ = 1;
	       elseif (isequal (color, [0,1,0]))
		 typ = 2;
	       elseif (isequal (color, [0,0,1]))
		 typ = 3;
	       elseif (isequal (color, [1,0,1]))
		 typ = 4;
	       elseif (isequal (color, [0,1,1]))
		 typ = 5;
	       elseif (isequal (color, [1,1,1]))
		 typ = -1;
	       elseif (isequal (color, [1,1,0]))
		 typ = 7;
	       else
		 typ = -1;
	       endif
	       withclause{data_idx} = sprintf ("with %s %s %s lt %d", 
					       style, pt, ps, typ);
	     endif

	     if (nd == 3)
	       if (! isnan (xcol) && ! isnan (ycol) && ! isnan (zcol))
		 data{data_idx} = [[xcol; xcol(1)], [ycol; ycol(1)], ...
				   [zcol; zcol(1)]]';
	       else
		 data{data_idx} = [xcol, ycol, zcol]';
	       endif
	       usingclause{data_idx} = "using ($1):($2):($3)";
	     else
	       if (! isnan (xcol) && ! isnan (ycol))
		 data{data_idx} = [[xcol; xcol(1)], [ycol; ycol(1)]]';
	       else
		 data{data_idx} = [xcol, ycol]';
	       endif
	       usingclause{data_idx} = "using ($1):($2)";
	     endif
	   endif
	 endfor

	case "surface"
	  view_map = true;
          if (! (strncmp (obj.edgecolor, "none", 4)
		 && strncmp (obj.facecolor, "none", 4)))
	    data_idx++;
	    is_image_data(data_idx) = false;
	    parametric(data_idx) = false;
	    have_cdata(data_idx) = true;
	    [style, typ, with] = do_linestyle_command (obj, data_idx,
						       mono, plot_stream);
	    if (isempty (obj.keylabel))
	      titlespec{data_idx} = "title \"\"";
	    else
	      tmp = undo_string_escapes (__maybe_munge_text__ (enhanced, obj, "keylabel", have_newer_gnuplot));
	      titlespec{data_idx} = strcat ("title \"", tmp, "\"");
	    endif
	    usingclause{data_idx} = "";
	    if (have_newer_gnuplot || isnan (typ))
	      withclause{data_idx} = sprintf ("with %s linestyle %d",
		           		      style, data_idx);
	    else
	      withclause{data_idx} = sprintf ("with %s linetype %d %s",
		 			      style, typ, with);
	    endif

	    xdat = obj.xdata;
	    ydat = obj.ydata;
	    zdat = obj.zdata;
	    cdat = obj.cdata;

  	    err = false;
            if (! size_equal(zdat, cdat))
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
	      error ("__go_draw_axes__: invalid grid data");
	    endif
	    xlen = columns (zdat);
	    ylen = rows (zdat);
	    if (xlen == columns (xdat) && xlen == columns (ydat)
	        && ylen == rows (xdat) && ylen == rows (ydat))
	      len = 4 * xlen;
	      zz = zeros (ylen, len);
	      k = 1;
	      for kk = 1:4:len
	        zz(:,kk)   = xdat(:,k);
	        zz(:,kk+1) = ydat(:,k);
	        zz(:,kk+2) = zdat(:,k);
	        zz(:,kk+3) = cdat(:,k);
	        k++;
	      endfor
	      data{data_idx} = zz.';
	    endif
	    usingclause{data_idx} = "using ($1):($2):($3):($4)";
	    ## fputs (plot_stream, "unset parametric;\n");

	    ## Interpolation does not work for flat surfaces (e.g. pcolor)
            ## and color mapping --> currently set empty.
            interp_str = "";
            flat_interp_face = (strncmp (obj.facecolor, "flat", 4)
				|| strncmp (obj.facecolor, "interp", 6));
            flat_interp_edge = (strncmp (obj.edgecolor, "flat", 4)
				|| strncmp (obj.edgecolor, "interp", 6));

	    facecolor_none_or_white = (strncmp (obj.facecolor, "none", 4)
				       || (isnumeric (obj.facecolor)
					   && all (obj.facecolor == 1)));
	    if (strncmp (obj.facecolor, "none", 4))
	      if (isnan (hidden_removal))
		hidden_removal = false;
	      endif
	    else
	      hidden_removal = true;
	    endif

            if (flat_interp_face
		|| (flat_interp_edge && facecolor_none_or_white))
	      withclause{data_idx} = "with line palette";
            endif

	    if (have_newer_gnuplot)
	      dord = "depthorder";
	    else
	      dord = "";
	    endif

	    if (facecolor_none_or_white)
	      ## Ensure faces aren't drawn
	      fprintf (plot_stream, "unset pm3d;\n");
            elseif (flat_interp_face && strncmp (obj.edgecolor, "flat", 4))
              fprintf (plot_stream, "set pm3d at s %s %s corners2color c3;\n", 
		       interp_str, dord);
            else
              if (strncmp (obj.edgecolor, "none", 4))
                fprintf (plot_stream, "set pm3d at s %s corners2color c3;\n", 
			 interp_str, dord);
              else
                edgecol = obj.edgecolor;
                if (ischar (obj.edgecolor))
                  edgecol = [0,0,0];
                endif
                fprintf (plot_stream, "set pm3d at s hidden3d %d %s %s corners2color c3;\n", 
			 data_idx, interp_str, dord);

		if (have_newer_gnuplot)
		  if (mono)
		    colorspec = "";
		  else
		    colorspec = sprintf ("linecolor rgb \"#%02x%02x%02x\"",
					 round (255*edgecol));
		  endif
                  fprintf (plot_stream,
                           "set style line %d %s lw %f;\n",
                           data_idx, colorspec, obj.linewidth);
		else
		  if (isequal (edgecol, [0,0,0]))
		    typ = -1;
		  elseif (isequal (edgecol, [1,0,0]))
		    typ = 1;
		  elseif (isequal (edgecol, [0,1,0]))
		    typ = 2;
		  elseif (isequal (edgecol, [0,0,1]))
		    typ = 3;
		  elseif (isequal (edgecol, [1,0,1]))
		    typ = 4;
		  elseif (isequal (edgecol, [0,1,1]))
		    typ = 5;
		  elseif (isequal (edgecol, [1,1,1]))
		    typ = -1;
		  elseif (isequal (edgecol, [1,1,0]))
		    typ = 7;
		  else
		    typ = -1;
		  endif
                  fprintf (plot_stream,
                           "set style line %d lt %d lw %f;\n",
                           data_idx, typ, obj.linewidth);
		endif
              endif
            endif
	  endif

	case "text"
	  [label, f, s] = __maybe_munge_text__ (enhanced, obj, "string",
						have_newer_gnuplot);
	  if (strcmp (f, "*"))
	    fontspec = "";
	  else
	    fontspec = sprintf ("font \"%s,%d\"", f, s);
	  endif
	  lpos = obj.position;
	  halign = obj.horizontalalignment;
	  angle = obj.rotation;
          units = obj.units;
	  color = obj.color;
          if (strcmpi (units, "normalized"))
            units = "graph";
          else
            units = "";
          endif
	  
	  if (isnumeric (color))
	    colorspec = get_text_colorspec (color, mono);
	  endif

	  if (nd == 3)
	    fprintf (plot_stream,
		     "set label \"%s\" at %s %.15g,%.15g,%.15g %s rotate by %f %s front %s;\n",
		     undo_string_escapes (label), units, lpos(1),
		     lpos(2), lpos(3), halign, angle, fontspec, colorspec);
	  else
 	    fprintf (plot_stream,
 		     "set label \"%s\" at %s %.15g,%.15g %s rotate by %f %s front %s;\n",
 		     undo_string_escapes (label), units,
 		     lpos(1), lpos(2), halign, angle, fontspec, colorspec);
	  endif

	otherwise
	  error ("__go_draw_axes__: unknown object class, %s",
		 obj.type);
      endswitch

    endfor

    if (isnan(hidden_removal) || hidden_removal)
      fputs (plot_stream, "set hidden3d;\n");
    else
      fputs (plot_stream, "unset hidden3d;\n");
    endif

    have_data = (! (isempty (data) || any (cellfun (@isempty, data))));

    if (isempty (xlim))
      return;
    endif
    if (strcmpi (axis_obj.xdir, "reverse"))
      xdir = "reverse";
    else
      xdir = "noreverse";
    endif
    fprintf (plot_stream, "set %srange [%.15e:%.15e] %s;\n",
	     xaxisloc, xlim, xdir);

    if (isempty (ylim))
      return;
    endif
    if (strcmpi (axis_obj.ydir, "reverse"))
      ydir = "reverse";
    else
      ydir = "noreverse";
    endif
    fprintf (plot_stream, "set %srange [%.15e:%.15e] %s;\n",
	     yaxisloc, ylim, ydir);

    if (nd == 3)
      if (isempty (zlim))
	return;
      endif
      if (strcmpi (axis_obj.zdir, "reverse"))
	zdir = "reverse";
      else
	zdir = "noreverse";
      endif
      fprintf (plot_stream, "set zrange [%.15e:%.15e] %s;\n", zlim, zdir);
    endif

    if (! any (isinf (clim)))
      fprintf (plot_stream, "set cbrange [%g:%g];\n", clim);
    endif

    if (strcmpi (axis_obj.box, "on"))
      if (nd == 3)
	fputs (plot_stream, "set border 4095;\n");
      else
	fputs (plot_stream, "set border 431;\n");
      endif
    else
      if (nd == 3)
	fputs (plot_stream, "set border 895;\n");
      else
	fputs (plot_stream, "set border 3;\n");
	fputs (plot_stream, "set xtics nomirror; set ytics nomirror;\n");
      endif
    endif

    if (strcmpi (axis_obj.visible, "off"))
      fputs (plot_stream, "unset border; unset tics\n");
    endif

    if (strcmpi (axis_obj.key, "on"))
      if (strcmpi (axis_obj.keybox, "on"))
	box = "box";
      else
	box = "nobox";
      endif
      inout = "inside";
      keypos = axis_obj.keypos;
      if (ischar (keypos))
	keypos = lower (keypos);
	keyout = findstr (keypos, "outside");
	if (! isempty (keyout))
	  inout = "outside";
	  keypos = keypos(1:keyout-1);
	endif
      endif
      switch (keypos)
	case -1
	  pos = "right top";
	  inout = "outside";
	case 1
	  pos = "right top";
	case 2
	  pos = "left top";
	case 3
	  pos = "left bottom";
	case {4, 0}
	  pos = "right bottom";
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
	  ## least conflict with data in plot
	  ## least unused space outside plot
	otherwise
	  pos = "";
      endswitch
      if (! have_newer_gnuplot)
	inout = "";
      endif
      fprintf (plot_stream, "set key %s %s %s;\n", inout, pos, box);
    else
      fputs (plot_stream, "unset key;\n");
    endif

    fputs (plot_stream, "set style data lines;\n");

    if (! use_gnuplot_for_images)
      for i = 1:ximg_data_idx
	view_fcn (xlim, ylim, ximg_data{i}, view_zoom, view_cmd);
      endfor
    endif

    cmap = parent_figure_obj.colormap;    
    cmap_sz = rows(cmap);
    if (length(cmap) > 0)
      if (have_newer_gnuplot)
        fprintf (plot_stream,
		 "set palette positive color model RGB maxcolors %i;\n",
	         cmap_sz);
	fprintf (plot_stream,
	         "set palette file \"-\" binary record=%d using 1:2:3:4;\n",
	         cmap_sz);
	fwrite (plot_stream, [1:cmap_sz; cmap.'], "float32");
      else
	fputs (plot_stream, "set palette defined (");
	for i = 1: cmap_sz
	  col = floor(cmap(i, :) * 255);
          if (i == 1)
	    fputs (plot_stream, sprintf("%d \"#%02X%02X%02X\"", i - 1, 
					col(1), col(2), col(3)));
	  else
	    fputs (plot_stream, sprintf(", %d \"#%02X%02X%02X\"", i - 1, 
					col(1), col(2), col(3)));
	  endif
	endfor
	fputs (plot_stream, ");\n");
      endif
    endif
	    
    if (strcmp (axis_obj.__colorbar__, "none"))
      fputs (plot_stream, "unset colorbox;\n");
    else
      ## FIXME If cbox_mirror is true we want to invert the tic labels
      ## but gnuplot doesn't allow that
      fputs (plot_stream, 
	     sprintf ("set colorbox %s user origin %f,%f size %f,%f;\n",
		      cbox_orient, cbox_origin, cbox_size));
    endif

    if (have_data)
      if (nd == 2)
	plot_cmd = "plot";
      else
	plot_cmd = "splot";
	rot_x = 90 - axis_obj.view(2);
	rot_z = axis_obj.view(1);
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
      fprintf (plot_stream, "%s \"-\" %s %s %s \\\n", plot_cmd,
	       usingclause{1}, titlespec{1}, withclause{1});
      for i = 2:data_idx
	fprintf (plot_stream, ", \"-\" %s %s %s \\\n",
		 usingclause{i}, titlespec{i}, withclause{i});
      endfor
      fputs (plot_stream, ";\n");
      for i = 1:data_idx
	if (is_image_data(i))
	  fwrite (plot_stream, data{i}, "float32");
	else
	  __gnuplot_write_data__ (plot_stream, data{i}, nd, parametric(i), 
				  have_cdata(i));
	endif
      endfor
    else
      fputs (plot_stream, "plot \"-\";\nInf Inf\ne\n");
    endif

    ## Needed to allow mouse rotation with pcolor
    if (view_map)
      fputs (plot_stream, "unset view;\n");
    endif

    fflush (plot_stream);

  else
    print_usage ();
  endif

endfunction

function [style, typ, with] = do_linestyle_command (obj, idx, mono, plot_stream)

  persistent have_newer_gnuplot ...
    = compare_versions (__gnuplot_version__ (), "4.0", ">");

  if (have_newer_gnuplot)
    fprintf (plot_stream, "set style line %d default;\n", idx);
  endif
  fprintf (plot_stream, "set style line %d", idx);

  found_style = false;
  typ = NaN;
  with = "";

  if (isfield (obj, "color"))
    color = obj.color;
    if (isnumeric (color))
      if (have_newer_gnuplot)
	if (! mono)
	  fprintf (plot_stream, " linecolor rgb \"#%02x%02x%02x\"",
		   round (255*color));
	endif
      else
	if (isequal (color, [0,0,0]))
	  typ = -1;
	elseif (isequal (color, [1,0,0]))
	  typ = 1;
	elseif (isequal (color, [0,1,0]))
	  typ = 2;
	elseif (isequal (color, [0,0,1]))
	  typ = 3;
	elseif (isequal (color, [1,0,1]))
	  typ = 4;
	elseif (isequal (color, [0,1,1]))
	  typ = 5;
	elseif (isequal (color, [1,1,1]))
	  typ = 6;
	elseif (isequal (color, [1,1,0]))
	  typ = 7;
	else
	  typ = 2;
	endif
      endif
    endif
    found_style = true;
  endif

  if (isfield (obj, "linestyle"))
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

    ## FIXME -- linetype is currently broken, since it disables the
    ## gnuplot default dashed and solid linestyles with the only
    ## benefit of being able to specify '--' and get a single sized
    ## dashed line of identical dash pattern for all called this way.
    ## All dash patterns are a subset of "with lines" and none of the
    ## lt specifications will correctly propagate into the x11 terminal
    ## or the print command.   Therefore, it is currently disabled in
    ## order to allow print (..., "-dashed") etc. to work correctly.

    ##    if (! isempty (lt))
    ##      fprintf (plot_stream, " linetype %s", lt);
    ##      found_style = true;
    ##    endif

  else
    lt = "";
  endif

  if (isfield (obj, "linewidth"))
    if (have_newer_gnuplot)
      fprintf (plot_stream, " linewidth %f", obj.linewidth);
    else
      with = sprintf ("%s lw %f", with, obj.linewidth);
    endif
    found_style = true;
  endif

  if (isfield (obj, "marker"))
    switch (obj.marker)
      case "+"
	pt = "1";
      case "o"
	pt = "6";
      case "*"
	pt = "3";
      case "."
	pt = "0";
      case "x"
	pt = "2";
      case {"square", "s"}
	pt = "5";
      case {"diamond", "d"}
	pt = "13";
      case "^"
	pt = "9";
      case "v"
	pt = "11";
      case ">"
	pt = "8";
      case "<"
	pt = "10";
      case {"pentagram", "p"}
	pt = "4";
      case {"hexagram", "h"}
	pt = "12";
      case "none"
	pt = "";
      otherwise
	pt = "";
    endswitch
    if (! isempty (pt))
      if (have_newer_gnuplot)
	fprintf (plot_stream, " pointtype %s", pt);
      else
	with = sprintf ("%s pt %s", with, pt);
      endif
      found_style = true;
    endif
  else
    pt = "";
  endif

  style = "lines";
  if (isempty (lt))
    if (! isempty (pt))
      style = "points";
    endif
  elseif (! isempty (pt))
    style = "linespoints";
  endif

  if (isfield (obj, "markersize"))
    if (have_newer_gnuplot)
      fprintf (plot_stream, " pointsize %f", obj.markersize);
    else
      if (! strcmpi (style, "lines"))
	with = sprintf ("%s ps %f", with, obj.markersize);
      endif
    endif
    found_style = true;
  endif

  if (have_newer_gnuplot && ! found_style)
    fputs (plot_stream, " default");
  endif

  fputs (plot_stream, ";\n");

endfunction

function nd = __calc_dimensions__ (obj)
  kids = obj.children;
  nd = 2;
  for i = 1:length (kids)
    obj = get (kids(i));
    switch (obj.type)
      case {"image", "text"}
	## ignore as they 
      case {"line", "patch"}
	if (! isempty (obj.zdata))
	  nd = 3;
	endif
      case "surface"
	nd = 3;
    endswitch
  endfor
endfunction

function __gnuplot_write_data__ (plot_stream, data, nd, parametric, cdata)
  
  ## DATA is already transposed.

  ## FIXME -- this may need to be converted to C++ for speed.

  ## Convert NA elements to normal NaN values because fprintf writes
  ## "NA" and that confuses gnuplot.
  idx = find (isna (data));
  if (any (idx))
    data(idx) = NaN;
  endif

  if (nd == 2)
    nan_elts = find (sum (isnan (data)));
    fmt = strcat (repmat ("%.15g ", 1, rows (data)), "\n");
    if (isempty (nan_elts))
      fprintf (plot_stream, fmt, data);
    else
      n = columns (data);
      have_nans = true;
      num_nan_elts = numel (nan_elts);
      k = 1;
      for i = 1:n
	if (have_nans && i == nan_elts(k))
	  fputs (plot_stream, "\n");
	  have_nans = ++k <= num_nan_elts;
	else
	  fprintf (plot_stream, fmt, data(:,i));
	endif
      endfor
    endif
  elseif (nd == 3)
    ## FIXME -- handle NaNs here too?
    if (parametric)
      fprintf (plot_stream, "%.15g %.15g %.15g\n", data);
    else
      nr = rows (data);
      if (cdata)
	for j = 1:4:nr
	  fprintf (plot_stream, "%.15g %.15g %.15g %.15g\n", data(j:j+3,:));
	  fputs (plot_stream, "\n");
	endfor
      else
	for j = 1:3:nr
	  fprintf (plot_stream, "%.15g %.15g %.15g\n", data(j:j+2,:));
	  fputs (plot_stream, "\n");
	endfor
      endif
    endif
  endif
  fputs (plot_stream, "e\n");

endfunction

function do_tics (obj, plot_stream, ymirror, mono)
  if (strcmpi (obj.xaxislocation, "top"))
    do_tics_1 (obj.xtickmode, obj.xtick, obj.xticklabelmode, obj.xticklabel,
	       obj.xcolor, "x2", plot_stream, true, mono);
    do_tics_1 ("manual", [], obj.xticklabelmode, obj.xticklabel,
	       obj.xcolor, "x", plot_stream, true, mono);
  else
    do_tics_1 (obj.xtickmode, obj.xtick, obj.xticklabelmode, obj.xticklabel,
	       obj.xcolor, "x", plot_stream, true, mono);
    do_tics_1 ("manual", [], obj.xticklabelmode, obj.xticklabel,
	       obj.xcolor, "x2", plot_stream, true, mono);
  endif
  if (strcmpi (obj.yaxislocation, "right"))
    do_tics_1 (obj.ytickmode, obj.ytick, obj.yticklabelmode, obj.yticklabel,
	       obj.ycolor, "y2", plot_stream, ymirror, mono);
    do_tics_1 ("manual", [], obj.yticklabelmode, obj.yticklabel,
	       obj.ycolor, "y", plot_stream, ymirror, mono);
  else
    do_tics_1 (obj.ytickmode, obj.ytick, obj.yticklabelmode, obj.yticklabel,
	       obj.ycolor, "y", plot_stream, ymirror, mono);
    do_tics_1 ("manual", [], obj.yticklabelmode, obj.yticklabel,
	       obj.ycolor, "y2", plot_stream, ymirror, mono);
  endif
  do_tics_1 (obj.ztickmode, obj.ztick, obj.zticklabelmode, obj.zticklabel,
	     obj.zcolor, "z", plot_stream, true, mono);
endfunction

function do_tics_1 (ticmode, tics, labelmode, labels, color, ax,
		    plot_stream, mirror, mono)
  colorspec = get_text_colorspec (color, mono);
  if (strcmpi (ticmode, "manual"))
    if (isempty (tics))
      fprintf (plot_stream, "unset %stics;\n", ax);
    elseif (strcmpi (labelmode, "manual") && ! isempty (labels))
      if (ischar (labels))
	labels = cellstr (labels);
      endif
      if (iscellstr (labels))
	k = 1;
	ntics = numel (tics);
	nlabels = numel (labels);
	fprintf (plot_stream, "set format %s \"%%s\";\n", ax);
	if (mirror)
	  fprintf (plot_stream, "set %stics (", ax);
	else
	  fprintf (plot_stream, "set %stics nomirror (", ax);
	endif
	labels = regexprep(labels, "%", "%%");
	for i = 1:ntics
	  fprintf (plot_stream, " \"%s\" %g", labels{k++}, tics(i))
	  if (i < ntics)
	    fputs (plot_stream, ", ");
	  endif
	  if (k > nlabels)
	    k = 1;
	  endif
	endfor
	fprintf (plot_stream, ") %s;\n", colorspec);
      else
	error ("unsupported type of ticklabel");
      endif
    else
      fprintf (plot_stream, "set format %s \"%%g\";\n", ax);
      if (mirror)
	fprintf (plot_stream, "set %stics (", ax);
      else
	fprintf (plot_stream, "set %stics nomirror (", ax);
      endif
      fprintf (plot_stream, " %g,", tics(1:end-1));
      fprintf (plot_stream, " %g);\n", tics(end));
    endif
  else
    fprintf (plot_stream, "set format %s \"%%g\";\n", ax);
    if (mirror)
      fprintf (plot_stream, "set %stics %s;\n", ax, colorspec);
    else
      fprintf (plot_stream, "set %stics nomirror %s;\n", ax, colorspec);
    endif
  endif
endfunction

function colorspec = get_text_colorspec (color, mono)
  persistent have_newer_gnuplot ...
      = compare_versions (__gnuplot_version__ (), "4.0", ">");

  if (have_newer_gnuplot)
    if (mono)
      colorspec = "";
    else
      colorspec = sprintf ("textcolor rgb \"#%02x%02x%02x\"",
			   round (255*color));
    endif
  else
    if (isequal (color, [0,0,0]))
      typ = -1;
    elseif (isequal (color, [1,0,0]))
      typ = 1;
    elseif (isequal (color, [0,1,0]))
      typ = 2;
    elseif (isequal (color, [0,0,1]))
      typ = 3;
    elseif (isequal (color, [1,0,1]))
      typ = 4;
    elseif (isequal (color, [0,1,1]))
      typ = 5;
    elseif (isequal (color, [1,1,1]))
      typ = -1;
    elseif (isequal (color, [1,1,0]))
      typ = 7;
    else
      typ = -1;
    endif
    colorspec = sprintf ("textcolor lt %d", typ);
  endif
endfunction

function [f, s, fnt, it, bld] = get_fontname_and_size (t)
  if (isempty (t.fontname))
    fnt = "helvetica";
  else
    fnt = tolower (t.fontname);
  endif
  f = fnt;
  it = false;
  bld = false;
  if (! isempty (t.fontweight) && strcmp (tolower (t.fontweight), "bold"))
    if (! isempty(t.fontangle)
	&& (strcmp (tolower (t.fontangle), "italic")
	    || strcmp (tolower (t.fontangle), "oblique")))
      f = strcat (f, "-bolditalic");
      it = true;
      bld = true;
    else
      f = strcat (f, "-bold");
      bld = true;
    endif
  elseif (! isempty(t.fontangle)
	  && (strcmp (tolower (t.fontangle), "italic")
	      || strcmp (tolower (t.fontangle), "oblique")))
    f = strcat (f, "-italic");
    it = true;
  endif
  if (isempty (t.fontsize))
    s = 10;
  else
    s = t.fontsize;
  endif
endfunction

function [str, f, s] = __maybe_munge_text__ (enhanced, obj, fld, 
					     have_newer_gnuplot)
  persistent warned_latex = false;

  if (strcmp (fld, "string"))
    [f, s, fnt, it, bld] = get_fontname_and_size (obj);
  else
    f = "Helvectica";
    s = 10;
    fnt = f;
    it = false;
    bld = false;
  endif

  str = getfield (obj, fld);
  if (enhanced)
    if (strcmp (obj.interpreter, "tex"))
      str = __tex2enhanced__ (str, fnt, it, bld);
      if (! have_newer_gnuplot)
	## Set the font to work around gnuplot 4.0 X11 enhanced terminal bug
	str = strcat ('{/', f, ' ', str, ' }'); 
      endif
    elseif (strcmp (obj.interpreter, "latex"))
      if (! warned_latex)
	warning ("latex text objects not supported");
	warned_latex = true;
      endif
    endif
  endif
endfunction

function str = __tex2enhanced__ (str, fnt, it, bld)
  persistent sym = __setup_sym_table__ ();
  persistent flds = fieldnames (sym);

  [s, e, m] = regexp(str,'\\([a-zA-Z]+|0)','start','end','matches');

  for i = length (s) : -1 : 1
    ## special case for "\0"  and replace with "{/Symbol \306}'
    if (strncmp (m{i}, '\0', 2))
      str = strcat (str(1:s(i) - 1), '{/Symbol \306}', str(s(i) + 2:end));
    else
      f = m{i}(2:end);
      if (isfield (sym, f))
	g = getfield(sym, f);
	## FIXME The symbol font doesn't seem to support bold or italic
	##if (bld)
	##  if (it)
	##    g = regexprep (g, '/Symbol', '/Symbol-bolditalic');
	##  else
	##    g = regexprep (g, '/Symbol', '/Symbol-bold');
	##  endif
	##elseif (it)
	##  g = regexprep (g, '/Symbol', '/Symbol-italic');
	##endif
        str = strcat (str(1:s(i) - 1), g, str(e(i) + 1:end));
      elseif (strncmp (f, "rm", 2))
	bld = false;
	it = false;
        str = strcat (str(1:s(i) - 1), '/', fnt, ' ', str(s(i) + 3:end));
      elseif (strncmp (f, "it", 2) || strncmp (f, "sl", 2))
	it = true;
	if (bld)
          str = strcat (str(1:s(i) - 1), '/', fnt, '-bolditalic ', 
			str(s(i) + 3:end));
        else
          str = strcat (str(1:s(i) - 1), '/', fnt, '-italic ', 
			str(s(i) + 3:end));
        endif
      elseif (strncmp (f, "bf", 2))
	bld = true;
	if (it)
          str = strcat (str(1:s(i) - 1), '/', fnt, '-bolditalic ', 
			str(2(i) + 3:end));
        else
          str = strcat (str(1:s(i) - 1), '/', fnt, '-bold ', 
			str(s(i) + 3:end));
        endif
      elseif (strcmp (f, "color"))
	## FIXME Ignore \color but remove trailing {} block as well
	d = strfind(str(e(i) + 1:end),'}');
        if (isempty (d))
	  warning ('syntax error in \color argument');
	else
	  str = strcat (str(1:s(i) - 1), str(e(i) + d + 1:end));
        endif
      elseif(strcmp (f, "fontname"))
	b1 = strfind(str(e(i) + 1:end),'{');
	b2 = strfind(str(e(i) + 1:end),'}');
        if (isempty(b1) || isempty(b2))
	  warning ('syntax error in \fontname argument');
	else
          str = strcat (str(1:s(i) - 1), '/', 
			str(e(i)+b1(1) + 1:e(i)+b2(1)-1), '{}',
			str(e(i) + b2(1) + 1:end));
        endif
      elseif(strcmp (f, "fontsize"))
	b1 = strfind(str(e(i) + 1:end),'{');
	b2 = strfind(str(e(i) + 1:end),'}');
        if (isempty(b1) || isempty(b2))
	  warning ('syntax error in \fontname argument');
	else
          str = strcat (str(1:s(i) - 1), '/=', 
			str(e(i)+b1(1) + 1:e(i)+b2(1)-1), '{}',
			str(e(i) + b2(1) + 1:end));
        endif
      else
	## Last desperate attempt to treat the symbol. Look for things
	## like \pix, that should be translated to the symbol Pi and x
	for j = 1 : length (flds)
	  if (strncmp (flds{j}, f, length (flds{j})))
	    g = getfield(sym, flds{j});
	    ## FIXME The symbol font doesn't seem to support bold or italic
	    ##if (bld)
	    ##  if (it)
	    ##    g = regexprep (g, '/Symbol', '/Symbol-bolditalic');
	    ##  else
	    ##    g = regexprep (g, '/Symbol', '/Symbol-bold');
	    ##  endif
	    ##elseif (it)
	    ##  g = regexprep (g, '/Symbol', '/Symbol-italic');
	    ##endif
            str = strcat (str(1:s(i) - 1), g, 
	    		  str(s(i) + length (flds{j}) + 1:end));
	    break;
	  endif
	endfor
      endif
    endif
  endfor

  ## Prepend @ to things  things like _0^x or _{-100}^{100} for alignment
  ## But need to put the shorter of the two arguments first. Carful of 
  ## nested {} and unprinted characters when defining shortest.. Don't 
  ## have to worry about things like ^\theta as they are already converted to
  ## ^{/Symbol q}.

  ## FIXME.. This is a mess... Is it worth it just for a "@" character?

  [s, m] = regexp(str,'[_\^]','start','matches');
  i = 1;
  p = 0;
  while (i < length (s))
    if (i < length(s))
      if (str(s(i) + p + 1) == "{")
	s1 = strfind(str(s(i) + p + 2:end),'{');
	si = 1;
	l1 = strfind(str(s(i) + p + 1:end),'}');
        li = 1;
	while (li <= length (l1) && si <= length (s1))
          if (l1(li) < s1(si))
	    if (li == si)
	      break;
	    endif
	    li++;
	  else
	    si++;
	  endif
	endwhile
	l1 = l1 (min (length(l1), si));
        if (s(i) + l1 + 1 == s(i+1))
	  if (str(s(i + 1) + p + 1) == "{")
	    s2 = strfind(str(s(i + 1) + p + 2:end),'{');
	    si = 1;
	    l2 = strfind(str(s(i + 1) + p + 1:end),'}');
            li = 1;
	    while (li <= length (l2) && si <= length (s2))
              if (l2(li) < s2(si))
		if (li == si)
		  break;
		endif
		li++;
	      else
		si++;
	      endif
	    endwhile
	    l2 = l2 (min (length(l2), si));
	    if (length_string (str(s(i)+p+2:s(i)+p+l1-1)) <=
		length_string(str(s(i+1)+p+2:s(i+1)+p+l2-1)))
	      ## shortest already first!
	      str = strcat (str(1:s(i)+p-1), "@", str(s(i)+p:end));
	    else
	      ## Have to swap sub/super-script to get shortest first
	      str = strcat (str(1:s(i)+p-1), "@", str(s(i+1)+p:s(i+1)+p+l2),
			    str(s(i)+p:s(i)+p+l1), str(s(i+1)+p+l2+1:end));
	    endif
	  else
	    ## Have to swap sub/super-script to get shortest first
	    str = strcat (str(1:s(i)+p-1), "@", str(s(i+1)+p:s(i+1)+p+1),
			  str(s(i)+p:s(i)+p+l1), str(s(i+1)+p+2:end));
	  endif
          i += 2;
	  p ++;
	else
	  i++;
	endif
      else
	if (s(i+1) == s(i) + 2)
	  ## shortest already first!
	  str = strcat (str(1:s(i)+p-1), "@", str(s(i)+p:end));
	  p ++;
          i += 2;
	else
	  i ++;
	endif
      endif
    else
      i ++;
    endif
  endwhile

endfunction

function l = length_string (s)
  l = length (s) - length (strfind(s,'{')) - length (strfind(s,'}'));
  m = regexp (s, '/([\w\-]+|[\w\-]+=\d+)', 'matches');
  if (!isempty (m))
    l = l - sum (cellfun (@length, m));
  endif
endfunction

function sym = __setup_sym_table__ ()
  ## Setup the translation table for TeX to gnuplot enhanced mode.
  sym.forall = '{/Symbol \042}';
  sym.exists = '{/Symbol \044}';
  sym.ni = '{/Symbol \047}';
  sym.cong = '{/Symbol \100}';
  sym.Delta = '{/Symbol D}';
  sym.Phi = '{/Symbol F}';
  sym.Gamma = '/Symbol G}';
  sym.vartheta = '{/Symbol J}';
  sym.Lambda = '{/Symbol L}';
  sym.Pi = '{/Symbol P}';
  sym.Theta = '{/Symbol Q}';
  sym.Sigma = '{/Symbol S}';
  sym.varsigma = '{/Symbol V}';
  sym.Omega = '{/Symbol O}';
  sym.Xi = '{/Symbol X}';
  sym.Psi = '{/Symbol Y}';
  sym.perp = '{/Symbol \136}';
  sym.alpha = '{/Symbol a}';
  sym.beta = '{/Symbol b}';
  sym.chi = '{/Symbol c}';
  sym.delta = '{/Symbol d}';
  sym.epsilon = '{/Symbol e}';
  sym.phi = '{/Symbol f}';
  sym.gamma = '/Symbol g}';
  sym.eta = '{/Symbol h}';
  sym.iota = '{/Symbol i}';
  sym.kappa = '{/Symbol k}';
  sym.lambda = '{/Symbol l}';
  sym.mu = '{/Symbol m}';
  sym.nu = '{/Symbol n}';
  sym.o =  '{/Symbol o}';
  sym.pi = '{/Symbol p}';
  sym.theta = '{/Symbol q}';
  sym.rho = '{/Symbol r}';
  sym.sigma = '{/Symbol s}';
  sym.tau = '{/Symbol t}';
  sym.upsilon = '{/Symbol u}';
  sym.varpi = '{/Symbol v}';
  sym.omega = '{/Symbol w}';
  sym.xi = '{/Symbol x}';
  sym.psi = '{/Symbol y}';
  sym.zeta = '{/Symbol z}';
  sym.sim = '{/Symbol \176}';
  sym.Upsilon = '{/Symbol \241}';
  sym.prime = '{/Symbol \242}';
  sym.leq = '{/Symbol \243}';
  sym.infty = '{/Symbol \245}';
  sym.clubsuit = '{/Symbol \247}';
  sym.diamondsuit = '{/Symbol \250}';
  sym.heartsuit = '{/Symbol \251}';
  sym.spadesuit = '{/Symbol \252}';
  sym.leftrightarrow = '{/Symbol \253}';
  sym.leftarrow = '{/Symbol \254}';
  sym.uparrow = '{/Symbol \255}';
  sym.rightarrow = '{/Symbol \256}';
  sym.downarrow = '{/Symbol \257}';
  sym.circ = '{/Symbol \260}';
  sym.pm = '{/Symbol \261}';
  sym.geq = '{/Symbol \263}';
  sym.times = '{/Symbol \264}';
  sym.propto = '{/Symbol \265}';
  sym.partial = '{/Symbol \266}';
  sym.bullet = '{/Symbol \267}';
  sym.div = '{/Symbol \270}';
  sym.neq = '{/Symbol \271}';
  sym.equiv = '{/Symbol \272}';
  sym.approx = '{/Symbol \273}';
  sym.ldots = '{/Symbol \274}';
  sym.mid = '{/Symbol \275}';
  sym.aleph = '{/Symbol \300}';
  sym.Im = '{/Symbol \301}';
  sym.Re = '{/Symbol \302}';
  sym.wp = '{/Symbol \303}';
  sym.otimes = '{/Symbol \304}';
  sym.oplus = '{/Symbol \305}';
  sym.oslash = '{/Symbol \306}';
  sym.cap = '{/Symbol \307}';
  sym.cup = '{/Symbol \310}';
  sym.supset = '{/Symbol \311}';
  sym.supseteq = '{/Symbol \312}';
  sym.subset = '{/Symbol \314}';
  sym.subseteq = '{/Symbol \315}';
  sym.in = '{/Symbol \316}';
  sym.langle = '{/Symbol \320}';
  sym.rangle = '{/Symbol \320}';
  sym.nabla = '{/Symbol \321}';
  sym.surd = '{/Symbol \326}';
  sym.cdot = '{/Symbol \327}';
  sym.neg = '{/Symbol \330}';
  sym.wedge = '{/Symbol \331}';
  sym.vee = '{/Symbol \332}';
  sym.copyright = '{/Symbol \343}';
  sym.rfloor = '{/Symbol \353}';
  sym.lceil  = '{/Symbol \351}';
  sym.lfloor = '{/Symbol \373}';
  sym.rceil  = '{/Symbol \371}';
  sym.int = '{/Symbol \362}';
endfunction

function [pos, orient, sz, origin, mirr] = gnuplot_postion_colorbox (pos, cbox)
  ## This is an emprically derived function that 

  if (strncmp (cbox, "north", 5) || strncmp (cbox, "south", 5))
    scl = pos([2,4]);
  else
    scl = pos([1,3]);
  endif

  if (length(cbox) > 7 && strncmp (cbox(end-6:end), "outside", 7))
    scl(2) -= 0.2 * scl(2);
    if (strncmp (cbox, "west", 4) || strncmp (cbox, "south", 5))
      scl(1) += 0.2 * scl(2);
    endif
  endif

  switch (cbox)
    case "northoutside"
      sz = pos(3:4) - 0.08;
      origin = [0.05, 0.06] + [0.00, 0.88] .* sz + pos(1:2);
      mirr = true;
      orient = "horizontal";
    case "north"
      sz = pos(3:4) - 0.16;
      origin = [0.09, 0.09] + [0.00, 0.94] .* sz + pos(1:2);
      mirr = false;
      orient = "horizontal";
    case "southoutside"
      sz = pos(3:4) - 0.08;
      origin = [0.05, 0.06] + [0.00, 0.00] .* sz + pos(1:2);
      mirr = false;
      orient = "horizontal";
    case "south"
      sz = pos(3:4) - 0.16;
      origin = [0.08, 0.09] + [0.03, 0.05] .* sz + pos(1:2);
      mirr = true;
      orient = "horizontal";
    case "eastoutside"
      sz = pos(3:4) - 0.08;
      origin = [0.00, 0.06] + [0.94, 0.00] .* sz + pos(1:2);
      mirr = false;
      orient = "vertical";
    case "east"
      sz = pos(3:4) - 0.16;
      origin = [0.09, 0.10] + [0.91, 0.01] .* sz + pos(1:2);
      mirr = true;
      orient = "vertical";
    case "westoutside"
      sz = pos(3:4) - 0.08;
      origin = [0.00, 0.06] + [0.06, 0.00] .* sz + pos(1:2);
      mirr = true;
      orient = "vertical";
    case "west"
      sz = pos(3:4) - 0.16;
      origin = [0.06, 0.09] + [0.04, 0.03] .* sz + pos(1:2);
      mirr = false;
      orient = "vertical";
  endswitch

  if (strncmp (cbox, "north", 5) || strncmp (cbox, "south", 5))
    sz = sz .* [1, 0.07];
    pos([2,4]) = scl;
  else
    sz = sz .* [0.07, 1];
    pos([1,3]) = scl;
  endif

endfunction
