## Copyright (C) 2005 John W. Eaton
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} __go_draw_axes__ (@var{axis_obj}, @var{plot_stream})
## Display the axes @var{axis_obj} on @var{plot_stream}.
## @end deftypefn

## Author: jwe

function __go_draw_axes__ (h, plot_stream)

  if (nargin == 2)

    axis_obj = get (h);

    parent_figure_obj = get (axis_obj.parent);

    persistent have_newer_gnuplot ...
      = compare_versions (__gnuplot_version__ (), "4.0", ">");

    ## Set axis properties here?

    if (! isempty (axis_obj.outerposition))
      pos = axis_obj.outerposition;
      fprintf (plot_stream, "set origin %g, %g;\n", pos(1), pos(2));
      fprintf (plot_stream, "set size %g, %g;\n", pos(3), pos(4));
    endif

    if (! isempty (axis_obj.position))
      pos = axis_obj.position;
      fprintf (plot_stream, "set origin %g, %g;\n", pos(1), pos(2));
      fprintf (plot_stream, "set size %g, %g;\n", pos(3), pos(4));
    endif

    if (strcmpi (axis_obj.dataaspectratiomode, "manual"))
      r = axis_obj.dataaspectratio;
      fprintf (plot_stream, "set size ratio %g;\n", -r(2)/r(1));
    else
      fputs (plot_stream, "set size noratio;\n");
    endif

    if (! isempty (axis_obj.title))
      t = get (axis_obj.title);
      if (isempty (t.string))
	fputs (plot_stream, "unset title;\n");
      else
	fprintf (plot_stream, "set title \"%s\";\n",
		 undo_string_escapes (t.string));
      endif
    endif

    if (! isempty (axis_obj.xlabel))
      t = get (axis_obj.xlabel);
      angle = t.rotation;
      if (isempty (t.string))
	fputs (plot_stream, "unset xlabel;\n");
      else
	fprintf (plot_stream, "set xlabel \"%s\"",
		 undo_string_escapes (t.string));
	if (have_newer_gnuplot)
	  ## Rotation of xlabel not yet support by gnuplot as of 4.2, but
	  ## there is no message about it.
	  fprintf (plot_stream, " rotate by %f", angle);
	endif
	fputs (plot_stream, ";\n");
      endif
    endif

    if (! isempty (axis_obj.ylabel))
      t = get (axis_obj.ylabel);
      angle = t.rotation;
      if (isempty (t.string))
	fputs (plot_stream, "unset ylabel;\n");
      else
	fprintf (plot_stream, "set ylabel \"%s\"",
		 undo_string_escapes (t.string));
	if (have_newer_gnuplot)
	  fprintf (" rotate by %f;\n", angle);
	endif
	fputs (plot_stream, ";\n");
      endif
    endif

    if (! isempty (axis_obj.zlabel))
      t = get (axis_obj.zlabel);
      angle = t.rotation;
      if (isempty (t.string))
	fputs (plot_stream, "unset zlabel;\n");
      else
	fprintf (plot_stream, "set zlabel \"%s\"",
		 undo_string_escapes (t.string));
	if (have_newer_gnuplot)
	  ## Rotation of zlabel not yet support by gnuplot as of 4.2, but
	  ## there is no message about it.
	  fprintf (plot_stream, " rotate by %f;\n", angle);
	endif
	fputs (plot_stream, ";\n");
      endif
    endif

    if (strcmpi (axis_obj.xgrid, "on"))
      fputs (plot_stream, "set grid xtics;\n");
    else
      fputs (plot_stream, "set grid noxtics;\n");
    endif

    if (strcmpi (axis_obj.ygrid, "on"))
      fputs (plot_stream, "set grid ytics;\n");
    else
      fputs (plot_stream, "set grid noytics;\n");
    endif

    if (strcmpi (axis_obj.zgrid, "on"))
      fputs (plot_stream, "set grid ztics;\n");
    else
      fputs (plot_stream, "set grid ztics;\n");
    endif

    if (strcmpi (axis_obj.xminorgrid, "on"))
      fputs (plot_stream, "set mxtics 5;\n");
      fputs (plot_stream, "set grid mxtics;\n");
    else
      fputs (plot_stream, "set grid nomxtics;\n");
    endif

    if (strcmpi (axis_obj.yminorgrid, "on"))
      fputs (plot_stream, "set mytics 5;\n");
      fputs (plot_stream, "set grid mytics;\n");
    else
      fputs (plot_stream, "set grid nomytics;\n");
    endif

    if (strcmpi (axis_obj.zminorgrid, "on"))
      fputs (plot_stream, "set mztics 5;\n");
      fputs (plot_stream, "set grid mztics;\n");
    else
      fputs (plot_stream, "set grid nomztics;\n");
    endif

    do_tics (axis_obj, plot_stream);

    xlogscale = strcmpi (axis_obj.xscale, "log");
    if (xlogscale)
      fputs (plot_stream, "set logscale x;\n");
    else
      fputs (plot_stream, "unset logscale x;\n");
    endif

    ylogscale = strcmpi (axis_obj.yscale, "log");
    if (ylogscale)
      fputs (plot_stream, "set logscale y;\n");
    else
      fputs (plot_stream, "unset logscale y;\n");
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

    kids = axis_obj.children;

    nd = 0;
    data_idx = 0;
    data = cell ();
    is_image_data = [];

    xminp = yminp = zminp = realmax ();
    xmax = ymax = zmax = -realmax ();
    xmin = ymin = zmin = realmax ();

    [view_cmd, view_fcn, view_zoom] = image_viewer ();
    use_gnuplot_for_images = (ischar (view_fcn)
			      && strcmp (view_fcn, "gnuplot_internal"));

    ximg_data = {};
    ximg_data_idx = 0;

    for i = 1:length (kids)

      obj = get (kids(i));

      switch (obj.type)
	case "image"
	  ## FIXME - Is there a better way to determine if the plot
	  ## command should be "plot" or "splot"?????  Could have images
	  ## projected into 3D so there is really no reason to limit
	  ## this.
	  if (nd == 0)
	    nd = 2;
	  endif

	  img_data = obj.cdata;
	  img_colormap = parent_figure_obj.colormap;
	  img_xdata = obj.xdata;
	  img_ydata = obj.ydata;

	  if (use_gnuplot_for_images)

	    data_idx++;
	    is_image_data(data_idx) = true;

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

	    if (nd == 2)
	      if (xautoscale)
		xmin = min (xmin, min (img_xdata) - dx/2);
		xmax = max (xmax, max (img_xdata) + dx/2);
		xminp = min (xminp, min (img_xdata((img_xdata - dx/2)>0)) - dx/2);
	      endif
	      if (yautoscale)
		ymin = min (ymin, min (img_ydata) - dy/2);
		ymax = max (ymax, max (img_ydata) + dy/2);
		yminp = min (yminp, min (img_ydata((img_ydata - dy/2)>0)) - dy/2);
	      endif
	    else
	      ## Can have images in 3D, but the image routines don't seem
	      ## to have a means of arbitrary projection.
	    endif

	    if (ndims (img_data) == 3)
	      data{data_idx} = permute (img_data, [3, 1, 2])(:);
	      format = "1:2:3";
	      imagetype = "rgbimage";
	    else
	      data{data_idx} = img_data(:);
	      format = "1";
	      imagetype = "image";

	      palette_size = rows (img_colormap);
	      fprintf (plot_stream,
		       "set palette positive color model RGB maxcolors %i;\n",
		       palette_size);
	      fprintf (plot_stream,
		       "set palette file \"-\" binary record=%d using 1:2:3:4;\n",
		       palette_size);
	      fwrite (plot_stream, [1:palette_size; img_colormap'], "float32");
	    endif

	    titlespec{data_idx} = "title \"\"";
	    usingclause{data_idx} = sprintf ("binary array=%dx%d scan=yx origin=(%g,%g) dx=%g dy=%g using %s",
		x_dim, y_dim, x_origin, y_origin, dx, dy, format);
	    withclause{data_idx} = sprintf ("with %s", imagetype);

	  else
	    ximg_data{++ximg_data_idx} = img_data;
	  endif

	case "line"
	  data_idx++;
	  is_image_data(data_idx) = false;
	  if (isempty (obj.keylabel))
	    titlespec{data_idx} = "title \"\"";
	  else
	    titlespec{data_idx} = strcat ("title \"", obj.keylabel, "\"");
	  endif
	  [style, typ, with] = do_linestyle_command (obj, data_idx, plot_stream);
	  usingclause{data_idx} = "";
	  if (have_newer_gnuplot || isnan (typ))
	    withclause{data_idx} = sprintf ("with %s linestyle %d",
					    style, data_idx);
	  else
	    withclause{data_idx} = sprintf ("with %s linetype %d",
					    style, typ);
	  endif
	  parametric(i) = true;
	  if (! isempty (obj.zdata))
	    nd = 3;
	    xdat = obj.xdata(:);
	    ydat = obj.ydata(:);
	    zdat = obj.zdata(:);
	    if (xautoscale)
	      [xmin, xmax, xminp] = get_data_limits (xmin, xmax, xminp, xdat);
	    endif
	    if (yautoscale)
	      [ymin, ymax, yminp] = get_data_limits (ymin, ymax, yminp, ydat);
	    endif
	    if (zautoscale)
	      [zmin, zmax, zminp] = get_data_limits (zmin, zmax, zminp, zdat);
	    endif
	    data{data_idx} = [xdat, ydat, zdat]';
	    usingclause{data_idx} = "using ($1):($2):($3)";
	    fputs (plot_stream, "set parametric;\n");
	    fputs (plot_stream, "unset hidden3d;\n");
	    fputs (plot_stream, "set style data lines;\n");
	    fputs (plot_stream, "set surface;\n");
	    fputs (plot_stream, "unset contour;\n");
	  else
	    nd = 2;
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
	      ylo = ydat-ldat;
	      yhi = ydat+udat;
	      if (yautoscale)
		ty = [ydat; ylo; yhi];
		[ymin, ymax, yminp] = get_data_limits (ymin, ymax, yminp, ty);
	      endif
	      if (xerr)
		xlo = xdat-xldat;
		xhi = xdat+xudat;
		if (xautoscale)
		  tx = [xdat; xlo; xhi];
		  [xmin, xmax, xminp] = get_data_limits (xmin, xmax, xminp, tx);
		endif
		data{data_idx} = [xdat, ydat, xlo, xhi, ylo, yhi]';
		usingclause{data_idx} = "using ($1):($2):($3):($4):($5):($6)";
		withclause{data_idx} = sprintf ("with xyerrorbars linestyle %d",
						data_idx);
	      else
		## Obtain the limits based on the exact x values.
		if (xautoscale)
		  [xmin, xmax, xminp] = get_data_limits (xmin, xmax,
							 xminp, xdat);
		endif
		data{data_idx} = [xdat, ydat, ylo, yhi]';
		usingclause{data_idx} = "using ($1):($2):($3):($4)";
		withclause{data_idx} = sprintf ("with yerrorbars linestyle %d",
						data_idx);
	      endif
	    elseif (xerr)
	      xlo = xdat-xldat;
	      xhi = xdat+xudat;
	      if (xautoscale)
		tx = [xdat; xlo; xhi];
		[xmin, xmax, xminp] = get_data_limits (xmin, xmax, xminp, tx);
	      endif
	      if (yautoscale)
		[ymin, ymax, yminp] = get_data_limits (ymin, ymax,
						       yminp, ydat, ty);
	      endif
	      data{data_idx} = [xdat, ydat, xlo, xhi]';
	      usingclause{data_idx} = "using ($1):($2):($3):($4)";
	      withclause{data_idx} = sprintf ("with xerrorbars linestyle %d",
					      data_idx);
	    else
	      if (xautoscale)
		[xmin, xmax, xminp] = get_data_limits (xmin, xmax, xminp, xdat);
	      endif
	      if (yautoscale)
		[ymin, ymax, yminp] = get_data_limits (ymin, ymax, yminp, ydat);
	      endif
	      data{data_idx} = [xdat, ydat]';
	      usingclause{data_idx} = "using ($1):($2)";
	    endif
	  endif
	  if (! (have_newer_gnuplot || isempty (with)))
	    if (isempty (withclause{data_idx}))
	      withclause{data_idx} = sprintf("with %s", with);
	    else
	      withclause{data_idx} = sprintf("%s %s", withclause{data_idx}, with);
	    endif
	  endif

	case "surface"
	  data_idx++;
	  is_image_data(data_idx) = false;
	  [style, typ, with] = do_linestyle_command (obj, data_idx, plot_stream);
	  if (isempty (obj.keylabel))
	    titlespec{data_idx} = "title \"\"";
	  else
	    titlespec{data_idx} = strcat ("title \"", obj.keylabel, "\"");
	  endif
	  usingclause{data_idx} = "";
	  if (have_newer_gnuplot || isnan (typ))
	    withclause{data_idx} = sprintf ("with %s linestyle %d",
					    style, data_idx);
	  else
	    withclause{data_idx} = sprintf ("with %s linetype %d %s",
					    style, typ, with);
	  endif
	  parametric(i) = false;
	  nd = 3;
	  xdat = obj.xdata;
	  ydat = obj.ydata;
	  zdat = obj.zdata;
	  if (xautoscale)
	    tx = xdat(:);
	    [xmin, xmax, xminp] = get_data_limits (xmin, xmax, xminp, tx);
	  endif
	  if (yautoscale)
	    ty = ydat(:);
	    [ymin, ymax, yminp] = get_data_limits (ymin, ymax, yminp, ty);
	  endif
	  if (zautoscale)
	    tz = zdat(:);
	    [zmin, zmax, zminp] = get_data_limits (zmin, zmax, zminp, tz);
	  endif
	  err = false;
	  if (isvector (xdat) && isvector (ydat) && ismatrix (zdat))
	    if (rows (zdat) == length (ydat) && columns (zdat) == length (xdat))
              [xdat, ydat] = meshgrid (xdat, ydat);
	    else
              err = true;
	    endif
	  elseif (ismatrix (xdat) && ismatrix (ydat) && ismatrix (zdat))
	    if (! (size_equal (xdat, ydat) && size_equal (xdat, zdat)))
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
	    len = 3 * xlen;
	    zz = zeros (ylen, len);
	    k = 1;
	    for kk = 1:3:len
	      zz(:,kk)   = xdat(:,k);
	      zz(:,kk+1) = ydat(:,k);
	      zz(:,kk+2) = zdat(:,k);
	      k++;
	    endfor
	    data{data_idx} = zz;
	  endif
	  usingclause{data_idx} = "using ($1):($2):($3)";
	  withclause{data_idx} = "with line palette";

	  fputs (plot_stream, "unset parametric;\n");
	  fputs (plot_stream, "set hidden3d;\n");
	  fputs (plot_stream, "set style data lines;\n");
	  fputs (plot_stream, "set surface;\n");
	  fputs (plot_stream, "unset contour;\n");
	  fputs (plot_stream, "set palette defined (0 \"dark-blue\", 1 \"blue\", 2 \"cyan\", 3 \"yellow\", 4 \"red\" , 5 \"dark-red\");\n");
	  fputs (plot_stream, "unset colorbox;\n");

	case "text"
	  lpos = obj.position;
	  label = obj.string;
	  halign = obj.horizontalalignment;
	  angle = obj.rotation;
          units = obj.units;
          if (strcmpi (units, "normalized"))
            units = "graph";
          else
            units = "";
          endif
	  
	  if (nd == 3)
	    fprintf (plot_stream,
		     "set label \"%s\" at %s %g,%g,%g %s rotate by %f;\n",
		     undo_string_escapes (label), units,
		     lpos(1), lpos(2), lpos(3), halign, angle);
	  else
	    fprintf (plot_stream,
		     "set label \"%s\" at %s %g,%g %s rotate by %f;\n",
		     undo_string_escapes (label), units,
		     lpos(1), lpos(2), halign, angle);
	  endif

	otherwise
	  error ("__go_draw_axes__: unknown object class, %s",
		 obj.type);
      endswitch

    endfor

    have_data = ! isempty (data);

    if (xautoscale && have_data)
      xlim = get_axis_limits (xmin, xmax, xminp, xlogscale);
      set (h, "xlim", xlim, "xlimmode", "auto");
    else
      xlim = axis_obj.xlim;
    endif
    if (strcmpi (axis_obj.xdir, "reverse"))
      xdir = "reverse";
    else
      xdir = "noreverse";
    endif
    fprintf (plot_stream, "set xrange [%g:%g] %s;\n", xlim, xdir);

    if (yautoscale && have_data)
      ylim = get_axis_limits (ymin, ymax, yminp, ylogscale);
      set (h, "ylim", ylim, "ylimmode", "auto");
    else
      ylim = axis_obj.ylim;
    endif
    if (strcmpi (axis_obj.ydir, "reverse"))
      ydir = "reverse";
    else
      ydir = "noreverse";
    endif
    fprintf (plot_stream, "set yrange [%g:%g] %s;\n", ylim, ydir);

    if (nd == 3)
      if (zautoscale && have_data)
	zlim = get_axis_limits (zmin, zmax, zminp, zlogscale);
	set (h, "zlim", zlim, "zlimmode", "auto");
      else
	zlim = axis_obj.zlim;
      endif
      if (strcmpi (axis_obj.zdir, "reverse"))
	zdir = "reverse";
      else
	zdir = "noreverse";
      endif
      fprintf (plot_stream, "set zrange [%g:%g] %s;\n", zlim, zdir);
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

    if (strcmpi (axis_obj.key, "on"))
      if (strcmpi (axis_obj.keybox, "on"))
	box = "box";
      else
	box = "nobox";
      endif
      inout = "inside";
      switch (axis_obj.keypos)
	case -1
	  pos = "right bottom";
	  inout = "outside";
	case 1
	  pos = "right top";
	case 2
	  pos = "left top";
	case 3
	  pos = "left bottom";
	case 4
	  pos = "right bottom";
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
	fprintf (plot_stream, "set view %g, %g;\n", rot_x, rot_z);
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
	  __gnuplot_write_data__ (plot_stream, data{i}, nd, parametric(i));
	endif
      endfor
    else
      fputs (plot_stream, "plot \"-\";\nInf Inf\ne\n");
    endif

    fflush (plot_stream);

  else
    print_usage ();
  endif    

endfunction

function [xmin, xmax, xminp] = get_data_limits (xmin, xmax, xminp, xdat, tx)
  xdat = xdat(! isinf (xdat));
  xmin = min (xmin, min (xdat));
  xmax = max (xmax, max (xdat));
  if (nargin == 5)
    tx = tx(! isinf (xdat) & tx > 0);
    xminp = min (xminp, min (tx));
  else
    xminp = min (xminp, min (xdat(xdat>0)));
  endif
endfunction

## Attempt to make "nice" limits from the actual max and min of the
## data.  For log plots, we will also use the smallest strictly positive
## value.

function lim = get_axis_limits (min_val, max_val, min_pos, logscale)

  if (logscale)
    if (isinf (min_pos))
      warning ("axis: logscale with no positive values to plot");
    endif
    if (min_val <= 0)
      min_val = min_pos;
      if (max_val <= 0)
	max_val = min_pos;
      endif
      warning ("axis: omitting nonpositive data in log plot");
    endif
    ## FIXME -- maybe this test should also be relative?
    if (abs (min_val - max_val) < sqrt (eps))
      min_val *= 0.9;
      max_val *= 1.1;
    endif
    min_val = 10 ^ floor (log10 (min_val));
    max_val = 10 ^ ceil (log10 (max_val));
  else
    if (min_val == 0 && max_val == 0)
      min_val = -1;
      max_val = 1;
    ## FIXME -- maybe this test should also be relative?
    elseif (abs (min_val - max_val) < sqrt (eps))
      min_val -= 0.1 * abs (min_val);
      max_val += 0.1 * abs (max_val);
    endif
    ## FIXME -- to do a better job, we should consider the tic spacing.
    scale = 10 ^ floor (log10 (max_val - min_val) - 1);
    min_val = scale * floor (min_val / scale);
    max_val = scale * ceil (max_val / scale);
  endif

  lim = [min_val, max_val];

endfunction

function [style, typ, with] = do_linestyle_command (obj, idx, plot_stream)

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
	fprintf (plot_stream, " linecolor rgb \"#%02x%02x%02x\"",
		 round (255*color));
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
    if (! isempty (lt))
      fprintf (plot_stream, " linetype %s", lt);
      found_style = true;
    endif
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
	pt = "7";
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

function __gnuplot_write_data__ (plot_stream, data, nd, parametric)
  
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
    fmt = strcat (repmat ("%g ", 1, rows (data)), "\n");
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
  else
    ## FIXME -- handle NaNs here too?
    if (parametric)
      fprintf (plot_stream, "%g %g %g\n", data);
    else
      nc = columns (data);
      for j = 1:3:nc
	fprintf (plot_stream, "%g %g %g\n", data(:,j:j+2)');
	fputs (plot_stream, "\n");
      endfor
    endif
  endif
  fputs (plot_stream, "e\n");

endfunction

function do_tics (obj, plot_stream)
  do_tics_1 (obj.xtickmode, obj.xtick, obj.xticklabelmode, obj.xticklabel,
	     "x", plot_stream);
  do_tics_1 (obj.ytickmode, obj.ytick, obj.yticklabelmode, obj.yticklabel,
	     "y", plot_stream);
  do_tics_1 (obj.ztickmode, obj.ztick, obj.zticklabelmode, obj.zticklabel,
	     "z", plot_stream);
endfunction

function do_tics_1 (ticmode, tics, labelmode, labels, ax, plot_stream)
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
	fprintf (plot_stream, "set %stics (", ax);
	for i = 1:ntics
	  fprintf (plot_stream, " \"%s\" %g", labels(k++), tics(i))
	  if (i < ntics)
	    fputs (plot_stream, ", ");
	  endif
	  if (k > nlabels)
	    k = 1;
	  endif
	endfor
	fputs (plot_stream, ");\n");
      else
	error ("unsupported type of ticklabel");
      endif
    else
      fprintf (plot_stream, "set %stics (", ax);
      fprintf (plot_stream, " %g,", tics(1:end-1));
      fprintf (plot_stream, " %g);\n", tics(end));
    endif
  else
    fprintf (plot_stream, "set %stics;\n", ax);
  endif
endfunction
