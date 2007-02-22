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
## @deftypefn {Function File} {} __uiobject_draw_axes__ (@var{axis_obj}, @var{plot_stream})
## Display the axes @var{axis_obj} on @var{plot_stream}.
## @end deftypefn

## Author: jwe

function __uiobject_draw_axes__ (h, plot_stream)

  if (nargin == 2)

    axis_obj = get (h);

    parent_figure_obj = get (axis_obj.parent);

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

    if (strcmp (axis_obj.dataaspectratiomode, "manual"))
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
      if (isempty (t.string))
	fputs (plot_stream, "unset xlabel;\n");
      else
	fprintf (plot_stream, "set xlabel \"%s\";\n",
		 undo_string_escapes (t.string));
      endif
    endif

    if (! isempty (axis_obj.ylabel))
      t = get (axis_obj.ylabel);
      if (isempty (t.string))
	fputs (plot_stream, "unset ylabel;\n");
      else
	fprintf (plot_stream, "set ylabel \"%s\";\n",
		 undo_string_escapes (t.string));
      endif
    endif

    if (! isempty (axis_obj.zlabel))
      t = get (axis_obj.zlabel);
      if (isempty (t.string))
	fputs (plot_stream, "unset zlabel;\n");
      else
	fprintf (plot_stream, "set zlabel \"%s\";\n",
		 undo_string_escapes (t.string));
      endif
    endif

    if (strcmp (axis_obj.xgrid, "on"))
      fputs (plot_stream, "set grid xtics;\n");
    else
      fputs (plot_stream, "set grid noxtics;\n");
    endif

    if (strcmp (axis_obj.ygrid, "on"))
      fputs (plot_stream, "set grid ytics;\n");
    else
      fputs (plot_stream, "set grid noytics;\n");
    endif

    if (strcmp (axis_obj.zgrid, "on"))
      fputs (plot_stream, "set grid ztics;\n");
    else
      fputs (plot_stream, "set grid ztics;\n");
    endif

    if (strcmp (axis_obj.xminorgrid, "on"))
      fputs (plot_stream, "set mxtics 5;\n");
      fputs (plot_stream, "set grid mxtics;\n");
    else
      fputs (plot_stream, "set grid nomxtics;\n");
    endif

    if (strcmp (axis_obj.yminorgrid, "on"))
      fputs (plot_stream, "set mytics 5;\n");
      fputs (plot_stream, "set grid mytics;\n");
    else
      fputs (plot_stream, "set grid nomytics;\n");
    endif

    if (strcmp (axis_obj.zminorgrid, "on"))
      fputs (plot_stream, "set mztics 5;\n");
      fputs (plot_stream, "set grid mztics;\n");
    else
      fputs (plot_stream, "set grid nomztics;\n");
    endif

    if (strcmp (axis_obj.xtickmode, "manual"))
      xtic = axis_obj.xtick;
      if (isempty (xtic))
	fputs (plot_stream, "unset xtics;\n");
      else
	## FIXME
      endif
    else
      fputs (plot_stream, "set xtics;\n");
    endif

    if (strcmp (axis_obj.ytickmode, "manual"))
      ytic = axis_obj.ytick;
      if (isempty (ytic))
	fputs (plot_stream, "unset ytics;\n");
      else
	## FIXME
      endif
    else
      fputs (plot_stream, "set ytics;\n");
    endif

    if (strcmp (axis_obj.ztickmode, "manual"))
      ztic = axis_obj.ztick;
      if (isempty (ztic))
	fputs (plot_stream, "unset ztics;\n");
      else
	## FIXME
      endif
    else
      fputs (plot_stream, "set ztics;\n");
    endif

    if (strcmp (axis_obj.xticklabelmode, "manual"))
      ## FIXME -- we should be able to specify the actual tick labels,
      ## not just the format.
      xticlabel = axis_obj.xticklabel;
      fprintf (plot_stream, "set format x \"%s\";\n", xticlabel);
    else
      fputs (plot_stream, "set xtics;\n");
    endif

    if (strcmp (axis_obj.yticklabelmode, "manual"))
      ## FIXME -- we should be able to specify the actual tick labels,
      ## not just the format.
      yticlabel = axis_obj.yticklabel;
      fprintf (plot_stream, "set format y \"%s\";\n", yticlabel);
    else
      fputs (plot_stream, "set ytics;\n");
    endif

    if (strcmp (axis_obj.zticklabelmode, "manual"))
      ## FIXME -- we should be able to specify the actual tick labels,
      ## not just the format.
      zticlabel = axis_obj.zticklabel;
      fprintf (plot_stream, "set format z \"%s\";\n", zticlabel);
    else
      fputs (plot_stream, "set ztics;\n");
    endif

    xlogscale = strcmp (axis_obj.xscale, "log");
    if (xlogscale)
      fputs (plot_stream, "set logscale x;\n");
    else
      fputs (plot_stream, "unset logscale x;\n");
    endif

    ylogscale = strcmp (axis_obj.yscale, "log");
    if (ylogscale)
      fputs (plot_stream, "set logscale y;\n");
    else
      fputs (plot_stream, "unset logscale y;\n");
    endif

    zlogscale = strcmp (axis_obj.zscale, "log");
    if (zlogscale)
      fputs (plot_stream, "set logscale z;\n");
    else
      fputs (plot_stream, "unset logscale z;\n");
    endif

    xautoscale = strcmp (axis_obj.xlimmode, "auto");
    yautoscale = strcmp (axis_obj.ylimmode, "auto");
    zautoscale = strcmp (axis_obj.zlimmode, "auto");

    kids = axis_obj.children;

    nd = 0;
    data_idx = 0;
    data = cell ();

    xminp = yminp = zminp = Inf;
    xmax = ymax = zmax = -Inf;
    xmin = ymin = zmin = Inf;

    palette_set = 0;

    for i = 1:length (kids)

      obj = get (kids(i));

      switch (obj.type)
	case "image"
	  % FIXME - Is there a better way to determine if the plot command should
	  % be "plot" or "splot"?????  Could have images projected into 3D so there
	  % is really no reason to limit this.
	  if (nd == 0)
	    nd = 2;
	  end
	  data_idx++;

	  img_data = obj.cdata;
	  img_colormap = parent_figure_obj.colormap;
	  img_xdata = obj.xdata;
	  img_ydata = obj.ydata;

	  [view_cmd, view_fcn, view_zoom] = image_viewer ();
	  if (ischar (view_fcn) && strcmp (view_fcn, "gnuplot_internal"))

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

	    ## Let the file be deleted when Octave exits or `purge_tmp_files'
	    ## is called.
	    [fid, fname] = mkstemp (strcat (P_tmpdir, "/gpimageXXXXXX"), 1);
	    if (ndims (img_data) == 3)
	      fwrite (fid, permute (img_data, [3, 1, 2])(:), "float");
	      format = "1:2:3";
	      imagetype = "rgbimage";
	    else
	      fwrite (fid, img_data(:), "float");
	      format = "1";
	      imagetype = "image";
	      % Only need to set pallete once because it doesn't change on a figure.
	      if (! palette_set)
		palette_set = 1;
		palette_size = rows (img_colormap);
		fprintf (plot_stream,
			 "set palette positive color model RGB maxcolors %i;\n",
			 palette_size);
		if (palette_size <= 128)
		  ## Break up command to avoid buffer overflow.
		  fprintf (plot_stream, "set palette file \"-\" using 1:2:3:4;\n");
		  for i = 1:palette_size
		    fprintf (plot_stream, "%g %g %g %g;\n",
			     1e-3*round (1e3*[(i-1)/(palette_size-1), img_colormap(i,:)]));
		  end
		  fprintf (plot_stream, "e;\n");
		else
		  # Let the file be deleted when Octave exits or `purge_tmp_files' is called.
		  [fid, binary_fname, msg] = mkstemp (strcat (P_tmpdir, "/gpimageXXXXXX"), 1);
		  fwrite (fid, img_colormap', "float32", 0, "ieee-le");
		  fclose (fid);
		  fprintf (plot_stream,
			   "set palette file \"%s\" binary record=%d using 1:2:3;\n",
			   binary_fname, palette_size);
		endif
	      endif
	    endif
	    fclose (fid);

	    filespec{data_idx} = fname;
	    titlespec{data_idx} = "";
	    usingclause{data_idx} = sprintf ("binary array=%dx%d scan=yx flipy origin=(%g,%g) dx=%g dy=%g using %s",
		x_dim, y_dim, x_origin, y_origin, dx, dy, format);
	    withclause{data_idx} = sprintf ("with %s", imagetype);

	    data{data_idx} = 0; % Data in file, set to zero for data available test to pass below.

	  else
	    view_fcn (xlim, ylim, img_data, view_zoom, view_cmd);
	  endif

	case "line"
	  data_idx++;
	  filespec{data_idx} = '-';
	  if (isempty (obj.keylabel))
	    titlespec{data_idx} = "title \"\"";
	  else
	    titlespec{data_idx} = strcat ("title \"", obj.keylabel, "\"");
	  endif
	  style = do_linestyle_command (obj, data_idx, plot_stream);
	  usingclause{data_idx} = "";
	  withclause{data_idx} = sprintf ("with %s linestyle %d",
					  style, data_idx);
	  parametric(i) = true;
	  if (! isempty (obj.zdata))
	    nd = 3;
	    xdat = obj.xdata(:);
	    ydat = obj.ydata(:);
	    zdat = obj.zdata(:);
	    if (xautoscale)
	      xmin = min (xmin, min (xdat));
	      xmax = max (xmax, max (xdat));
	      xminp = min (xminp, min (xdat(xdat>0)));
	    endif
	    if (yautoscale)
	      ymin = min (ymin, min (ydat));
	      ymax = max (ymax, max (ydat));
	      yminp = min (yminp, min (ydat(ydat>0)));
	    endif
	    if (zautoscale)
	      zmin = min (zmin, min (zdat));
	      zmax = max (zmax, max (zdat));
	      zminp = min (zminp, min (zdat(zdat>0)));
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
		ymin = min (ymin, min (ty));
		ymax = max (ymax, max (ty));
		yminp = min (yminp, min (ty(ty>0)));
	      endif
	      if (xerr)
		xlo = xdat-xldat;
		xhi = xdat+xudat;
		if (xautoscale)
		  tx = [xdat; xlo; xhi];
		  xmin = min (xmin, min (tx));
		  xmax = max (xmax, max (tx));
		  xminp = min (xminp, min (tx(tx>0)));
		endif
		data{data_idx} = [xdat, ydat, xlo, xhi, ylo, yhi]';
		usingclause{data_idx} = "using ($1):($2):($3):($4):($5):($6)";
		withclause{data_idx} = sprintf ("with xyerrorbars linestyle %d",
						data_idx);
	      else
		if (xautoscale)
		  xmin = min (xmin, min (xdat));
		  xmax = max (xmax, max (xdat));
		  xminp = min (xminp, min (tx(tx>0)));
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
		xmin = min (xmin, min (tx));
		xmax = max (xmax, max (tx));
		xminp = min (xminp, min (tx(tx>0)));
	      endif
	      if (yautoscale)
		ymin = min (ymin, min (ydat));
		ymax = max (ymax, max (ydat));
		yminp = min (yminp, min (ty(ty>0)));
	      endif
	      data{data_idx} = [xdat, ydat, xlo, xhi]';
	      usingclause{data_idx} = "using ($1):($2):($3):($4)";
	      withclause{data_idx} = sprintf ("with xerrorbars linestyle %d",
					      data_idx);
	    else
	      if (xautoscale)
		xmin = min (xmin, min (xdat));
		xmax = max (xmax, max (xdat));
		xminp = min (xminp, min (xdat(xdat>0)));
	      endif
	      if (yautoscale)
		ymin = min (ymin, min (ydat));
		ymax = max (ymax, max (ydat));
		yminp = min (yminp, min (ydat(ydat>0)));
	      endif
	      data{data_idx} = [xdat, ydat]';
	      usingclause{data_idx} = "using ($1):($2)";
	    endif
	  endif

	case "surface"
	  data_idx++;
	  style = do_linestyle_command (obj, data_idx, plot_stream);
	  filespec{data_idx} = '-';
	  if (isempty (obj.keylabel))
	    titlespec{data_idx} = "title \"\"";
	  else
	    titlespec{data_idx} = strcat ("title \"", obj.keylabel, "\"");
	  endif
	  usingclause{data_idx} = "";
	  withclause{data_idx} = sprintf ("with %s linestyle %d",
					  style, data_idx);
	  parametric(i) = false;
	  nd = 3;
	  xdat = obj.xdata;
	  ydat = obj.ydata;
	  zdat = obj.zdata;
	  if (xautoscale)
	    tx = xdat(:);
	    xmin = min (xmin, min (tx));
	    xmax = max (xmax, max (tx));
	    xminp = min (xminp, min (tx(tx>0)));
	  endif
	  if (yautoscale)
	    ty = ydat(:);
	    ymin = min (ymin, min (ty));
	    ymax = max (ymax, max (ty));
	    yminp = min (yminp, min (ty(ty>0)));
	  endif
	  if (zautoscale)
	    tz = zdat(:);
	    zmin = min (zmin, min (tz));
	    zmax = max (zmax, max (tz));
	    zminp = min (zminp, min (tz(tz>0)));
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
	    error ("__uiobject_draw_axes__: invalid grid data");
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
	  if (nd == 3)
	    fprintf (plot_stream, "set label \"%s\" at %d,%d,%d %s;\n",
		     undo_string_escapes (label),
		     lpos(1), lpos(2), lpos(3), halign);
	  else
	    fprintf (plot_stream, "set label \"%s\" at %d,%d %s;\n",
		     undo_string_escapes (label),
		     lpos(1), lpos(2), halign);
	  endif

	otherwise
	  error ("__uiobject_draw_axes__: unknown object class, %s",
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
    if (strcmp (axis_obj.xdir, "reverse"))
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
    if (strcmp (axis_obj.ydir, "reverse"))
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
      if (strcmp (axis_obj.zdir, "reverse"))
	zdir = "reverse";
      else
	zdir = "noreverse";
      endif
      fprintf (plot_stream, "set zrange [%g:%g] %s;\n", zlim, zdir);
    endif

    if (strcmp (axis_obj.box, "on"))
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

    if (strcmp (axis_obj.key, "on"))
      if (strcmp (axis_obj.keybox, "on"))
	fputs (plot_stream, "set key box;\n");
      else
	fputs (plot_stream, "set key nobox;\n");
      endif
    else
      fputs (plot_stream, "unset key;\n");
    endif

    fputs (plot_stream, "set style data lines;\n");
    fflush (plot_stream);

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
	fprintf (plot_stream, "set view %g, %g;\n", rot_x, rot_z);
      endif
      fprintf (plot_stream, "%s \"%s\" %s %s %s", plot_cmd,
	       filespec{1}, usingclause{1}, titlespec{1}, withclause{1});
      for i = 2:data_idx
	fprintf (plot_stream, ", \"%s\" %s %s %s",
		 filespec{i}, usingclause{i}, titlespec{i}, withclause{i});
      endfor
      for i = 1:data_idx
	if (strcmp (filespec{i}, "-"))
	  if (nd == 2)
	    fprintf (plot_stream,
		     strcat (repmat ("%g ", 1, rows (data{i})), "\n"),
		     data{i});
	  else
	    if (parametric(i))
	      fprintf (plot_stream, "%g %g %g\n", data{i});
	    else
	      tmp = data{i};
	      nc = columns (tmp);
	      for j = 1:3:nc
		fprintf (plot_stream, "%g %g %g\n", tmp(:,j:j+2)');
		fputs (plot_stream, "\n");
	      endfor
	    endif
	  endif
	  fputs (plot_stream, "e");
	  fflush (plot_stream);
	endif
      endfor
    endif

    fputs (plot_stream, "\n");
    fflush (plot_stream);

  else
    print_usage ();
  endif    

endfunction

function lim = get_axis_limits (min_val, max_val, min_pos, logscale)

  ## FIXME -- this needs to make "nice" limits from the actual max and
  ## min of the data.  For log plots, we will also need the smallest
  ## strictly positive value, which we aren't currently computing and
  ## caching above.

  if (logscale)
    if (isinf (min_pos))
      warning ("axis: logscale with no positive values to plot");
    endif
    if (min_val < 0)
      min_val = min_pos;
      if (max_val < 0)
	max_val = min_pos;
      endif
      warning ("axis: omitting negative data in log plot");
    endif
    if (min_val == max_val)
      min_val = 0.9 * min_val;
      max_val = 1.1 * max_val;
    endif
    min_val = 10 ^ floor (log10 (min_val));
    max_val = 10 ^ ceil (log10 (max_val));
  else
    if (min_val == 0 && max_val == 0)
      min_val = -1;
      max_val = 1;
    elseif (min_val == max_val)
      min_val = 0.9 * min_val;
      max_val = 1.1 * max_val;
    endif
    ## FIXME -- to do a better job, we should consider the tic spacing.
    scale = 10 ^ floor (log10 (max_val - min_val) - 1);
    min_val = scale * floor (min_val / scale);
    max_val = scale * ceil (max_val / scale);
  endif

  lim = [min_val, max_val];

endfunction

function style = do_linestyle_command (obj, idx, plot_stream)

  have_newer_gnuplot = compare_versions (__gnuplot_version__ (), "4.0", ">");

  if (have_newer_gnuplot)
    fprintf (plot_stream, "set style line %d default;\n", idx);
  endif
  fprintf (plot_stream, "set style line %d", idx);

  found_style = false;

  if (isfield (obj, "color") && have_newer_gnuplot)
    color = obj.color;
    if (isnumeric (color))
      fprintf (plot_stream, " linecolor rgb \"#%02x%02x%02x\"",
	       round (255*color));
    endif
    found_style = true;
  endif

  if (isfield (obj, "linestyle"))
    switch (obj.linestyle)
      case "-"
	lt = "lines";
      case "--"
	lt = "";
      case ":"
	lt = "";
      case "-."
	lt = "";
      case "none"
	lt = "";
      otherwise
	lt = "";
    endswitch
  else
    lt = "";
  endif

  if (isfield (obj, "linewidth"))
    fprintf (plot_stream, " linewidth %f", obj.linewidth);
    found_style = true;
  endif

  if (isfield (obj, "marker"))
    switch (obj.marker)
      case "+"
	pt = "1";
      case "o"
	pt = "7";
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
	pt = "6";
      case "none"
	pt = "";
      otherwise
	pt = "";
    endswitch
    if (! isempty (pt))
      fprintf (plot_stream, " pointtype %s", pt);
      found_style = true;
    endif
  else
    pt = "";
  endif

  if (isfield (obj, "markersize"))
    fprintf (plot_stream, " pointsize %f", obj.markersize);
    found_style = true;
  endif

  style = "lines";
  if (isempty (lt))
    if (! isempty (pt))
      style = "points";
    endif
  elseif (! isempty (pt))
    style = "linespoints";
  endif

  if (have_newer_gnuplot && ! found_style)
    fputs (plot_stream, " default");
  endif

  fputs (plot_stream, ";\n");

endfunction
