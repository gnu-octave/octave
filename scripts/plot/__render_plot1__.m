## Copyright (C) 2006 John W. Eaton
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

function __render_plot1__ (mxi, myi)

  __plot_globals__;

  cf = __current_figure__;

  if (! isempty (__plot_data__{cf}{mxi,myi}))

    have_image = false;

    for j = 1:length (__plot_data__{cf}{mxi,myi})
      if (__plot_data_type__{cf}{mxi,myi}(j) == 1)
	have_image = true;
      endif
      ## Do this check before sending any commands to gnuplot so that if
      ## there is an error, we don't leave things in a bad state.
      if (j == 1)
	first_plot_data_type = __plot_data_type__{cf}{mxi,myi}(j);
	this_plot_data_type = first_plot_data_type;
      else
	this_plot_data_type = __plot_data_type__{cf}{mxi,myi}(j);
	if ((first_plot_data_type != 3 && this_plot_data_type == 3)
	    || (first_plot_data_type == 3 && this_plot_data_type != 3))
	  error ("can't mix 2-d and 3-d data in the same plot");
	endif
      endif
    endfor

    if (have_image)
      __gnuplot_raw__ ("set size ratio -1;\n");

      cmap = __plot_image_colormap__{cf}{mxi,myi};

      palette_size = rows (cmap);
      __gnuplot_raw__ (sprintf ("set palette positive color model RGB maxcolors %i\n",
				palette_size));

      __gnuplot_raw__ ("set palette file \"-\"\n");
      tmp = round (1000 * cmap) / 1000;
      tmp_rows = rows (tmp);
      idx = (0:tmp_rows-1)';
      __gnuplot_raw__ (sprintf ("%d %.4g %.4g %.4g\n", [idx, tmp]'));
      __gnuplot_raw__("e\n");

      __gnuplot_raw__ ("set yrange [:] reverse;\n");
      __gnuplot_raw__ ("set autoscale fix\n"); # "fix" is helpful for "a" hotkey
      __gnuplot_raw__ ("set tics out\n");
    endif

    first = true;

    for j = 1:length (__plot_data__{cf}{mxi,myi})

      this_plot_data_type = __plot_data_type__{cf}{mxi,myi}(j);

      for i = 1:length (__plot_data__{cf}{mxi,myi}{j})

	if (first)
	  first = false;
	  __do_legend__ ();
	  if (first_plot_data_type == 3)
	    cmd = gnuplot_command_splot;
	  else
	    cmd = gnuplot_command_plot;
	  endif
	  __gnuplot_raw__ (sprintf ("%s ", cmd));
	else
	  __gnuplot_raw__ (",\\\n");
	endif

	if (this_plot_data_type == 1)
	  label = undo_string_escapes (__plot_key_labels__{cf}{mxi,myi}{j}{i});

	  tmp = __plot_image_dims__{cf}{mxi,myi}{j}{i};

	  x_dim = tmp(1);
	  y_dim = tmp(2);
	  x_origin = tmp(3);
	  y_origin = tmp(4);
	  dx = tmp(5);
	  dy = tmp(6);

	  A = __plot_data__{cf}{mxi,myi}{j}{i};

	  ## Let the file be deleted when Octave exits or
	  ## `purge_tmp_files' is called.
	  [fid, binary_file_name, msg] ...
	      = mkstemp (strcat (P_tmpdir, "/gpimageXXXXXX"), 1);

	  ## Gnuplot reads binary files very quickly.  However, the
	  ## 'fwrite' below is much slower than using the current
	  ## '__gnuplot_plot__' command.
	  fwrite (fid, A(:), "float");
	  fclose (fid);

	  __gnuplot_raw__ (sprintf ("\"%s\" binary array=%dx%d scan=yx flipy origin=(%g,%g) dx=%g dy=%g using 1 %s '%s' with image",
				    binary_file_name, x_dim, y_dim, x_origin,
				    y_origin, dx, dy,
				    gnuplot_command_title, label));
	else
	  usingstr = __plot_usingstr__{cf}{mxi,myi}{j}{i};
	  fmtstr = __plot_fmtstr__{cf}{mxi,myi}{j}{i};
	  withstr = __plot_withstr__{cf}{mxi,myi}{j}{i};

	  label = undo_string_escapes (__plot_key_labels__{cf}{mxi,myi}{j}{i});

	  __gnuplot_raw__ (sprintf ("'-' %s %s '%s' %s %s %s", usingstr,
				    gnuplot_command_title, label, fmtstr,
				    withstr));
	endif
      endfor
    endfor

    for j = 1:length (__plot_data__{cf}{mxi,myi})
      for i = 1:length (__plot_data__{cf}{mxi,myi}{j})
	this_plot_data_type = __plot_data_type__{cf}{mxi,myi}(j);
	if (this_plot_data_type != 1)
	  if (this_plot_data_type == 3)
	    parametric = __plot_data_parametric__{cf}{mxi,myi}{j}{i};
	  else
	    parametric = false;
	  endif
	  __gnuplot_send_inline_data__ (__plot_data__{cf}{mxi,myi}{j}{i},
					this_plot_data_type, parametric);
	endif
      endfor
    endfor

    if (! first)
      __gnuplot_raw__ ("\n");
    endif

  endif

endfunction
