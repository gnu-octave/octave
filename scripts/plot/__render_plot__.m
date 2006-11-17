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

function __render_plot__ (terminal, output)

  __plot_globals__;

  cf = __current_figure__;
  mxn = __multiplot_xn__(cf);
  myn = __multiplot_yn__(cf);

  set_terminal = nargin > 0;
  set_output = nargin > 1;

  unwind_protect

    if (set_terminal)
      __gnuplot_raw__ ("set terminal push;\n");
      __gnuplot_raw__ (sprintf ("set terminal %s;\n", terminal));
    endif

    if (set_output)
      __gnuplot_raw__ (sprintf ("set output \"%s\";\n", output));
    endif

    if (__multiplot_mode__(cf))
      __gnuplot_raw__ ("set size 1, 1;\n");
      __gnuplot_raw__ ("set origin 0, 0;\n");
      __gnuplot_raw__ ("set multiplot;\n");
      for mxi = 1:mxn
	for myi = 1:myn

	  columns = __multiplot_xn__(cf);
	  rows = __multiplot_yn__(cf);
	  __gnuplot_raw__ (sprintf ("set size %g, %g;\n",
				    __multiplot_xsize__(cf),
				    __multiplot_ysize__(cf)));

	  xo = (mxi - 1.0) * __multiplot_xsize__(cf);
	  yo = (rows - myi) * __multiplot_ysize__(cf);

	  __gnuplot_raw__ (sprintf ("set origin %g, %g;\n", xo, yo));

	  __render_plot1__ (mxi, myi)

	endfor
      endfor
      __gnuplot_raw__ ("unset multiplot;\n");
    else
      __render_plot1__ (1, 1);
    endif

  unwind_protect_cleanup

    if (set_terminal)
      __gnuplot_raw__ ("set terminal pop;\n");
    endif

    if (set_output)
      __gnuplot_raw__ ("set output;\n")
    endif

  end_unwind_protect

endfunction
