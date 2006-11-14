## Copyright (C) 1996, 1997 John W. Eaton
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
## @deftypefn {Function File} {} multiplot (@var{xn}, @var{yn})
## Sets and resets multiplot mode.
##
## If the arguments are non-zero, @code{multiplot} will set up multiplot
## mode with @var{xn}, @var{yn} subplots along the @var{x} and @var{y}
## axes.  If both arguments are zero, @code{multiplot} closes multiplot
## mode.
## @end deftypefn

## Author: Vinayak Dutt, Dutt.Vinayak@mayo.EDU
## Created: 3 July 95
## Adapted-By: jwe

function multiplot (xn, yn)

  plot_globals;

  cf = __current_figure__;

  if (nargin != 2)
    print_usage ();
  endif

  if (! (isscalar (xn) && isscalar (yn)))
    error ("multiplot: xn and yn have to be scalars");
  endif

  xn = round (xn);
  yn = round (yn);

  if (xn == 0 && yn == 0)

    oneplot ();

    __multiplot_xsize__(cf) = 1;
    __multiplot_ysize__(cf) = 1;
    __multiplot_xn__(cf) = 1;
    __multiplot_yn__(cf) = 1;
    __multiplot_xi__(cf) = 1;
    __multiplot_yi__(cf) = 1;

  else

    if (xn < 1 || yn < 1)
      error ("multiplot: xn and yn have to be positive integers");
    endif

    __gnuplot_raw__ ("set multiplot;\n");

    xsize = 1.0 ./ xn;
    ysize = 1.0 ./ yn;

    __gnuplot_raw__ (sprintf ("set size %g, %g;\n", xsize, ysize));

    xo = 0.0;
    yo = (yn - 1.0)*ysize;

    __gnuplot_raw__ (sprintf ("set origin %g, %g;\n", xo, yo));

    __multiplot_mode__(cf) = true;
    __multiplot_xsize__(cf) = xsize;
    __multiplot_ysize__(cf) = ysize;
    __multiplot_xn__(cf) = xn;
    __multiplot_yn__(cf) = yn;
    __multiplot_xi__(cf) = 1;
    __multiplot_yi__(cf) = 1;

    gnuplot_command_replot = "cle;rep";

    clearplot;

  endif

endfunction
