# Copyright (C) 1996 John W. Eaton
# 
# This file is part of Octave.
# 
# Octave is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 2, or (at your option) any
# later version.
# 
# Octave is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
# 
# You should have received a copy of the GNU General Public License
# along with Octave; see the file COPYING.  If not, write to the Free
# Software Foundation, 59 Temple Place - Suite 330, Boston, MA
# 02111-1307, USA. 

function multiplot (xn, yn)

# usage: multiplot (xn, yn)
#
# Sets and resets multiplot mode
#
# If multiplot(0,0) then it will close multiplot mode and and if
# arguments are non-zero, then it will set up multiplot mode with
# xn,yn subplots along x and y axes. 
#
# NOTE: this will work only with gnuplot installed with
#       multiplot patch

# Written by Vinayak Dutt, Dutt.Vinayak@mayo.EDU  3 Jul 95 

# global variables to keep track of multiplot options

  global multiplot_mode
  global multiplot_xsize multiplot_ysize
  global multiplot_xn multiplot_yn
  global multiplot_xi multiplot_yi

# This is a real kludge.  We gnuplot should be made so that replot can
# be executed while doing multiple plots...

  global multiplot_save_auto_replot = automatic_replot

  if (nargin != 2)
    usage ("multiplot (xn, yn)");
  endif

  if (! (is_scalar (xn) && is_scalar (yn)))
    error ("multiplot: xn and yn have to be scalars");
  endif

  if ((isstr (automatic_replot) && strcmp (automatic_replot,"true"))
       || automatic_replot)
    warning ("turning off automatic replot for multiplot mode");
    multiplot_save_auto_replot = automatic_replot;
    automatic_replot = 0;
  endif

  xn = round (xn);
  yn = round (yn);

  if (xn == 0 && yn == 0)

    set nomultiplot;
    set size 1, 1;
    set origin 0, 0;

    multiplot_mode = 0;
    multiplot_xsize = 1;
    multiplot_ysize = 1;
    multiplot_xn = 1;
    multiplot_yn = 1;
    multiplot_xi = 1;
    multiplot_yi = 1;

# Someone may have reset it betweeen calls...

    if (! isstr (automatic_replot) && ! automatic_replot)
      automatic_replot = multiplot_save_auto_replot;
    endif

    return;

  else

    if (xn < 1 || yn < 1)
      error ("multiplot: xn and yn have to be positive integers");
    endif

    set multiplot;

    xsize = 1.0 ./ xn;
    ysize = 1.0 ./ yn;

    eval (sprintf ("set size %g, %g", xsize, ysize));

    xo = 0.0;
    yo = (yn - 1.0)*ysize;

    eval (sprintf ("set origin %g, %g", xo, yo));

    multiplot_mode = 1;
    multiplot_xsize = xsize;
    multiplot_ysize = ysize;
    multiplot_xn = xn;
    multiplot_yn = yn;
    multiplot_xi = 1;
    multiplot_yi = 1;

  endif

endfunction
