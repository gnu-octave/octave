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
## Software Foundation, 59 Temple Place - Suite 330, Boston, MA
## 02111-1307, USA.

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

  ## global variables to keep track of multiplot options

  global __multiplot_mode__ = 0;
  global __multiplot_xsize__;
  global __multiplot_ysize__;
  global __multiplot_xn__;
  global __multiplot_yn__;
  global __multiplot_xi__;
  global __multiplot_yi__;

  if (nargin != 2)
    usage ("multiplot (xn, yn)");
  endif

  if (! (isscalar (xn) && isscalar (yn)))
    error ("multiplot: xn and yn have to be scalars");
  endif

  xn = round (xn);
  yn = round (yn);

  if (xn == 0 && yn == 0)

    oneplot ();

    ## XXX FIXME XXX -- do we really need to reset these here?

    __multiplot_xsize__ = 1;
    __multiplot_ysize__ = 1;
    __multiplot_xn__ = 1;
    __multiplot_yn__ = 1;
    __multiplot_xi__ = 1;
    __multiplot_yi__ = 1;

  else

    if (xn < 1 || yn < 1)
      error ("multiplot: xn and yn have to be positive integers");
    endif

    __gset__ multiplot;

    xsize = 1.0 ./ xn;
    ysize = 1.0 ./ yn;

    eval (sprintf ("__gset__ size %g, %g", xsize, ysize));

    xo = 0.0;
    yo = (yn - 1.0)*ysize;

    eval (sprintf ("__gset__ origin %g, %g", xo, yo));

    __multiplot_mode__ = 1;
    __multiplot_xsize__ = xsize;
    __multiplot_ysize__ = ysize;
    __multiplot_xn__ = xn;
    __multiplot_yn__ = yn;
    __multiplot_xi__ = 1;
    __multiplot_yi__ = 1;

    gnuplot_command_replot = "cle;rep";

    clearplot;

  endif

endfunction
