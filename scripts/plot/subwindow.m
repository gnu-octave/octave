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
## @deftypefn {Function File} {} subwindow (@var{xn}, @var{yn})
## Sets the subwindow position in multiplot mode for the next plot.  The
## multiplot mode has to be previously initialized using the
## @code{multiplot} function, otherwise this command just becomes an alias
## to @code{multiplot}
## @end deftypefn

## Author: Vinayak Dutt <Dutt.Vinayak@mayo.EDU>
## Created: 3 July 95
## Adapted-By: jwe

function subwindow (xn, yn)

  ## global variables to keep track of multiplot options

  global __multiplot_mode__ = 0;
  global __multiplot_xsize__;
  global __multiplot_ysize__;
  global __multiplot_xn__;
  global __multiplot_yn__;

  ## check calling argument count

  if (nargin != 2)
    usage ("subwindow (xn, yn)");
  endif

  ## check for scalar inputs

  if (! (isscalar (xn) && isscalar (yn)))
    error ("subwindow: xn and yn have to be scalars");
  endif

  xn = round (xn);
  yn = round (yn);

  ## switch to multiplot mode if not already in, and use the args as the
  ## args to multiplot()

  if (! __multiplot_mode__)
    multiplot (xn, yn);
    return;
  endif

  ## get the sub plot location

  if (xn < 1 || xn > __multiplot_xn__ || yn < 1 || yn > __multiplot_yn__)
    error ("subwindow: incorrect xn and yn");
  endif

  xo = (xn - 1.0) * __multiplot_xsize__;
  yo = (__multiplot_yn__ - yn) * __multiplot_ysize__;

  __gnuplot_raw__ (sprintf ("set origin %g, %g;\n", xo, yo));

  clearplot;

endfunction
