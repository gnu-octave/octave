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
## @deftypefn {Function File} {} mplot (@var{x}, @var{y})
## @deftypefnx {Function File} {} mplot (@var{x}, @var{y}, @var{fmt})
## @deftypefnx {Function File} {} mplot (@var{x1}, @var{y1}, @var{x2}, @var{y2})
## This is a modified version of the @code{plot} function that works with
## the multiplot version of @code{gnuplot} to plot multiple plots per page.
## This plot version automatically advances to the next subplot position
## after each set of arguments are processed.
##
## See the description of the @var{plot} function for the various options.
## @end deftypefn

## Author: Vinayak Dutt <Dutt.Vinayak@mayo.EDU>
## Adapted-By: jwe

function mplot (varargin)

  ## global variables to keep track of multiplot options

  global __multiplot_mode__ = 0;
  global __multiplot_xsize__;
  global __multiplot_ysize__;
  global __multiplot_xn__;
  global __multiplot_yn__;
  global __multiplot_xi__;
  global __multiplot_yi__;

  __gnuplot_set__ nologscale;
  __gnuplot_set__ nopolar;

  __plt__ ("plot", varargin{:});

  ## update the plot position

  if (__multiplot_mode__)

    if (__multiplot_xi__ < __multiplot_xn__)
      __multiplot_xi__++;
    else
      __multiplot_xi__ = 1;
      if (__multiplot_yi__ < __multiplot_yn__)
        __multiplot_yi__++;
      else
        __multiplot_yi__ = 1;
      endif
    endif

    xo = (__multiplot_xi__ - 1.0) * __multiplot_xsize__;
    yo = (__multiplot_yn__ - __multiplot_yi__) * __multiplot_ysize__;

    eval (sprintf ("__gnuplot_set__ origin %g, %g", xo, yo));

  endif

endfunction
