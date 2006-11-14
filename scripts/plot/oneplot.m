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
## @deftypefn {Function File} {} oneplot ()
## If in multiplot mode, switches to single plot mode.
## @end deftypefn

## Author: Vinayak Dutt <Dutt.Vinayak@mayo.EDU>
## Created: 3 July 95
## Adapted-By: jwe

function oneplot ()

  plot_globals;

  if (__multiplot_mode__(__current_figure__))
    __gnuplot_raw__ ("set nomultiplot;\n");
    __gnuplot_raw__ ("set size 1, 1;\n");
    __gnuplot_raw__ ("set origin 0, 0;\n");
    __multiplot_mode__(__current_figure__) = false;
    gnuplot_command_replot = "rep";
  endif

endfunction
