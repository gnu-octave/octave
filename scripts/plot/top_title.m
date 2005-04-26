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
## @deftypefn {Function File} {} top_title (@var{string})
## @deftypefnx {Function File} {} bottom_title (@var{string})
## Makes a title with text @var{string} at the top (bottom) of the plot.
## @end deftypefn

## Author: Vinayak Dutt <Dutt.Vinayak@mayo.EDU>
## Created: 3 July 95
## Adapted-By: jwe

function top_title (text)

  if (nargin != 1)
    usage ("top_title (text)");
  endif

  if (isstr (text))
    __gnuplot_raw__ ("set bottom_title;\n")
    __gnuplot_raw__ ("set title;\n")
    __gnuplot_raw__ (sprintf ("set top_title \"%s\";\n",
			      undo_string_escapes (text)));
    if (automatic_replot)
      ## No semicolon (see replot.m).
      __gnuplot_replot__
    endif
  else
    error ("error: top_title: text must be a string");
  endif

endfunction
