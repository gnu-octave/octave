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
## @deftypefn {Function File} {} subplot (@var{rows}, @var{cols}, @var{index})
## @deftypefnx {Function File} {} subplot (@var{rcn})
## Sets @code{gnuplot} in multiplot mode and plots in location
## given by index (there are @var{cols} by @var{rows} subwindows).
##
## The global variable @var{__multiplot_scale__} should be used when the
## command @code{__gnuplot_set__ size xsize, ysize} has been used prior to calling
## @code{subplot}.
##
## The value of @var{__multiplot_scale__} should be a vector with two
## elements, the first set equal to @var{xsize} and the second to
## @var{ysize}.
##
## Input:
##
## @table @var
## @item rows
## Number of rows in subplot grid.
##
## @item columns
## Number of columns in subplot grid.
##
## @item index
## Index of subplot where to make the next plot.
## @end table
##
## If only one argument is supplied, then it must be a three digit value
## specifying the location in digits 1 (rows) and 2 (columns) and the plot
## index in digit 3.
##
## The plot index runs row-wise.  First all the columns in a row are filled
## and then the next row is filled.
##
## For example, a plot with 4 by 2 grid will have plot indices running as
## follows:
## @iftex
## @tex
## \vskip 10pt
## \hfil\vbox{\offinterlineskip\hrule
## \halign{\vrule#&&\qquad\hfil#\hfil\qquad\vrule\cr
## height13pt&1&2&3&4\cr height12pt&&&&\cr\noalign{\hrule}
## height13pt&5&6&7&8\cr height12pt&&&&\cr\noalign{\hrule}}}
## \hfil
## \vskip 10pt
## @end tex
## @end iftex
## @ifinfo
## @display
## @group
## +-----+-----+-----+-----+
## |  1  |  2  |  3  |  4  |
## +-----+-----+-----+-----+
## |  5  |  6  |  7  |  8  |
## +-----+-----+-----+-----+
## @end group
## @end display
## @end ifinfo
## @end deftypefn

## Author: Vinayak Dutt <Dutt.Vinayak@mayo.EDU>
## Adapted-By: jwe

function subplot (rows, columns, index)

  ## global variables to keep track of multiplot options

  global __multiplot_mode__ = 0;
  global __multiplot_xsize__;
  global __multiplot_ysize__;
  global __multiplot_xn__;
  global __multiplot_yn__;
  global __multiplot_xi__;
  global __multiplot_yi__;
  global __multiplot_scale__;

  if (isempty (__multiplot_scale__))
    __multiplot_scale__ = [1, 1];
  endif

  if (nargin != 3 && nargin != 1)
    usage ("subplot (rows, columns, index) or subplot (rcn)");
  endif

  if (nargin == 1)

    if (! (isscalar (rows) && rows >= 0))
      error ("subplot: input rcn has to be a positive scalar");
    endif

    tmp = rows;
    index = rem (tmp, 10);
    tmp = (tmp - index) / 10;
    columns = rem (tmp, 10);
    tmp = (tmp - columns) / 10;
    rows = rem (tmp, 10);

  elseif (! (isscalar (columns) && isscalar (rows) && isscalar (index)))
    error ("subplot: columns, rows, and index have to be scalars");
  endif

  columns = round (columns);
  rows = round (rows);
  index = round (index);

  if (index > columns*rows)
    error ("subplot: index must be less than columns*rows");
  endif

  if (columns < 1 || rows < 1 || index < 1)
    error ("subplot: columns,rows,index must be be positive");
  endif

  if (columns*rows == 1)

    ## switching to single plot ?

    oneplot ();

    ## XXX FIXME XXX -- do we really need to reset these here?

    __multiplot_xn__ = 1;
    __multiplot_yn__ = 1;

  else

    ## doing multiplot plots

    if (! __multiplot_mode__
        || __multiplot_xn__ != columns
        || __multiplot_yn__ != rows)

      __multiplot_mode__ = 1;
      __multiplot_xn__ = columns;
      __multiplot_yn__ = rows;
      __multiplot_xsize__ = __multiplot_scale__(1) ./ columns;
      __multiplot_ysize__ = __multiplot_scale__(2) ./ rows;

      gnuplot_command_replot = "cle;rep";

      __gnuplot_raw__ ("set multiplot;\n");

      __gnuplot_raw__ (sprintf ("set size %g, %g;\n",
				__multiplot_xsize__, __multiplot_ysize__));
    endif

    ## get the sub plot location

    yp = fix ((index-1)/columns);
    xp = index - yp*columns - 1;
    __multiplot_xi__ = ++xp;
    __multiplot_yi__ = ++yp;

    ## set the origin

    xo = (xp - 1.0) * __multiplot_xsize__;
    yo = (rows - yp) * __multiplot_ysize__;

    __gnuplot_raw__ (sprintf ("set origin %g, %g;\n", xo, yo));

  endif

endfunction
