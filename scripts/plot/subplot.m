## Copyright (C) 1996 John W. Eaton
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

## usage: subplot (rows, columns, index)
##        subplot (rcn)
##
## NOTE: this will work only with gnuplot installed with
##       multiplot patch (or version 3.6 beta)
##
## Sets gnuplot in multiplot mode and plots in location
## given by index (there are columns X rows subwindows)
##
## Input:
##
##   rows   : number of rows in subplot grid
##   columns: number of columns in subplot grid
##   index  : index of subplot where to make the next plot
##
## If only one arg, then it (crn) has to be three digit value
## specifying the location in digit 1 (rows) and 2 (columns) and digit
## 3 is the plot index
##
## The plot index runs row-wise,i.e., first all the columns in a row
## are filled and then the next row is filled
##
## For example, plot with 4 X 2 grid, will have plot indices running as
## follows:
##
##   -----------------------------------
##   |        |       |       |        |
##   |    1   |    2  |    3  |    4   |
##   |        |       |       |        |
##   -----------------------------------
##   |        |       |       |        |
##   |    5   |    6  |    7  |    8   |
##   |        |       |       |        |
##   -----------------------------------
##

## Author: Vinayak Dutt <Dutt.Vinayak@mayo.EDU>
## Adapted-By: jwe

function subplot (rows, columns, index)

  if (! gnuplot_has_multiplot)
    error ("subplot: gnuplot does not appear to support this feature");
  endif

  ## global variables to keep track of multiplot options

  global multiplot_mode
  global multiplot_xsize multiplot_ysize
  global multiplot_xn multiplot_yn
  global multiplot_xi multiplot_yi

  ## This is a real kludge.  We gnuplot should be made so that replot can
  ## be executed while doing multiple plots...

  global multiplot_save_auto_replot = automatic_replot

  if (nargin != 3 && nargin != 1)
    usage ("subplot (rows, columns, index) or subplot (rcn)");
  endif

  if (automatic_replot)
    warning ("turning off automatic replot for multiplot mode");
    multiplot_save_auto_replot = automatic_replot;
    automatic_replot = 0;
  endif

  if (nargin == 1)

    if (! (is_scalar (rows) && rows >= 0))
      error ("subplot: input rcn has to be a positive scalar");
    endif

    tmp = rows;
    index = rem (tmp, 10);
    tmp = (tmp - index) / 10;
    columns = rem (tmp, 10);
    tmp = (tmp - columns) / 10;
    rows = rem (tmp, 10);

  elseif (! (is_scalar (columns) && is_scalar (rows) && is_scalar (index)))
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

    gset nomultiplot;
    gset size 1, 1;
    gset origin 0, 0;

    multiplot_xn = 1;
    multiplot_yn = 1;
    multiplot_mode = 0;

    ## Someone may have reset it betweeen calls...

    if (! isstr (automatic_replot) && ! automatic_replot)
      automatic_replot = multiplot_save_auto_replot;
    endif

  else

    ## doing multiplot plots

    doagain = 0;

    if (exist ("multiplot_mode") != 1)
      doagain = 1;
    elseif (multiplot_mode != 1 || multiplot_xn != columns
	    || multiplot_yn != rows)
      doagain = 1;
    endif

    if (doagain)

      multiplot_mode = 1;
      multiplot_xn = columns;
      multiplot_yn = rows;
      multiplot_xsize = 1.0 ./ columns;
      multiplot_ysize = 1.0 ./ rows;

      gset multiplot;

      eval (sprintf ("gset size %g, %g", multiplot_xsize, multiplot_ysize));

    endif

    ## get the sub plot location

    yp = fix ((index-1)/columns);
    xp = index - yp*columns - 1;
    multiplot_xi = ++xp;
    multiplot_yi = ++yp;

    ## set the origin

    xo = (xp - 1.0)*multiplot_xsize;
    yo = (rows - yp)*multiplot_ysize;

    eval (sprintf ("gset origin %g, %g", xo, yo));

  endif

endfunction
