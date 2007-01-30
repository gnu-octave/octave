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
## For example, a plot with 2 by 3 grid will have plot indices running as
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
## @example
##
## +-----+-----+-----+-----+
## |  1  |  2  |  3  |  4  |
## +-----+-----+-----+-----+
## |  5  |  6  |  7  |  8  |
## +-----+-----+-----+-----+
## @end example
## @end group
## @end display
## @end ifinfo
## @seealso{plot}
## @end deftypefn

## Author: Vinayak Dutt <Dutt.Vinayak@mayo.EDU>
## Adapted-By: jwe

function h = subplot (rows, columns, index)

  if (nargin != 3 && nargin != 1)
    print_usage ();
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

  xsize = 1 / columns;
  ysize = 1 / rows;

  yp = fix ((index-1)/columns);
  xp = index - yp*columns - 1;

  xorigin = xp * xsize;
  yorigin = (rows - yp - 1) * ysize;

  pos = [xorigin, yorigin, xsize, ysize];

  cf = gcf ();

  set (cf, "nextplot", "add");

  obj = get (cf);

  found = false;
  for child = obj.children
    obj = get (child);
    if (strcmp (obj.type, "axes"))
      if (obj.outerposition == pos)
	found = true;
	tmp = child;
	axes (h);
	break;
      endif
    endif
  endfor

  if (! found)
    tmp = axes ("outerposition", pos);
  endif

  if (nargout > 0)
    h = tmp;
  endif

endfunction
