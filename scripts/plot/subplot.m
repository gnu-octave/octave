## Copyright (C) 1996, 1997 John W. Eaton
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn {Function File} {} subplot (@var{rows}, @var{cols}, @var{index})
## @deftypefnx {Function File} {} subplot (@var{rcn})
## Set up a plot grid with @var{cols} by @var{rows} subwindows and plot
## in location given by @var{index}.
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

  x0 = xp * xsize;
  y0 = (rows - yp - 1) * ysize;

  pos = [x0, y0, xsize, ysize];

  x1 = x0 + xsize;
  y1 = y0 + ysize;

  cf = gcf ();

  set (cf, "nextplot", "add");

  found = false;
  for child = get (cf, "children")
    ## Check if this child is still valid; this might not be the case
    ## anymore due to the deletion of previous children (due to DeleteFcn
    ## callback or for legends/colorbars that get deleted with their
    ## corresponding axes)
    if (! ishandle (child))
      continue;
    endif
    if (strcmp (get (child, "type"), "axes"))
      objpos = get (child, "outerposition");
      if (objpos == pos)
	## If the new axes are in exactly the same position as an
	## existing axes object, use the existing axes.
	found = true;
	tmp = child;
	break;
      else
	## If the new axes overlap an old axes object, delete the old
	## axes.
	objx0 = objpos(1);
	objx1 = objx0 + objpos(3);
	objy0 = objpos(2);
	objy1 = objy0 + objpos(4);
	if (! (x0 >= objx1 || x1 <= objx0 || y0 >= objy1 || y1 <= objy0))
	  delete (child);
	endif
      endif
    endif
  endfor

  if (found)
    set (cf, "currentaxes", tmp);
  else
    tmp = axes ("outerposition", pos);
  endif

  if (nargout > 0)
    h = tmp;
  endif

endfunction
