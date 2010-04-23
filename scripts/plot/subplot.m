## Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2002, 2003, 2005,
##               2006, 2007, 2008, 2009 John W. Eaton
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
## @tex
## \vskip 10pt
## \hfil\vbox{\offinterlineskip\hrule
## \halign{\vrule#&&\qquad\hfil#\hfil\qquad\vrule\cr
## height13pt&1&2&3\cr height12pt&&&\cr\noalign{\hrule}
## height13pt&4&5&6\cr height12pt&&&\cr\noalign{\hrule}}}
## \hfil
## \vskip 10pt
## @end tex
## @ifnottex
## @display
## @example
## @group
##
## +-----+-----+-----+
## |  1  |  2  |  3  |
## +-----+-----+-----+
## |  4  |  5  |  6  |
## +-----+-----+-----+
## @end group
## @end example
## @end display
## @end ifnottex
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

  elseif (! (isscalar (columns) && isscalar (rows)))
    error ("subplot: columns, and rows must be scalars");
  elseif (any (index < 1) || any (index > rows*columns))
    error ("subplot: index value must be greater than 1 and less than rows*columns")
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

  units = get (0, "defaultaxesunits");
  unwind_protect
    set (0, "defaultaxesunits", "normalized")
    pos = subplot_position (rows, columns, index, "position", units);

    cf = gcf ();

    set (cf, "nextplot", "add");

    found = false;
    kids = get (cf, "children");
    for child = reshape (kids, 1, numel (kids))
      ## Check whether this child is still valid; this might not be the
      ## case anymore due to the deletion of previous children (due to
      ## "deletefcn" callback or for legends/colorbars that are deleted
      ## with their corresponding axes).
      if (! ishandle (child))
        continue;
      endif
      if (strcmp (get (child, "type"), "axes"))
        ## Skip legend and colorbar objects.
        if (strcmp (get (child, "tag"), "legend") || 
            strcmp (get (child, "tag"), "colorbar"))
          continue;
        endif
        objpos = get (child, "position");
        if (all (objpos == pos))
          ## If the new axes are in exactly the same position as an
          ## existing axes object, use the existing axes.
          found = true;
          tmp = child;
        else
          ## If the new axes overlap an old axes object, delete the old
          ## axes.
          x0 = pos(1);
          x1 = x0 + pos(3);
          y0 = pos(2);
          y1 = y0 + pos(4);     
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
      pos = subplot_position (rows, columns, index, "outerposition", units);
      pos2 = subplot_position (rows, columns, index, "position", units);
      tmp = axes ("outerposition", pos, "position", pos2);
    endif

  unwind_protect_cleanup
    set (0, "defaultaxesunits", units);
  end_unwind_protect

  if (nargout > 0)
    h = tmp;
  endif

endfunction

function pos = subplot_position (rows, columns, index, position_property, units)

  ## For 1 row and 1 column return the usual default.
  if (rows == 1 && columns == 1)
    if (strcmpi (position_property, "position"))
      pos = get (0, "defaultaxesposition");
    else
      pos = get (0, "defaultaxesouterposition");
    endif
    return
  endif

  ## This produces compatible behavior for the "position" property.
  margins.left   = 0.130;
  margins.right  = 0.095;
  margins.top    = 0.075;
  margins.bottom = 0.110;
  pc = 1 ./ [0.1860, (margins.left + margins.right - 1)];
  margins.column = 1 ./ polyval (pc , columns);
  pr = 1 ./ [0.2282, (margins.top + margins.bottom - 1)];
  margins.row    = 1 ./ polyval (pr , rows);

  ## Calculate the width/height of the subplot axes.
  width = 1 - margins.left - margins.right - (columns-1)*margins.column;
  width = width / columns;
  height = 1 - margins.top - margins.bottom - (rows-1)*margins.row;
  height = height / rows;

  if (strcmp (position_property, "outerposition") )
    ## Calculate the outerposition/position inset
    if (rows > 1)
      inset.top    = 8/420;
      inset.bottom = max (polyval ([0.1382,-0.0026], height), 16/420);
    else
      inset.bottom = margins.bottom;
      inset.top = margins.top;
    endif
    if (columns > 1)
      if (strcmpi (units, "normalized"))
        inset.right = max (polyval ([0.1200,-0.0014], width), 5/560);
      else
        inset.right = max (polyval ([0.1252,-0.0023], width), 5/560);
      endif
      inset.left   = 22/560;
    else
      inset.left  = margins.left;
      inset.right = margins.right;
    endif
    ## Apply the inset to the geometries for the "position" property.
    margins.column = margins.column - inset.right - inset.left;
    margins.row = margins.row - inset.top - inset.bottom;
    width = width + inset.right + inset.left;
    height = height + inset.top + inset.bottom;
  endif

  yp = fix ((index(:)-1)/columns);
  xp = index(:) - yp*columns - 1;
  yp = (rows - 1) - yp;

  x0 = xp .* (width + margins.column) + margins.left;
  y0 = yp .* (height + margins.row) + margins.bottom;

  if (strcmp (position_property, "outerposition") )
    x0 = x0 - inset.left;
    y0 = y0 - inset.bottom;
  endif

  if (numel(x0) > 1)
    x1 = max (x0) + width;
    y1 = max (y0) + height;
    x0 = min (x0);
    y0 = min (y0);
    pos = [x0, y0, x1-x0, y1-y0];
  else
    pos = [x0, y0, width, height];
  endif

endfunction

%!demo
%! clf
%! r = 3;
%! c = 3;
%! fmt = {'horizontalalignment', 'center', 'verticalalignment', 'middle'};
%! for n = 1:(r*c)
%!   subplot (r, c, n)
%!   xlabel (sprintf ("xlabel #%d", n))
%!   ylabel (sprintf ("ylabel #%d", n))
%!   title (sprintf ("title #%d", n))
%!   text (0.5, 0.5, sprintf('subplot(%d,%d,%d)', r, c, n), fmt{:})
%!   axis ([0 1 0 1])
%! endfor
%! subplot (r, c, 1:3)
%! xlabel (sprintf ("xlabel #%d:%d", 1, 3))
%! ylabel (sprintf ("ylabel #%d:%d", 1, 3))
%! title (sprintf ("title #%d:%d", 1, 3))
%! text (0.5, 0.5, sprintf('subplot(%d,%d,%d:%d)', r, c, 1, 3), fmt{:})
%! axis ([0 1 0 1])

