## Copyright (C) 1995-2012 John W. Eaton
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
## @deftypefn  {Function File} {} subplot (@var{rows}, @var{cols}, @var{index})
## @deftypefnx {Function File} {} subplot (@var{rcn})
## Set up a plot grid with @var{rows} by @var{cols} subwindows and plot
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
##
## @example
## @group
## +-----+-----+-----+
## |  1  |  2  |  3  |
## +-----+-----+-----+
## |  4  |  5  |  6  |
## +-----+-----+-----+
## @end group
## @end example
##
## @var{index} may be a vector.  In which case, the new axis will enclose
## the grid locations specified.  The first demo illustrates an example:
##
## @example
## demo ("subplot", 1)
## @end example
##
## @end ifnottex
## @seealso{axes, plot}
## @end deftypefn

## Author: Vinayak Dutt <Dutt.Vinayak@mayo.EDU>
## Adapted-By: jwe

function h = subplot (varargin)

  align_axes = false;
  replace_axes = false;
  have_position = false;
  initial_args_decoded = false;

  if (nargin > 2)
    ## R, C, N?
    arg1 = varargin{1};
    arg2 = varargin{2};
    arg3 = varargin{3};
    if (isnumeric (arg1) && isscalar (arg1) && isnumeric (arg2)
        && isscalar (arg2) && isnumeric (arg3))
      rows = arg1;
      cols = arg2;
      index = arg3;
      varargin(1:3)= [];
      initial_args_decoded = true;
    endif
  endif

  if (! initial_args_decoded && nargin > 1)
    ## check for 'position', pos, ...
    if (strcmpi (varargin{1}, "position"))
      arg = varargin{2};
      if (isnumeric (arg) && numel (arg) == 4)
        pos = arg;
        varargin(1:2) = [];
        have_position = true;
        initial_args_decoded = true;
      else
        error ("expecting position to be a 4-element numeric array");
      endif
    endif
  endif
    
  if (! initial_args_decoded && nargin > 0)
    arg = varargin{1};
    if (nargin == 1 && ishandle (arg))
      ## Axes handle?
      axes (arg);
      cf = get (0, "currentfigure");
      set (cf, "nextplot", "add");
      return;
    elseif (isscalar (arg) && arg >= 0)
      ## RCN?
      index = rem (arg, 10);
      arg = (arg - index) / 10;
      cols = rem (arg, 10);
      arg = (arg - cols) / 10;
      rows = rem (arg, 10);
      varargin(1) = [];
      initial_args_decoded = true;
    else
      error ("subplot: expecting axes handle or RCN argument");
    endif
  endif

  if (! initial_args_decoded)
    print_usage ();
  endif

  if (! have_position)
    cols = round (cols);
    rows = round (rows);
    index = round (index);

    if (any (index < 1) || any (index > rows*cols))
      error ("subplot: INDEX value must be greater than 1 and less than ROWS*COLS");
    endif

    if (cols < 1 || rows < 1 || index < 1)
      error ("subplot: COLS, ROWS, and INDEX must be be positive");
    endif
  endif

  nargs = numel (varargin);
  while (nargs > 0)
    arg = varargin{1};
    if (strcmpi (arg, "align"))
      align_axes = true;
    elseif (strcmpi (arg, "replace"))
      replace_axes = true;
    else
      break;
    endif
    varargin(1) = [];
    nargs--;
  endwhile

  axesunits = get (0, "defaultaxesunits");
  cf = gcf ();
  figureunits = get (cf, "units");
  unwind_protect
    units = "normalized";
    set (0, "defaultaxesunits", units);
    set (cf, "units", "pixels");

    ## FIXME: At the moment we force gnuplot to use the aligned mode
    ##        which will set "activepositionproperty" to "position".
    ##        Τhis can yield to text overlap between labels and titles
    ##        see bug #31610
    if (strcmp (get (cf, "__graphics_toolkit__"), "gnuplot"))
      align_axes = true;
    endif

    if (! have_position)
      if (align_axes)
        pos = subplot_position (rows, cols, index, "position");
      elseif (strcmp (get (cf, "__graphics_toolkit__"), "gnuplot"))
        pos = subplot_position (rows, cols, index, "outerpositiontight");
      else
        pos = subplot_position (rows, cols, index, "outerposition");
      endif
    endif

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
        if (strcmp (get (child, "tag"), "legend")
            || strcmp (get (child, "tag"), "colorbar"))
          continue;
        endif
        if (align_axes)
          objpos = get (child, "position");
        else
          objpos = get (child, "outerposition");
        endif
        if (all (abs (objpos - pos) < eps) && ! replace_axes)
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
    elseif (align_axes)
      tmp = axes ("box", "off", "position", pos, varargin{:});
    elseif (strcmp (get (cf, "__graphics_toolkit__"), "gnuplot"))
      tmp = axes ("box", "off", "outerposition", pos, varargin{:});
    else
      tmp = axes ("looseinset", [0 0 0 0], "box", "off", "outerposition", pos,
                  "autopos_tag", "subplot", varargin{:});
    endif

  unwind_protect_cleanup
    set (0, "defaultaxesunits", axesunits);
    set (cf, "units", figureunits);
  end_unwind_protect

  if (nargout > 0)
    h = tmp;
  endif

endfunction

function pos = subplot_position (rows, cols, index, position_property)

  if (rows == 1 && cols == 1)
    ## Trivial result for subplot (1,1,1)
    if (strcmpi (position_property, "position"))
      pos = get (0, "defaultaxesposition");
    else
      pos = get (0, "defaultaxesouterposition");
    endif
    return
  endif

  if (strcmp (position_property, "outerposition")
      || strcmp (position_property, "outerpositiontight"))
    margins.left   = 0.05;
    margins.bottom = 0.05;
    margins.right  = 0.05;
    margins.top    = 0.05;
    if (strcmp (position_property, "outerpositiontight"))
      margins.column = 0.;
      margins.row = 0.;
    else
      margins.column = 0.04 / cols;
      margins.row = 0.04 / rows;
    endif
    width = 1 - margins.left - margins.right - (cols-1)*margins.column;
    width = width / cols;
    height = 1 - margins.top - margins.bottom - (rows-1)*margins.row;
    height = height / rows;
  else
    defaultaxesposition = get (0, "defaultaxesposition");

    ## The outer margins surrounding all subplot "positions" are independent
    ## of the number of rows and/or columns
    margins.left   = defaultaxesposition(1);
    margins.bottom = defaultaxesposition(2);
    margins.right  = 1.0 - margins.left - defaultaxesposition(3);
    margins.top    = 1.0 - margins.bottom - defaultaxesposition(4);

    ## Fit from Matlab experiments
    pc = 1 ./ [0.1860, (margins.left + margins.right - 1)];
    margins.column = 1 ./ polyval (pc , cols);
    pr = 1 ./ [0.2282, (margins.top + margins.bottom - 1)];
    margins.row    = 1 ./ polyval (pr , rows);

    ## Calculate the width/height of the subplot axes "position".
    ## This is also consistent with Matlab
    width = 1 - margins.left - margins.right - (cols-1)*margins.column;
    width = width / cols;
    height = 1 - margins.top - margins.bottom - (rows-1)*margins.row;
    height = height / rows;
  endif

  ## Index offsets from the lower left subplot
  yi = fix ((index(:)-1)/cols);
  xi = index(:) - yi*cols - 1;
  yi = (rows - 1) - yi;

  ## Lower left corner of the subplot, i.e. position(1:2)
  x0 = xi .* (width + margins.column) + margins.left;
  y0 = yi .* (height + margins.row) + margins.bottom;

  if (numel(x0) > 1)
    ## subplot (row, col, m:n)
    x1 = max (x0(:)) + width;
    y1 = max (y0(:)) + height;
    x0 = min (x0(:));
    y0 = min (y0(:));
    pos = [x0, y0, x1-x0, y1-y0];
  else
    ## subplot (row, col, num)
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

%!demo
%! clf
%! x = 0:1;
%! for n = 1:4
%!   subplot (2, 2, n, "align")
%!   plot (x, x)
%!   xlabel (sprintf ("xlabel (2,2,%d)", n))
%!   ylabel (sprintf ("ylabel (2,2,%d)", n))
%!   title (sprintf ("title (2,2,%d)", n))
%! endfor
%! subplot (1, 2, 1, "align")
%! plot (x, x)
%! xlabel ("xlabel (1,2,1)")
%! ylabel ("ylabel (1,2,1)")
%! title ("title (1,2,1)")

