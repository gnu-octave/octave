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

## Undocumented internal function.

## @deftypefn {Function File} {} __pltopt__ (@var{caller}, @var{opt})
##
## Decode plot option strings.
##
## @var{opt} can currently be some combination of the following:
##
## @table @code
## @item "-"
## For solid linestyle (default).
##
## @item "--"
## For dashed line style.
##
## @item "-."
## For linespoints plot style.
##
## @item ":"
## For dots plot style.
##
## @item "r"
## Red line color.
##
## @item "g"
## Green line color.
##
## @item "b"
## Blue line color.
##
## @item "c"
## Cyan line color.
##
## @item "m"
## Magenta line color.
##
## @item "y"
## Yellow line color.
##
## @item "k"
## Black line color.
##
## @item "w"
## White line color.
##
## @item ";title;"
## Here @code{"title"} is the label for the key.
##
## @item "+"
## @itemx "o"
## @itemx "*"
## @itemx "."
## @itemx "x"
## @itemx "s"
## @itemx "d"
## @itemx "^"
## @itemx "v"
## @itemx ">"
## @itemx "<"
## @itemx "p"
## @itemx "h"
## Used in combination with the points or linespoints styles, set the point
## style.
## @end table
##
## The legend may be fixed to include the name of the variable
## plotted in some future version of Octave.

## Author: jwe

function [options, valid] = __pltopt__ (caller, opt, err_on_invalid)

  valid = true;
  options =  __default_plot_options__ ();

  if ((nargin == 2 || nargin == 3) && (nargout == 1 || nargout == 2))
    if (nargin == 2)
      err_on_invalid = true;
    endif
    if (ischar (opt))
      nel = rows (opt);
    elseif (iscellstr (opt))
      nel = numel (opt);
    else
      error ("__pltopt__: expecting argument to be character string or cell array of character strings");
    endif
    if (ischar (opt))
      opt = cellstr (opt);
    endif
    for i = nel:-1:1
      [options(i), valid] = __pltopt1__ (caller, opt{i}, err_on_invalid);
      if (! err_on_invalid && ! valid)
	return;
      endif
    endfor
  else
    print_usage ();
  endif

endfunction
