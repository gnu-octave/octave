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
## @deftypefn {Function File} {} plot_border (...)
## Multiple arguments allowed to specify the sides on which the border
## is shown.  Allowed arguments include:
##
## @table @code
## @item "blank"
## No borders displayed.
##
## @item "all"
## All borders displayed
##
## @item "north"
## North Border
##
## @item "south"
## South Border
##
## @item "east"
## East Border
##
## @item "west"
## West Border
## @end table
##
## @noindent
## The arguments may be abbreviated to single characters.  Without any
## arguments, @code{plot_border} turns borders off.
## @end deftypefn

## Author: Vinayak Dutt <Dutt.Vinayak@mayo.EDU>
## Created: 3 July 95
## Adapted-By: jwe

function plot_border (varargin)

  south = 0;
  west = 0;
  north = 0;
  east = 0;
  all = 0;
  none = 1;

  k = 1;

  nargs = nargin ();

  while (nargs--)

    arg = varargin{k++};

    if (! isstr (arg))
      error ("plot_border: input not a string");
    endif

    ## The effect of the arguments in cumulative.  If something is found
    ## after "b", do that and ignore "b".

    if (strcmp (arg, "blank") || strcmp (arg, "BLANK")
        || strcmp (arg, "b") || strcmp (arg, "B"))
      none = 1;
    else
      none = 0;
      if (strcmp (arg, "south") || strcmp (arg, "SOUTH")
          || strcmp (arg, "s") || strcmp (arg, "S"))
        south = 1;
      elseif (strcmp (arg, "west") || strcmp (arg, "WEST")
              || strcmp (arg, "w") || strcmp (arg, "W"))
        west = 2;
      elseif (strcmp (arg, "north") || strcmp (arg, "NORTH")
              || strcmp (arg, "n") || strcmp (arg, "N"))
        north = 4;
      elseif (strcmp (arg, "east") || strcmp (arg, "EAST")
              || strcmp (arg, "e") || strcmp (arg, "E"))
        east = 8;
      elseif (strcmp (arg, "all") || strcmp (arg, "ALL")
              || strcmp (arg, "a") || strcmp (arg, "A"))
        all = 1;
      endif
    endif
  endwhile

  if (none)
    __gset__ noborder;
  else
    if (all)
      border = 15;
    else
      border = south + west + north + east;
    endif
    eval (sprintf ("__gset__ border %d", border));
  endif

  if (automatic_replot)
    replot ();
  endif

endfunction
