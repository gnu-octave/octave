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

## usage: plot_border (...)
##
## NOTE: this will work only with gnuplot installed with
##       multiplot patch
##
## Multiple arguments allowed to specify the sides on which the border
## is shown. allowed strings: 
##
## allowed input strings:
##
##  "blank", "BLANK", "b", "B",   --->  No borders displayed
##    "all",   "ALL", "a", "A",   ---> All borders displayed
##  "north", "NORTH", "n", "N",   ---> North Border
##  "south", "SOUTH", "s", "S",   ---> South Border
##   "east",  "EAST", "e", "E",   --->  East Border
##   "west",  "WEST", "w", "W",   --->  West Border
##
## Without any arguments, turns borders off.

## Author: Vinayak Dutt <Dutt.Vinayak@mayo.EDU>
## Created: 3 July 95
## Adapted-By: jwe

function plot_border (...)

  if (! gnuplot_has_multiplot)
    error ("plot_border: gnuplot does not appear to support this feature");
  endif
    
  south = 0;
  west = 0;
  north = 0;
  east = 0;
  all = 0;
  none = 1;

  va_start ();

  while (nargin--)

    arg = va_arg ();

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
    set noborder;
  else
    if (all)
      border = 15;
    else
      border = south + west + north + east;
    endif
    eval (sprintf ("set border %d", border));
  endif

endfunction
