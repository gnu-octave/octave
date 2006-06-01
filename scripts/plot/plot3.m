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
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} plot (@var{args})
##
## This function produces three-dimensional plots.  Many different
## combinations of arguments are possible.  The simplest form is
##
## @example
## plot3 (@var{x}, @var{y}, @var{z})
## @end example
##
## @noindent
## where the arguments are taken to be the vertices of the points to be
## plotted in three dimensions. If all arguments are vectors of the same
## length, then a single continuous line is drawn. If all arguments are
## matrices, then each column of the matrices is treated as a seperate
## line. No attempt is made to transpose the arguments to make the
## number of rows match.
##
## To save a plot, in one of several image formats such as PostScript
## or PNG, use the @code{print} command.
##
## An optional format argument can be given as
##
## @example
## plot3 (@var{x}, @var{y}, @var{y}, @var{fmt})
## @end example
##
## If the @var{fmt} argument is supplied, it is interpreted as
## follows.  If @var{fmt} is missing, the default gnuplot line style
## is assumed.
##
## @table @samp
## @item -
## Set lines plot style (default).
##
## @item .
## Set dots plot style.
##
## @item @@
## Set points plot style.
##
## @item -@@
## Set linespoints plot style.
##
## @item ^
## Set impulses plot style.
##
## @item L
## Set steps plot style.
##
## @item @var{n}
## Interpreted as the plot color if @var{n} is an integer in the range 1 to
## 6.
##
## @item @var{nm}
## If @var{nm} is a two digit integer and @var{m} is an integer in the
## range 1 to 6, @var{m} is interpreted as the point style.  This is only
## valid in combination with the @code{@@} or @code{-@@} specifiers.
##
## @item @var{c}
## If @var{c} is one of @code{"k"}, @code{"r"}, @code{"g"}, @code{"b"},
## @code{"m"}, @code{"c"}, or @code{"w"}, it is interpreted as the plot
## color (black, red, green, blue, magenta, cyan, or white).
##
## @item ";title;"
## Here @code{"title"} is the label for the key.
##
## @item +
## @itemx *
## @itemx o
## @itemx x
## Used in combination with the points or linespoints styles, set the point
## style.
## @end table
##
## The color line styles have the following meanings on terminals that
## support color.
##
## @example
## Number  Gnuplot colors  (lines)points style
##   1       red                   *
##   2       green                 +
##   3       blue                  o
##   4       magenta               x
##   5       cyan                house
##   6       brown            there exists
## @end example
##
## The @var{fmt} argument can also be used to assign key titles.
## To do so, include the desired title between semi-colons after the
## formatting sequence described above, e.g. "+3;Key Title;"
## Note that the last semi-colon is required and will generate an error if
## it is left out.
##
## Arguments can also be given in groups of three as
##
## @example
## plot3 (@var{x1}, @var{y1}, @var{y1}, @var{x2}, @var{y2}, @var{y2}, @dots{})
## @end example
## 
## @noindent
## where each set of three arguments are treated as seperate lines or
## sets of lines in three dimensions.
##
## An example of the use of plot3 is
##
## @example
## @group
##    z = [0:0.05:5];
##    plot3(cos(2*pi*z), sin(2*pi*z), z, ";helix;");
## @end group
## @end example
##
## @seealso{plot, semilogx, semilogy, loglog, polar, mesh, contour, __pltopt__
## bar, stairs, errorbar, replot, xlabel, ylabel, title, print}
## @end deftypefn

## Author: Paul Kienzle
##         (modified from __plt__.m)

function plot3(varargin)

  hold_state = ishold ();
  
  unwind_protect

    x_set = 0;
    y_set = 0;
    z_set = 0;
    
    ## Gather arguments, decode format, and plot lines.
    for arg = 1:length(varargin)
      new = varargin{arg};
      
      if (ischar (new))
	if (! z_set)
	  error ("plot3: needs x, y, z");
	endif
	fmt = __pltopt__ ("plot3", new);
	__plt3__(x, y, z, fmt);
	hold on;
	x_set = 0;
	y_set = 0;
	z_set = 0;
      elseif (!x_set)
	x = new;
	x_set = 1;
      elseif (!y_set)
	y = new;
	y_set = 1;
      elseif (!z_set)
	z = new;
	z_set = 1;
      else
	__plt3__ (x, y, z, "");
	hold on;
	x = new;
	y_set = 0;
	z_set = 0;
      endif
       
    endfor
    
    ## Handle last plot.
    
    if  (z_set)
      __plt3__ (x, y, z, "");
    elseif (x_set)
      error ("plot3: needs x, y, z");
    endif
    
  unwind_protect_cleanup
    
    if (! hold_state)
      hold off;
    endif
    
  end_unwind_protect

endfunction
