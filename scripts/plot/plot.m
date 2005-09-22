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
## @deftypefn {Function File} {} plot (@var{args})
## This function produces two-dimensional plots.  Many different
## combinations of arguments are possible.  The simplest form is
##
## @example
## plot (@var{y})
## @end example
##
## @noindent
## where the argument is taken as the set of @var{y} coordinates and the
## @var{x} coordinates are taken to be the indices of the elements,
## starting with 1.
##
## If more than one argument is given, they are interpreted as
##
## @example
## plot (@var{x}, @var{y}, @var{fmt} ...)
## @end example
##
## @noindent
## where @var{y} and @var{fmt} are optional, and any number of argument
## sets may appear.  The @var{x} and @var{y} values are
## interpreted as follows:
##
## @itemize @bullet
## @item
## If a single data argument is supplied, it is taken as the set of @var{y}
## coordinates and the @var{x} coordinates are taken to be the indices of
## the elements, starting with 1.
##
## @item
## If the first argument is a vector and the second is a matrix, the
## the vector is plotted versus the columns (or rows) of the matrix.
## (using whichever combination matches, with columns tried first.)
##
## @item
## If the first argument is a matrix and the second is a vector, the
## the columns (or rows) of the matrix are plotted versus the vector.
## (using whichever combination matches, with columns tried first.)
##
## @item
## If both arguments are vectors, the elements of @var{y} are plotted versus
## the elements of @var{x}.
##
## @item
## If both arguments are matrices, the columns of @var{y} are plotted
## versus the columns of @var{x}.  In this case, both matrices must have
## the same number of rows and columns and no attempt is made to transpose
## the arguments to make the number of rows match.
##
## If both arguments are scalars, a single point is plotted.
## @end itemize
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
## Here are some plot examples:
##
## @example
## plot (x, y, "@@12", x, y2, x, y3, "4", x, y4, "+")
## @end example
##
## This command will plot @code{y} with points of type 2 (displayed as
## @samp{+}) and color 1 (red), @code{y2} with lines, @code{y3} with lines of
## color 4 (magenta) and @code{y4} with points displayed as @samp{+}.
##
## @example
## plot (b, "*")
## @end example
##
## This command will plot the data in the variable @code{b} will be plotted
## with points displayed as @samp{*}.
##
## @example
## t = 0:0.1:6.3;
## plot (t, cos(t), "-;cos(t);", t, sin(t), "+3;sin(t);");
## @end example
##
## This will plot the cosine and sine functions and label them accordingly
## in the key.
## @end deftypefn
##
## @seealso{semilogx, semilogy, loglog, polar, mesh, contour, __pltopt__
## bar, stairs, errorbar, replot, xlabel, ylabel, and title}

## Author: jwe

function plot (varargin)

  ## XXX FIXME XXX -- these plot states should really just be set
  ## temporarily, probably inside an unwind_protect block, but there is
  ## no way to determine their current values.

  __gnuplot_raw__ ("set nologscale;\n");
  __gnuplot_raw__ ("set nopolar;\n");

  __plt__ ("plot", varargin{:});

endfunction
