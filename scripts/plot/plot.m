## Copyright (C) 1993-2012 John W. Eaton
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
## @deftypefn  {Function File} {} plot (@var{y})
## @deftypefnx {Function File} {} plot (@var{x}, @var{y})
## @deftypefnx {Function File} {} plot (@var{x}, @var{y}, @var{property}, @var{value}, @dots{})
## @deftypefnx {Function File} {} plot (@var{x}, @var{y}, @var{fmt})
## @deftypefnx {Function File} {} plot (@var{hax}, @dots{})
## @deftypefnx {Function File} {@var{h} =} plot (@dots{})
## Produce 2-D plots.
##
## Many different combinations of arguments are possible.  The simplest
## form is
##
## @example
## plot (@var{y})
## @end example
##
## @noindent
## where the argument is taken as the set of @var{y} coordinates and the
## @var{x} coordinates are taken to be the indices of the elements
## starting with 1.
##
## To save a plot, in one of several image formats such as PostScript
## or PNG, use the @code{print} command.
##
## If more than one argument is given, they are interpreted as
##
## @example
## plot (@var{y}, @var{property}, @var{value}, @dots{})
## @end example
##
## @noindent
## or
##
## @example
## plot (@var{x}, @var{y}, @var{property}, @var{value}, @dots{})
## @end example
##
## @noindent
## or
##
## @example
## plot (@var{x}, @var{y}, @var{fmt}, @dots{})
## @end example
##
## @noindent
## and so on.  Any number of argument sets may appear.  The @var{x} and
## @var{y} values are interpreted as follows:
##
## @itemize @bullet
## @item
## If a single data argument is supplied, it is taken as the set of @var{y}
## coordinates and the @var{x} coordinates are taken to be the indices of
## the elements, starting with 1.
##
## @item
## If the @var{x} is a vector and @var{y} is a matrix, then
## the columns (or rows) of @var{y} are plotted versus @var{x}.
## (using whichever combination matches, with columns tried first.)
##
## @item
## If the @var{x} is a matrix and @var{y} is a vector,
## @var{y} is plotted versus the columns (or rows) of @var{x}.
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
## Multiple property-value pairs may be specified, but they must appear
## in pairs.  These arguments are applied to the line objects drawn by
## @code{plot}.
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
## If @var{c} is one of @code{"k"} (black), @code{"r"} (red), @code{"g"}
## (green), @code{"b"} (blue), @code{"m"} (magenta), @code{"c"} (cyan),
## or @code{"w"} (white), it is interpreted as the line plot color.
##
## @item ";title;"
## Here @code{"title"} is the label for the key.
##
## @item  +
## @itemx *
## @itemx o
## @itemx x
## @itemx ^
## Used in combination with the points or linespoints styles, set the point
## style.
##
## @item @@
## Select the next unused point style.
## @end table
##
## The @var{fmt} argument may also be used to assign key titles.
## To do so, include the desired title between semi-colons after the
## formatting sequence described above, e.g., "+3;Key Title;"
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
## plot (b, "*", "markersize", 3)
## @end example
##
## This command will plot the data in the variable @code{b},
## with points displayed as @samp{*} with a marker size of 3.
##
## @example
## @group
## t = 0:0.1:6.3;
## plot (t, cos(t), "-;cos(t);", t, sin(t), "+3;sin(t);");
## @end group
## @end example
##
## This will plot the cosine and sine functions and label them accordingly
## in the key.
##
## If the first argument @var{hax} is an axes handle, then plot into this axis,
## rather than the current axes returned by @code{gca}.
##
## The optional return value @var{h} is a vector of graphics handles to
## the created line objects.
##
## @seealso{axis, box, grid, hold, legend, title, xlabel, ylabel, xlim, ylim, ezplot, errorbar, fplot, line, plot3, polar, loglog, semilogx, semilogy, subplot}
## @end deftypefn

## Author: jwe

function h = plot (varargin)

  [hax, varargin, nargs] = __plt_get_axis_arg__ ("plot", varargin{:});

  if (nargs < 1)
    print_usage ();
  endif

  oldfig = ifelse (isempty (hax), [], get (0, "currentfigure"));
  unwind_protect
    hax = newplot (hax);
    htmp = __plt__ ("plot", hax, varargin{:});
  unwind_protect_cleanup
    if (! isempty (oldfig))
      set (0, "currentfigure", oldfig);
    endif
  end_unwind_protect

  if (nargout > 0)
    h = htmp;
  endif

endfunction


%!demo
%! x = 1:5;  y = 1:5;
%! plot (x,y,'g');
%! title ('plot of green line at 45 degrees');

%!demo
%! x = 1:5;  y = 1:5;
%! plot (x,y,'g*');
%! title ('plot of green stars along a line at 45 degrees');

%!demo
%! x1 = 1:5;  y1 = 1:5;
%! x2 = 5:9; y2 = 5:-1:1;
%! plot (x1,y1,'bo-', x2,y2,'rs-');
%! axis ('tight');
%! title ('plot of blue circles ascending and red squares descending with connecting lines drawn'); 

