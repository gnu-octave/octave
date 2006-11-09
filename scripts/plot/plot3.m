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
## @deftypefn {Function File} {} plot3 (@var{args})
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
## Additionally, only two arguments can be given as
##
## @example
## plot3 (@var{x}, @var{c})
## @end example
##
## where the real and imaginary parts of the second argument are used as
## the @var{y} and @var{z} coordinates, respectively.
##
## If only one argument is given, as
##
## @example
## plot3 (@var{c})
## @end example
##
## the real and imaginary parts of the argument are used as the @var{y}
## and @var{z} values, and they are plotted versus their index.
##
## To save a plot, in one of several image formats such as PostScript
## or PNG, use the @code{print} command.
##
## An optional format argument can be given as
##
## @example
## plot3 (@var{x}, @var{y}, @var{z}, @var{fmt})
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
## plot3 (@var{x1}, @var{y1}, @var{z1}, @var{x2}, @var{y2}, @var{z2}, @dots{})
## @end example
## 
## @noindent
## where each set of three arguments is treated as a seperate line or
## set of lines in three dimensions.
##
## To plot multiple one- or two-argument groups, separate each group with an
## empty format string, as
##
## @example
## plot3 (@var{x1}, @var{c1}, '', @var{c2}, '', @dots{})
## @end example
##
## An example of the use of plot3 is
##
## @example
## @group
##    z = [0:0.05:5];
##    plot3(cos(2*pi*z), sin(2*pi*z), z, ";helix;");
##    plot3(z, exp(2i*pi*z), ";complex sinusoid;");
## @end group
## @end example
##
## @seealso{plot, semilogx, semilogy, loglog, polar, mesh, contour, __pltopt__
## bar, stairs, errorbar, replot, xlabel, ylabel, title, print}
## @end deftypefn

## Author: Paul Kienzle
##         (modified from __plt__.m)

function plot3 (varargin)

  hold_state = ishold ();
  
  unwind_protect

    x_set = 0;
    y_set = 0;
    z_set = 0;
    
    ## Gather arguments, decode format, and plot lines.
    for arg = 1:nargin
      new = varargin{arg};
      
      if (ischar (new))
	if (! z_set)
	  if (! y_set)
	    if (! x_set)
	      error ("plot3: needs x, [ y, [ z ] ]");
	    else
	      z = imag (x);
	      y = real (x);
	      y_set = 1;
	      z_set = 1;
	      if (rows(x) > 1)
	        x = repmat ((1:rows(x))', 1, columns(x));
	      else
	        x = 1:columns(x);
	      endif
	    endif
	  else
	    z = imag (y);
	    y = real (y);
	    z_set = 1;
	  endif
	endif
	[fmt, key] = __pltopt__ ("plot3", new);

	if (isvector (x) && isvector (y))
	  if (isvector (z))
	    x = x(:);
	    y = y(:);
	    z = z(:);
	  elseif (length (x) == rows (z) && length (y) == columns (z))
	    error ("plot3: [length(x), length(y)] must match size(z)");
	  else
	    [x, y] = meshgrid (x, y);
	  endif
	endif

	if (any (size (x) != size (y)) || any (size (x) != size (z)))
	  error ("plot3: x, y, and z must have the same shape");
	endif

	__gnuplot_raw__ ("set nohidden3d;\n")
	__gnuplot_set__ parametric; 

	__plt3__ ([([x; NaN*ones(1,size(x,2))])(:), ...
		 ([y; NaN*ones(1,size(y,2))])(:), ...
		 ([z; NaN*ones(1,size(z,2))])(:)],
		  "u($1):($2):($3)", fmt{1}, key{1});

	hold ("on");
	x_set = 0;
	y_set = 0;
	z_set = 0;
      elseif (! x_set)
	x = new;
	x_set = 1;
      elseif (! y_set)
	y = new;
	y_set = 1;
      elseif (! z_set)
	z = new;
	z_set = 1;
      else
	if (isvector (x) && isvector (y))
	  if (isvector (z))
	    x = x(:);
	    y = y(:);
	    z = z(:);
	  elseif (length (x) == rows (z) && length (y) == columns (z))
	    error ("plot3: [length(x), length(y)] must match size(z)");
	  else
	    [x, y] = meshgrid (x, y);
	  endif
	endif

	if (any (size (x) != size (y)) || any (size (x) != size (z)))
	  error ("plot3: x, y, and z must have the same shape");
	endif

	__gnuplot_raw__ ("set nohidden3d;\n")
	__gnuplot_set__ parametric; 

	__plt3__ ([([x; NaN*ones(1,size(x,2))])(:), ...
		   ([y; NaN*ones(1,size(y,2))])(:), ...
		   ([z; NaN*ones(1,size(z,2))])(:)]);

	hold ("on");
	x = new;
	y_set = 0;
	z_set = 0;
      endif
       
    endfor
    
    ## Handle last plot.
    
    if (x_set)
      if (y_set)
        if (! z_set)
          z = imag (y);
          y = real (y);
          z_set = 1;
        endif
      else
        z = imag (x);
        y = real (x);
        y_set = 1;
        z_set = 1;
        if (rows (x) > 1)
          x = repmat ((1:rows (x))', 1, columns(x));
        else
          x = 1:columns(x);
        endif
      endif

      if (isvector (x) && isvector (y))
	if (isvector (z))
	  x = x(:);
	  y = y(:);
	  z = z(:);
	elseif (length (x) == rows (z) && length (y) == columns (z))
	  error ("plot3: [length(x), length(y)] must match size(z)");
	else
	  [x, y] = meshgrid (x, y);
	endif
      endif

      if (any (size (x) != size (y)) || any (size (x) != size (z)))
	error ("plot3: x, y, and z must have the same shape");
      endif

      __gnuplot_raw__ ("set nohidden3d;\n")
      __gnuplot_set__ parametric; 

      __plt3__ ([([x; NaN*ones(1,size(x,2))])(:), ...
		 ([y; NaN*ones(1,size(y,2))])(:), ...
		 ([z; NaN*ones(1,size(z,2))])(:)]);
    endif
    
  unwind_protect_cleanup
    
    if (! hold_state)
      hold ("off");
    endif
    
  end_unwind_protect

endfunction
