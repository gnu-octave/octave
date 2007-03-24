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
## See @code{__pltopt__} for a description of the optional format
## argument.
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
## bar, stairs, errorbar, xlabel, ylabel, title, print}
## @end deftypefn

## Author: Paul Kienzle
##         (modified from __plt__.m)

function retval = plot3 (varargin)

  x_set = 0;
  y_set = 0;
  z_set = 0;

  idx = 0;

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
      options = __pltopt__ ("plot3", new);

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

      if (! size_equal (x, y) || ! size_equal (x, z))
	error ("plot3: x, y, and z must have the same shape");
      endif

      key = options.key;
      if (! isempty (key))
	set (gca (), "key", "on");
      endif

      tmp(++idx) = line (x(:), y(:), z(:), "keylabel", key,
			 "color", options.color,
			 "linestyle", options.linestyle,
			 "marker", options.marker);

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

      if (! size_equal (x, y) || ! size_equal (x, z))
	error ("plot3: x, y, and z must have the same shape");
      endif

      tmp(++idx) = line (x(:), y(:), z(:));

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

    if (! size_equal (x, y) || ! size_equal (x, z))
      error ("plot3: x, y, and z must have the same shape");
    endif

    tmp(++idx) = line (x(:), y(:), z(:));

  endif

  set (gca (), "view", [-37.5, 30]);

  if (nargout > 0 && idx > 0)
    retval = tmp;
  endif

endfunction
