## Copyright (C) 2006 Daniel Sebald
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
## @deftypefn {Function File} {} __img_gnuplot__ (@var{x}, @var{y}, @var{A})
## Display an image using @code{gnuplot}, where vectors @var{x} and
## @var{y} define the axes and the matrix @var{A} contains the image
## data.
## @end deftypefn

function __img_gnuplot__ (x, y, A, zoom)

  ## ZOOM is ignored.

  __plot_globals__;

  cf = __current_figure__;
  mxi = __multiplot_xi__(cf);
  myi = __multiplot_yi__(cf);

  __setup_plot__ ("plot");

  if (nargin < 3)
    print_usage ();
  endif

  if (isempty (A))
    error ("__img_gnuplot__: matrix is empty");
  endif

  ## Use the newly added mode of "plot" called "with image".
  if (isempty (x))
    x = [1, columns(A)];
    y = [1, rows(A)];
  endif

  ## Force rectangular grid by using only end points of
  ## first row (column) if x (y) is a matrix or vector.
  if (columns (x) > 1)
    x = x(1,:)';
  endif
  if (abs (x(end) - x(1)) < 10*eps)
    error ("__img_gnuplot__: end points in x dimension must not be equal");
  else
    x_dim = size (A, 2);
    if (x_dim > 1)
      dx = abs (x(end)-x(1))/(x_dim-1);
    else
      dx = 1;
    endif
  endif
  if (rows (y) > 1)
    y = y(:,1)';
  endif
  if (abs (y(end) - y(1)) < 10*eps)
    error ("__img_gnuplot__: end points in y dimension must not be equal");
  else
    y_dim = size (A, 1);
    if (y_dim > 1)
      dy = abs (y(end)-y(1))/(y_dim-1);
    else
      dy = 1;
    endif
  endif

  x_origin = min (x(1), x(end));
  y_origin = min (y(1), y(end));

  j = __plot_data_offset__{cf}(mxi,myi);

  __plot_data__{cf}{mxi,myi}{j}{1} = A;
  __plot_data_type__{cf}{mxi,myi}(j) = 1;
  __plot_key_labels__{cf}{mxi,myi}{j}{1} = "";
  __plot_image_colormap__{cf}{mxi,myi} = colormap ();
  __plot_image_dims__{cf}{mxi,myi}{j}{1} ...
      = [x_dim, y_dim, x_origin, y_origin, dx, dy];

  __plot_data_offset__{cf}(mxi,myi) = ++j;

  __render_plot__ ();

endfunction
