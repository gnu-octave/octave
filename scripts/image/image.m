## Copyright (C) 1994-2012 John W. Eaton
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
## @deftypefn  {Function File} {} image (@var{img})
## @deftypefnx {Function File} {} image (@var{x}, @var{y}, @var{img})
## @deftypefnx {Function File} {@var{h} =} image (@dots{})
## Display a matrix as a color image.  The elements of @var{img} are indices
## into the current colormap, and the colormap will be scaled so that the
## extremes of @var{img} are mapped to the extremes of the colormap.
##
## The axis values corresponding to the matrix elements are specified in
## @var{x} and @var{y}.  If you're not using gnuplot 4.2 or later, these
## variables are ignored.
##
## Implementation Note: The origin (0, 0) for images is located in the
## upper left.  For ordinary plots, the origin is located in the lower
## left.  Octave handles this inversion by plotting the data normally,
## and then reversing the direction of the y-axis by setting the
## @code{ydir} property to @code{"reverse"}.  This has implications whenever
## an image and an ordinary plot need to be overlaid.  The recommended
## solution is to display the image and then plot the reversed ydata
## using, for example, @code{flipud (ydata,1)}.
##
## The optional return value @var{h} is a graphics handle to the image.
## @seealso{imshow, imagesc, colormap}
## @end deftypefn

## Author: Tony Richardson <arichard@stark.cc.oh.us>
## Created: July 1994
## Adapted-By: jwe

function retval = image (varargin)

  [ax, varargin, nargin] = __plt_get_axis_arg__ ("image", varargin{:});

  firstnonnumeric = Inf;
  for i = 1 : nargin
    if (! isnumeric (varargin{i}))
      firstnonnumeric = i;
      break;
    endif
  endfor

  if (nargin == 0 || firstnonnumeric == 1)
    img = imread ("default.img");
    x = y = [];
  elseif (nargin == 1 || firstnonnumeric == 2)
    img = varargin{1};
    x = y = [];
  elseif (nargin == 2 || firstnonnumeric == 3)
    print_usage ();
  else
    x = varargin{1};
    y = varargin{2};
    img = varargin{3};
    firstnonnumeric = 4;
  endif

  oldax = gca ();
  unwind_protect
    axes (ax);
    h = __img__ (x, y, img, varargin {firstnonnumeric:end});
    set (ax, "layer", "top");
  unwind_protect_cleanup
    axes (oldax);
  end_unwind_protect

  if (nargout > 0)
    retval = h;
  endif

endfunction

## Generic image creation.
##
## The axis values corresponding to the matrix elements are specified in
## @var{x} and @var{y}. If you're not using gnuplot 4.2 or later, these
## variables are ignored.

## Author: Tony Richardson <arichard@stark.cc.oh.us>
## Created: July 1994
## Adapted-By: jwe

function h = __img__ (x, y, img, varargin)
  
  newplot ();

  if (isempty (img))
    error ("__img__: matrix is empty");
  endif

  if (isempty (x))
    x = [1, columns(img)];
  endif

  if (isempty (y))
    y = [1, rows(img)];
  endif

  xdata = [x(1), x(end)];
  ydata = [y(1), y(end)];

  dx = diff (x);
  dy = diff (y);
  dx = std (dx) / mean (abs (dx));
  dy = std (dy) / mean (abs (dy));
  tol = 100*eps;
  if (any (dx > tol) || any (dy > tol))
    warning ("Image does not map to non-linearly spaced coordinates")
  endif

  ca = gca ();

  tmp = __go_image__ (ca, "cdata", img, "xdata", xdata, "ydata", ydata,
                    "cdatamapping", "direct", varargin {:});

  px = __image_pixel_size__ (tmp);

  if (xdata(2) < xdata(1))
    xdata = xdata(2:-1:1);
  elseif (xdata(2) == xdata(1))
    xdata = xdata(1) + [0, size(img,2)-1];
  endif
  if (ydata(2) < ydata(1))
    ydata = ydata(2:-1:1);
  elseif (ydata(2) == ydata(1))
    ydata = ydata(1) + [0, size(img,1)-1];
  endif
  xlim = xdata + [-px(1), px(1)];
  ylim = ydata + [-px(2), px(2)];

  ## FIXME -- how can we do this and also get the {x,y}limmode
  ## properties to remain "auto"?  I suppose this adjustment should
  ## happen automatically in axes::update_axis_limits instead of
  ## explicitly setting the values here.  But then what information is
  ## available to axes::update_axis_limits to determine that the
  ## adjustment is necessary?
  set (ca, "xlim", xlim, "ylim", ylim);

  if (ndims (img) == 3)
    if (isinteger (img))
      c = class (img);
      mn = intmin (c);
      mx = intmax (c);
      set (ca, "clim", double ([mn, mx]));
    endif
  endif

  set (ca, "view", [0, 90]);

  if (strcmp (get (ca, "nextplot"), "replace"))
    # Always reverse y-axis for images, unless hold is on
    set (ca, "ydir", "reverse");
  endif

  if (nargout > 0)
    h = tmp;
  endif

endfunction

%!demo
%! clf
%! img = 1 ./ hilb (11);
%! x = -5:5;
%! y = x;
%! subplot (2,2,1)
%! h = image (abs(x), abs(y), img);
%! set (h, "cdatamapping", "scaled")
%! ylabel ("limits = [4.5, 15.5]")
%! title ('image (abs(x), abs(y), img)')
%! subplot (2,2,2)
%! h = image (-x, y, img);
%! set (h, "cdatamapping", "scaled")
%! title ('image (-x, y, img)')
%! subplot (2,2,3)
%! h = image (x, -y, img);
%! set (h, "cdatamapping", "scaled")
%! title ('image (x, -y, img)')
%! ylabel ("limits = [-5.5, 5.5]")
%! subplot (2,2,4)
%! h = image (-x, -y, img);
%! set (h, "cdatamapping", "scaled")
%! title ('image (-x, -y, img)')

%!demo
%! clf
%! g = 0.1:0.1:10;
%! h = g'*g;
%! imagesc (g, g, sin (h));
%! hold on
%! imagesc (g, g+12, cos (h/2));
%! axis ([0 10 0 22])
%! hold off
%! title ("two consecutive images")

%!demo
%! clf
%! g = 0.1:0.1:10;
%! h = g'*g;
%! imagesc (g, g, sin (h));
%! hold all
%! plot (g, 11.0 * ones (size (g)))
%! imagesc (g, g+12, cos (h/2));
%! axis ([0 10 0 22])
%! hold off
%! title ("image, line, image")

%!demo
%! clf
%! g = 0.1:0.1:10;
%! h = g'*g;
%! plot (g, 10.5 * ones (size (g)))
%! hold all
%! imagesc (g, g, sin (h));
%! plot (g, 11.0 * ones (size (g)))
%! imagesc (g, g+12, cos (h/2));
%! plot (g, 11.5 * ones (size (g)))
%! axis ([0 10 0 22])
%! hold off
%! title ("line, image, line, image, line")

