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
## @deftypefnx {Function File} {} image (@dots{}, "@var{property}", @var{value}, @dots{})
## @deftypefnx {Function File} {@var{h} =} image (@dots{})
## Display a matrix as an indexed color image.
##
## The elements of @var{img} are indices into the current colormap.
## @var{x} and @var{y} are optional 2-element vectors, @w{@code{[min, max]}},
## which specify the range for the axis labels.  If a range is specified as
## @w{@code{[max, min]}} then the image will be reversed along that axis.  For
## convenience, @var{x} and @var{y} may be specified as N-element vectors
## matching the length of the data in @var{img}.  However, only the first and
## last elements will be used to determine the axis limits.
## @strong{Warning:} @var{x} and @var{y} are ignored when using gnuplot 4.0
## or earlier.
##
## The optional return value @var{h} is a graphics handle to the image.
##
## Implementation Note: The origin (0, 0) for images is located in the
## upper left.  For ordinary plots, the origin is located in the lower
## left.  Octave handles this inversion by plotting the data normally,
## and then reversing the direction of the y-axis by setting the
## @code{ydir} property to "reverse".  This has implications whenever
## an image and an ordinary plot need to be overlaid.  The recommended
## solution is to display the image and then plot the reversed ydata
## using, for example, @code{flipud (ydata)}.
##
## @seealso{imshow, imagesc, colormap}
## @end deftypefn

## Author: Tony Richardson <arichard@stark.cc.oh.us>
## Created: July 1994
## Adapted-By: jwe

function h = image (varargin)

  [hax, varargin, nargin] = __plt_get_axis_arg__ ("image", varargin{:});
  
  if (isempty (hax))
    hax = gca ();
  endif

  chararg = find (cellfun ("isclass", varargin, "char"), 1, "first");
  
  if (nargin == 0 || chararg == 1)
    img = imread ("default.img");
    x = y = [];
  elseif (nargin == 1 || chararg == 2)
    img = varargin{1};
    x = y = [];
  elseif (nargin == 2 || chararg == 3)
    print_usage ();
  else
    x = varargin{1};
    y = varargin{2};
    img = varargin{3};
    chararg = 4;
  endif
  
  htmp = __img__ (hax, x, y, img, varargin{chararg:end});
  set (hax, "layer", "top");

  if (nargout > 0)
    h = htmp;
  endif

endfunction

## Generic image creation.
##
## The axis values corresponding to the matrix elements are specified in
## @var{x} and @var{y}.  If you're not using gnuplot 4.2 or later, these
## variables are ignored.

## Author: Tony Richardson <arichard@stark.cc.oh.us>
## Created: July 1994
## Adapted-By: jwe

function h = __img__ (hax, x, y, img, varargin)

  if (isempty (img))
    error ("__img__: matrix is empty");
  endif

  ## FIXME: Hack for integer formats which use zero-based indexing
  ##        Hack favors correctness of display over size of image in memory.
  ##        True fix will be done in C++ code. 
  if (ndims (img) == 2 && (isinteger (img) || islogical (img)))
    img = single (img) + 1;
  endif

  if (isempty (x))
    x = [1, columns(img)];
  endif

  if (isempty (y))
    y = [1, rows(img)];
  endif

  xdata = x([1, end]);
  ydata = y([1, end]);

  if (numel (x) > 2 && numel (y) > 2)
    ## Test data for non-linear spacing which is unsupported
    ## FIXME: Need a better check on linearity
    tol = 100*eps;
    dx = diff (x);
    dy = diff (y);
    dx = std (dx) / mean (abs (dx));
    dy = std (dy) / mean (abs (dy));
    if (any (dx > tol) || any (dy > tol))
      warning ("image: non-linear X, Y data is ignored.  IMG will be shown with linear mapping");
    endif
  endif

  htmp = __go_image__ (hax, "cdata", img, "xdata", xdata, "ydata", ydata,
                       "cdatamapping", "direct", varargin {:});

  px = __image_pixel_size__ (htmp);

  if (xdata(2) < xdata(1))
    xdata = fliplr (xdata);
  elseif (xdata(2) == xdata(1))
    xdata = xdata(1) + [0, columns(img)-1];
  endif
  if (ydata(2) < ydata(1))
    ydata = fliplr (ydata);
  elseif (ydata(2) == ydata(1))
    ydata = ydata(1) + [0, rows(img)-1];
  endif
  xlim = xdata + [-px(1), px(1)];
  ylim = ydata + [-px(2), px(2)];

  ## FIXME -- how can we do this and also get the {x,y}limmode
  ## properties to remain "auto"?  I suppose this adjustment should
  ## happen automatically in axes::update_axis_limits instead of
  ## explicitly setting the values here.  But then what information is
  ## available to axes::update_axis_limits to determine that the
  ## adjustment is necessary?
  set (hax, "xlim", xlim, "ylim", ylim);

  if (ndims (img) == 3)
    if (isinteger (img))
      cls = class (img);
      mn = intmin (cls);
      mx = intmax (cls);
      set (hax, "clim", double ([mn, mx]));
    endif
  endif

  set (hax, "view", [0, 90]);

  if (strcmp (get (hax, "nextplot"), "replace"))
    # Always reverse y-axis for images, unless hold is on
    set (hax, "ydir", "reverse");
  endif

  if (nargout > 0)
    h = htmp;
  endif

endfunction


%!demo
%! clf;
%! colormap (jet (21));
%! img = 1 ./ hilb (11);
%! x = y = -5:5;
%! subplot (2,2,1);
%!  h = image (x, y, img);
%!  ylabel ("limits = [-5.5, 5.5]");
%!  title ("image (x, y, img)");
%! subplot (2,2,2);
%!  h = image (-x, y, img);
%!  title ("image (-x, y, img)");
%! subplot (2,2,3);
%!  h = image (x, -y, img);
%!  title ("image (x, -y, img)");
%!  ylabel ("limits = [-5.5, 5.5]");
%! subplot (2,2,4);
%!  h = image (-x, -y, img);
%!  title ("image (-x, -y, img)");

