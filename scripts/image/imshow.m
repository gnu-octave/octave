########################################################################
##
## Copyright (C) 1994-2023 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.
##
########################################################################

## -*- texinfo -*-
## @deftypefn  {} {} imshow (@var{im})
## @deftypefnx {} {} imshow (@var{im}, @var{limits})
## @deftypefnx {} {} imshow (@var{im}, @var{map})
## @deftypefnx {} {} imshow (@var{rgb}, @dots{})
## @deftypefnx {} {} imshow (@var{filename})
## @deftypefnx {} {} imshow (@dots{}, @var{string_param1}, @var{value1}, @dots{})
## @deftypefnx {} {@var{h} =} imshow (@dots{})
## Display the image @var{im}, where @var{im} can be a 2-dimensional
## (grayscale image) or a 3-dimensional (RGB image) matrix.
##
## If @var{limits} is a 2-element vector @code{[@var{low}, @var{high}]}, the
## image is shown using a display range between @var{low} and @var{high}.  If
## an empty matrix is passed for @var{limits}, the display range is computed
## as the range between the minimal and the maximal value in the image.
##
## If @var{map} is a valid color map, the image will be shown as an indexed
## image using the supplied color map.
##
## If a filename is given instead of an image, the file will be read and shown.
##
## If given, the parameter @var{string_param1} has value @var{value1}.
## @var{string_param1} can be any of the following:
##
## @table @asis
## @item @qcode{"displayrange"}
## @var{value1} is the display range as described above.
##
## @item @qcode{"colormap"}
## @var{value1} is the colormap to use when displaying an indexed image.
##
## @item @qcode{"xdata"}
## If @var{value1} is a 2-element vector, it must contain horizontal image
## limits in the form [xfirst, xlast], where xfirst and xlast are the
## abscissa of the centers of the corner pixels.  Otherwise @var{value1}
##  must be a vector and only the first and last elements will be used
## for xfirst and xlast respectively.
##
## @item @qcode{"ydata"}
## If @var{value1} is a 2-element vector, it must contain vertical image
## limits in the form [yfirst, ylast], where yfirst and ylast are the ordinates
## of the center of the corner pixels.  Otherwise @var{value1} must be a vector
## and only the first and last elements will be used for yfirst and ylast
## respectively.
##
## @end table
##
## The optional return value @var{h} is a graphics handle to the image.
## @seealso{image, imagesc, colormap, gray2ind, rgb2ind}
## @end deftypefn

function h = imshow (im, varargin)

  if (nargin == 0)
    print_usage ();
  endif

  display_range = NA;
  truecolor = false;
  indexed = false;
  xdata = ydata = [];
  prop_val_args = {};

  ## Get the image.
  if (ischar (im))
    [im, map] = imread (im);
    if (isempty (map))
      indexed = false;
    else
      indexed = true;
      colormap (gca, map);
    endif
  endif

  nd = ndims (im);

  if (! ((isnumeric (im) || islogical (im)) && (nd == 2 || nd == 3)))
    error ("imshow: IM must be an image or the FILENAME of an image");
  endif

  if (nd == 2)
    if (! indexed)
      colormap (gca, gray ());
    endif
  elseif (size (im, 3) == 3)
    if (ismember (class (im), {"uint8", "uint16", "double", "single"}))
      truecolor = true;
    else
      error ("imshow: TrueColor image must be uint8, uint16, double, or single");
    endif
  else
    error ("imshow: image must be MxN or MxNx3 matrix");
  endif

  narg = 1;
  while (narg <= numel (varargin))
    arg = varargin{narg++};
    if (isnumeric (arg))
      if (numel (arg) == 2 || isempty (arg))
        display_range = arg;
      elseif (columns (arg) == 3)
        indexed = true;
        if (iscolormap (arg) && min (arg) >= 0 || max (arg) <= 1)
          colormap (gca,  arg);
        else
          error ("imshow: invalid colormap MAP");
        endif
      elseif (! isempty (arg))
        error ("imshow: argument number %d is invalid", narg);
      endif
    elseif (ischar (arg))
      switch (tolower (arg))
        case "border"
          warning ("imshow: border argument is not implemented");
          narg += 1;
        case "colormap"
          map = varargin{narg++};
          if (iscolormap (map) && min (map) >= 0 || max (map) <= 1)
            colormap (gca, map);
          else
            error ("imshow: invalid colormap");
          endif
        case "displayrange"
          display_range = varargin{narg++};
        case {"initialmagnification"}
          warning ("imshow: zoom argument ignored -- use GUI features");
          narg += 1;
        case "parent"
          prop_val_args(end+(1:2)) = {"parent", varargin{narg++}};
          if (! isaxes (prop_val_args{end}))
            error ("imshow: parent must be an axes handle");
          endif
        case "reduce"
          warning ("imshow: reduce argument is not implemented");
          narg += 1;
        case "xdata"
          xdata = varargin{narg++};
          if (! isvector (xdata))
            error ("imshow: xdata must be a vector");
          endif
          xdata = [xdata(1) xdata(end)];
        case "ydata"
          ydata = varargin{narg++};
          if (! isvector (ydata))
            error ("imshow: ydata must be a vector");
          endif
          ydata = [ydata(1) ydata(end)];
        otherwise
          warning ("imshow: unrecognized property %s", arg);
          narg += 1;
      endswitch
    else
      error ("imshow: argument number %d is invalid", narg);
    endif
  endwhile

  ## Check for complex images.
  if (iscomplex (im))
    warning ("imshow: only showing real part of complex image");
    im = real (im);
  endif

  ## Set default display range if display_range not set yet.
  if (isempty (display_range))
    display_range = double ([min(im(:)), max(im(:))]);
  elseif (isna (display_range))
    t = class (im);
    switch (t)
      case {"double", "single", "logical"}
        display_range = [0, 1];
      case {"uint8", "uint16", "int16"}
        display_range = [intmin(t), intmax(t)];
      otherwise
        error ("imshow: invalid data type for image");
    endswitch
  endif

  if (isfloat (im))
    nans = isnan (im(:));
    if (any (nans))
      warning ("Octave:imshow-NaN",
               "imshow: pixels with NaN or NA values are set to minimum pixel value");
      im(nans) = display_range(1);
    endif
  endif

  if (truecolor || indexed)
    htmp = image (xdata, ydata, im, prop_val_args{:});
  else
    htmp = imagesc (xdata, ydata, im, display_range, prop_val_args{:});
    set (get (htmp, "parent"), "clim", display_range);
  endif
  set (get (htmp, "parent"), "visible", "off", "view", [0, 90],
                             "ydir", "reverse", "layer", "top");
  axis ("image");

  if (nargout > 0)
    h = htmp;
  endif

endfunction


%!demo
%! clf;
%! imshow ("default.img");

%!demo
%! clf;
%! imshow ("default.img");
%! colormap (gca, autumn (64));

%!demo
%! clf;
%! [I, M] = imread ("default.img");
%! imshow (I, M);

%!demo
%! clf;
%! [I, M] = imread ("default.img");
%! [R, G, B] = ind2rgb (I, M);
%! imshow (cat (3, R, G*0.5, B*0.8));

%!demo
%! clf;
%! imshow (rand (100, 100));
%! title ({"imshow with random 100x100 matrix", "black and white"});

%!demo
%! clf;
%! imshow (rand (100, 100));
%! colormap (gca, jet (64));
%! title ({"imshow with random 100x100 matrix", "colormap() makes color image"});

%!demo
%! clf;
%! imshow (rand (100, 100, 3));
%! title ({"imshow with random 100x100x3 matrix", "RGB color"});

%!demo
%! clf;
%! imshow (100*rand (100, 100, 3));
%! title ({"imshow with random 100x100x3 matrix", "RGB values > 1 are clipped"});

## Test input validation
%!error <Invalid call> imshow ()
%!error <IM must be an image> imshow ({"cell"})
%!error <TrueColor image must be uint8> imshow (ones (3,3,3, "uint32"))
%!error <TrueColor image must be uint8> imshow (ones (3,3,3, "int16"))
%!error <image must be MxN or MxNx3 matrix> imshow (ones (4,4,4))

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   fail ("imshow ([1,1], [2 0 0])", "all MAP values must be in the range");
%!   fail ("imshow ([1,1], [1 0 0 0])", "argument number 2 is invalid");
%!   fail ('imshow ([1,1], "colormap", [2 0 0])', "all MAP values must be in the range");
%!   fail ('imshow ([1,1], "parent", -1)', "must be an axes handle");
%!   fail ('imshow ([1,1], "xdata", ones (2,2))', "xdata must be a vector");
%!   fail ('imshow ([1,1], "ydata", ones (2,2))', "ydata must be a vector");
%!   fail ('imshow ([1,1], "foobar")', "warning", "unrecognized property foobar");
%!   fail ("imshow ([1,1], {1})", "argument number 2 is invalid");
%!   fail ("imshow ([1+i,1-i])", "warning", "only showing real part of complex image");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
