## Copyright (C) 2005 Soren Hauberg
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
## @deftypefn {Function File} {} imshow (@var{im})
## @deftypefnx {Function File} {} imshow (@var{im}, @var{limits})
## @deftypefnx {Function File} {} imshow (@var{im}, @var{map})
## @deftypefnx {Function File} {} imshow (@var{R}, @var{G}, @var{B}, @dots{})
## @deftypefnx {Function File} {} imshow (@var{filename})
## @deftypefnx {Function File} {} imshow (@dots{}, @var{string_param1}, @var{value1}, ...)
## Display the image @var{im}, where @var{im} can a 2-dimensional
## (gray-scale image) or a 3-dimensional (RGB image) matrix. If three matrices
## of the same size are given as arguments, they will be concatenated into
## a 3-dimensional (RGB image) matrix.
##
## If @var{limits} is a 2-element vector @code{[@var{low}, @var{high}]},
## the image is shown using a display range between @var{low} and
## @var{high}.  If an empty matrix is passed for @var{limits}, the
## display range is computed as the range between the minimal and the
## maximal value in the image.
##
## If @var{map} is a valid color map, the image will be shown as an indexed
## image using the supplied color map.
##
## If a file name is given instead of an image, the file will be read and
## shown.
##
## If given, the parameter @var{string_param1} has value
## @var{value1}. @var{string_param1} can be any of the following:
## @table @samp
## @item "display_range"
## @var{value1} is the display range as described above.
##
## @item "InitialMagnification"
## @var{value1} sets the zoom level in percent. 
## If @var{value1} is 100 the image is showed unscaled.
## @end table
## @seealso{image, imagesc, colormap, gray2ind, rgb2ind}
## @end deftypefn

## Author: Soren Hauberg <hauberg at gmail dot com>
## Adapted-By: jwe

function imshow (im, varargin)

  if (nargin == 0)
    print_usage ();
  endif

  ## Get the image
  if (ischar (im))
    im = loadimage (im); # It would be better to use imread from octave-forge
  elseif (! ismatrix (im))
    error ("imshow: first argument must be an image or the filename of an image");
  endif
  
  ## Is the function called with 3 matrices (i.e., imshow (R, G, B))?
  if (nargin >= 3
      && ndims (im) == 2
      && ndims (varargin{1}) == 2
      && ndims (varargin{2}) == 2
      && size (im) == size (varargin{1})
      && size (im) == size (varargin{2}))
    im(:,:,3) = varargin{2};
    im(:,:,2) = varargin{1};
    varargin(1:2) = [];
  endif

  ## Set default display range.
  switch class (im)
    case {"uint8"}
      display_range = [0, 255];
    case {"uint16"}
      display_range = [0, 65535];
    case {"double", "single", "logical"}
      display_range = [0, 1];
    otherwise
      error ("imshow: currently only images whos class is uint8, uint16, logical, or double are supported");
  endswitch

  ## Set other default parameters.
  isindexed = false;
  initial_magnification = 100;
  old_colormap = color_map = colormap ();
  
  ## Handle the rest of the arguments.
  narg = 1;
  while (narg <= length (varargin))
    arg = varargin{narg};
    if (ismatrix (arg) && ndims (arg) == 2)
      display_range = arg;
    elseif (isempty (arg))
      display_range = [min(im(:)), max(im(:))];
    elseif (ismatrix (arg) && size (arg, 2) == 3)
      color_map = arg;
      isindexed = true;
    elseif (ischar (arg) && strcmpi (arg, "truesize"))
      initial_magnification = 100;
    elseif (ischar (arg) && strcmpi (arg, "displayrange"))
      narg++;
      display_range = varargin{narg};
    elseif (ischar (arg) && strcmpi (arg, "initialmagnification"))
      narg++;
      initial_magnification = varargin{narg};
    else
      warning ("imshow: input argument number %d is unsupported", narg) 
    endif
    narg++;
  endwhile

  ## Check for complex images
  if (iscomplex (im))
    warning ("imshow: only showing real part of complex image");
    im = real (im);
  endif
  
  ## Scale the image to the interval [0, 1] according to display_range.
  if (! isindexed)
    low = display_range(1);
    high = display_range(2);
    im = (double (im) - low)/(high-low);
    im(im < 0) = 0;
    im(im > 1) = 1;
  endif
  
  ## Convert to indexed image
  dim = ndims (im);
  if (dim == 2)
    im = round ((size (color_map, 1) - 1) * im);
  elseif (dim == 3 && size (im, 3) == 3)
    [im, color_map] = rgb2ind (im(:,:,1), im(:,:,2), im(:,:,3));
    #[im, color_map] = rgb2ind (im); # Change rgb2ind to support ND-arrays and then use this line
  else
    error ("imshow: input image must be a 2D or 3D matrix");
  endif
  
  ## And now, we show the image
  colormap (color_map);
  image (im, initial_magnification/100);
  colormap (old_colormap);

endfunction

%!error imshow ()                           # no arguments
%!error imshow ({"cell"})                   # No image or filename given
%!error imshow (int8(1))                    # Unsupported image class
%!error imshow (ones(4,4,4))                # Too many dimensions in image

%!demo
%!  imshow (loadimage ("default.img"));

%!demo
%!  I = loadimage ("default.img");
%!  imshow (I, "truesize")

%!demo
%!  [I, M] = loadimage ("default.img");
%!  imshow (I, M);

%!demo
%!  [I, M] = loadimage ("default.img");
%!  imshow (cat(3, I, I*0.5, I*0.8));

%!demo
%!  I = loadimage("default.img");
%!  imshow(I, I, I);
