## Copyright (C) 2008-2012 John W. Eaton
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
## @deftypefn  {Function File} {} imwrite (@var{img}, @var{filename})
## @deftypefnx {Function File} {} imwrite (@var{img}, @var{filename}, @var{fmt})
## @deftypefnx {Function File} {} imwrite (@var{img}, @var{filename}, @var{fmt}, @var{p1}, @var{v1}, @dots{})
## @deftypefnx {Function File} {} imwrite (@var{img}, @var{map}, @var{filename}, @dots{})
## Write images in various file formats.
##
## If @var{fmt} is not supplied, the file extension of @var{filename} is used
## to determine the format.
##
## The parameter-value pairs (@var{p1}, @var{v1}, @dots{}) are optional.
## Currently the following options are supported for @t{JPEG} images:
##
## @table @samp
## @item Quality
## Set the quality of the compression.  The value should be an
## integer between 0 and 100, with larger values indicating higher visual
## quality and lower compression.
## @end table
##
## @strong{Supported Formats}
## @multitable @columnfractions .33 .66
## @headitem Extension @tab Format
## @item bmp @tab Windows Bitmap
## @item gif @tab Graphics Interchange Format
## @item jpg and jpeg @tab Joint Photographic Experts Group
## @item pbm @tab Portable Bitmap
## @item pcx @tab
## @item pgm @tab Portable Graymap
## @item png @tab Portable Network Graphics
## @item pnm @tab Portable Anymap
## @item ppm @tab Portable Pixmap
## @item ras @tab Sun Raster
## @item tif and tiff @tab Tagged Image File Format
## @item xwd @tab X11 Dump
## @end multitable
##
## @strong{Unsupported Formats}
## @multitable @columnfractions .33 .66
## @headitem Extension @tab Format
## @item hdf @tab Hierarchical Data Format V4
## @item @nospell{jp2} and jpx @tab Joint Photographic Experts Group 2000
## @end multitable
##
## @seealso{imread, imfinfo}
## @end deftypefn

function imwrite (img, varargin)

  persistent imwrite_possible_formats = {
    "bmp"; "gif"; "jp2"; "jpg"; "jpx"; "jpeg"; "hdf"; "pbm"; "pcx";
    "pgm"; "png"; "pnm"; "ppm"; "ras"; "tif"; "tiff"; "xwd" };

  persistent accepted_formats = __magick_format_list__ (imwrite_possible_formats);

  if (nargin < 2 || ! (isnumeric (img) || islogical (img)))
    print_usage ();
  endif

  map = [];
  fmt = "";

  offset = 1;
  if (isnumeric (varargin{1}))
    map = varargin{1};
    if (isempty (map))
      error ("imwrite: colormap must not be empty");
    endif
    offset = 2;
  endif
  if (offset <= length (varargin) && ischar (varargin{offset}))
    filename = varargin{offset};
    offset++;
    if (rem (length (varargin) - offset, 2) == 0 && ischar (varargin{offset}))
      fmt = varargin{offset};
      offset++;
    endif
  else
    print_usage ();
  endif
  if (offset < length (varargin))
    has_param_list = 1;
    for ii = offset:2:(length (varargin) - 1)
      options.(varargin{ii}) = varargin{ii + 1};
    endfor
  else
    has_param_list = 0;
  endif

  filename = tilde_expand (filename);

  if (isempty (fmt))
    [d, n, fmt] = fileparts (filename);
    if (! isempty (fmt))
      fmt = fmt(2:end);
    endif
  endif

  if (isempty (img))
    error ("imwrite: invalid empty image");
  endif

  if (issparse (img) || issparse (map))
    error ("imwrite: sparse images not supported");
  endif

  if (! strcmp (fmt, accepted_formats))
    error ("imwrite: %s: unsupported or invalid image format", fmt);
  endif

  img_class = class (img);
  map_class = class (map);
  nd = ndims (img);

  if (isempty (map))
    if (any (strcmp (img_class, {"logical", "uint8", "uint16", "double"})))
      if ((nd == 2 || nd == 3) && strcmp (img_class, "double"))
        img = uint8 (img * 255);
      endif
      ## FIXME -- should we handle color images w/ alpha channel here?
      if (nd == 3 && size (img, 3) < 3)
        error ("imwrite: invalid dimensions for truecolor image");
      endif
      if (nd > 5)
        error ("imwrite: invalid %d-dimensional image data", nd);
      endif
    else
      error ("imwrite: %s: invalid class for truecolor image", img_class);
    endif
    if (has_param_list)
      __magick_write__ (filename, fmt, img, options);
    else
      __magick_write__ (filename, fmt, img);
    endif
  else
    if (any (strcmp (img_class, {"uint8", "uint16", "double"})))
      if (strcmp (img_class, "double"))
        img = uint8 (img - 1);
      endif
      if (nd != 2 && nd != 4)
        error ("imwrite: invalid size for indexed image");
      endif
    else
      error ("imwrite: %s: invalid class for indexed image data", img_class);
    endif
    if (isa (map, "double"))
      if (ndims (map) != 2 || size (map, 2) != 3)
        error ("imwrite: invalid size for colormap");
      endif
    else
      error ("imwrite: %s invalid class for indexed image colormap",
             class (map));
    endif

    ## FIXME -- we should really be writing indexed images here but
    ## __magick_write__ needs to be fixed to handle them.

    [r, g, b] = ind2rgb (img, map);
    tmp = uint8 (cat (3, r, g, b) * 255);

    if (has_param_list)
      __magick_write__ (filename, fmt, tmp, options);
      ## __magick_write__ (filename, fmt, img, map, options);
    else
      __magick_write__ (filename, fmt, tmp);
      ## __magick_write__ (filename, fmt, img, map);
    endif
  endif

endfunction

%% Test input validation
%!error imwrite ()                           # Wrong # of args
%!error imwrite (1)                          # Wrong # of args
%!error imwrite ({"cell"}, "filename.jpg")   # Wrong class for img
%!error imwrite (1, [], "filename.jpg")      # Empty image map
%!error imwrite (1, 2, 3)                    # No filename specified
%!error imwrite (1, "filename")              # No fmt specified
%!error imwrite (1, "filename", "junk")      # Invalid fmt specified
%!error imwrite ([], "filename.jpg")         # Empty img matrix
%!error imwrite (spones(2), "filename.jpg")  # Invalid sparse img

