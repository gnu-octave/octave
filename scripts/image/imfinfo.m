## Copyright (C) 2008-2012 Soren Hauberg <hauberg@gmail.com>
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
## @deftypefn  {Function File} {@var{info} =} imfinfo (@var{filename})
## @deftypefnx {Function File} {@var{info} =} imfinfo (@var{url})
## Read image information from a file.
##
## @code{imfinfo} returns a structure containing information about the image
## stored in the file @var{filename}.  The output structure contains the
## following fields.
##
## @table @samp
## @item Filename
## The full name of the image file.
##
## @item FileSize
## Number of bytes of the image on disk
##
## @item FileModDate
## Date of last modification to the file.
##
## @item Height
## Image height in pixels.
##
## @item Width
## Image Width in pixels.
##
## @item BitDepth
## Number of bits per channel per pixel.
##
## @item Format
## Image format (e.g., @code{"jpeg"}).
##
## @item LongFormat
## Long form image format description.
##
## @item XResolution
## X resolution of the image.
##
## @item YResolution
## Y resolution of the image.
##
## @item TotalColors
## Number of unique colors in the image.
##
## @item TileName
## Tile name.
##
## @item AnimationDelay
## Time in 1/100ths of a second (0 to 65535) which must expire before displaying
## the next image in an animated sequence.
##
## @item AnimationIterations
## Number of iterations to loop an animation (e.g., Netscape loop extension)
## for.
##
## @item ByteOrder
## Endian option for formats that support it.  Is either @code{"little-endian"},
## @code{"big-endian"}, or @code{"undefined"}.
##
## @item Gamma
## Gamma level of the image.  The same color image displayed on two different
## workstations may look different due to differences in the display monitor.
##
## @item Matte
## @code{true} if the image has transparency.
##
## @item ModulusDepth
## Image modulus depth (minimum number of bits required to support
## red/green/blue
## components without loss of accuracy).
##
## @item Quality
## JPEG/MIFF/PNG compression level.
##
## @item QuantizeColors
## Preferred number of colors in the image.
##
## @item ResolutionUnits
## Units of image resolution.  Is either @code{"pixels per inch"},
## @code{"pixels per centimeter"}, or @code{"undefined"}.
##
## @item ColorType
## Image type.  Is either @code{"grayscale"}, @code{"indexed"},
## @code{"truecolor"},
## or @code{"undefined"}.
##
## @item View
## FlashPix viewing parameters.
## @end table
##
## @seealso{imread, imwrite}
## @end deftypefn

function info = imfinfo (filename)

  if (nargin < 1)
    print_usage ();
  endif

  if (!ischar (filename))
    error ("imfinfo: FILENAME must be a string");
  endif

  filename = tilde_expand (filename);

  delete_file = false;

  unwind_protect

    fn = file_in_path (IMAGE_PATH, filename);

    if (isempty (fn))

      ## Couldn't find file. See if it's an URL.

      tmp = tmpnam ();

      [fn, status, msg] = urlwrite (filename, tmp);

      if (! status)
        error ("imfinfo: cannot find %s", filename);
      endif

      if (! isempty (fn))
        delete_file = true;
      endif

    endif

    info = __magick_finfo__ (fn);

  unwind_protect_cleanup

    if (delete_file)
      unlink (fn);
    endif

  end_unwind_protect

endfunction
