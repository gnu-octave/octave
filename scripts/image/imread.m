## Copyright (C) 2013 Carnë Draug
## Copyright (C) 2008-2012 Thomas L. Scofield
## Copyright (C) 2008 Kristian Rumberg
## Copyright (C) 2006 Thomas Weber
## Copyright (C) 2005 Stefan van der Walt
## Copyright (C) 2002 Andy Adler
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
## @deftypefn  {Function File} {[@var{img}, @var{map}, @var{alpha}] =} imread (@var{filename})
## @deftypefnx {Function File} {[@dots{}] =} imread (@var{filename}, @var{ext})
## @deftypefnx {Function File} {[@dots{}] =} imread (@var{url})
## @deftypefnx {Function File} {[@dots{}] =} imread (@dots{}, @var{idx})
## @deftypefnx {Function File} {[@dots{}] =} imread (@dots{}, @var{param1}, @var{val1}, @dots{})
## Read images from various file formats.
##
## Reads an image as a matrix from the file @var{filename}.  If there is
## no file @var{filename}, and @var{ext} was specified, it will look for
## a file named @var{filename} and extension @var{ext}, i.e., a file named
## @var{filename}.@var{ext}.
##
## The size and class of the output depends on the
## format of the image.  A color image is returned as an
## @nospell{MxNx3} matrix.  Gray-level and black-and-white images are
## of size @nospell{MxN}.  Multipage images will have an additional 4th
## dimension.
## The bit depth of the image determines the
## class of the output: "uint8" or "uint16" for gray
## and color, and "logical" for black and white.
## Note that indexed images always return the indexes for a colormap,
## independent if @var{map} is a requested output.  To obtain the actual
## RGB image, use @code{ind2rgb}.
## See the Octave manual for more information in representing images.
##
## Some file formats, such as TIFF and GIF, are able to store multiple
## images in a single file.  @var{idx} can be a scalar or vector
## specifying the index of the images to read.  By default, Octave
## will only read the first page.
##
## Depending on the file format, it is possible to configure the reading
## of images with @var{param}, @var{val} pairs.  The following options
## are supported:
##
## @table @samp
## @item "Frames" or "Index"
## This is an alternative method to specify @var{idx}.  When specifying it
## in this way, its value can also be the string "all".
##
## @item "Info"
## This option exists for @sc{matlab} compatibility and has no effect.  For
## maximum performance while reading multiple images from a single file,
## use the Index option.
##
## @item "PixelRegion"
## Controls the image region that is read.  Takes as value a cell array
## with two arrays of 3 elements @code{@{@var{rows} @var{cols}@}}.  The
## elements in the array are the start, increment and end pixel to be
## read.  If the increment value is omitted, defaults to 1.
## @end table
##
## @seealso{imwrite, imfinfo, imformats}
## @end deftypefn

## Author: Carnë Draug <carandraug@octave.org>
## Author: Thomas L. Scofield <scofield@calvin.edu>
## Author: Kristian Rumberg <kristianrumberg@gmail.com>
## Author: Thomas Weber <thomas.weber.mail@gmail.com>
## Author: Stefan van der Walt <stefan@sun.ac.za>
## Author: Andy Adler

function varargout = imread (varargin)
  if (nargin < 1)
    print_usage ();
  elseif (! ischar (varargin{1}))
    error ("imread: FILENAME must be a string");
  endif
  ## In case the file format was specified as a separate argument we
  ## do this. imageIO() will ignore the second part if filename on its
  ## own is enough. And if the second argument was a parameter name instead
  ## of an extension, it is still going to be passed to the next function
  ## since we are passing the whole function input as well.
  filename = {varargin{1}};
  if (nargin > 1 && ischar (varargin {2}))
    filename{2} = varargin{2};
  endif

  [varargout{1:nargout}] = imageIO (@__imread__, "read", filename, varargin{:});
endfunction

%!testif HAVE_MAGICK
%! vpng = [ ...
%!  137,  80,  78,  71,  13,  10,  26,  10,   0,   0, ...
%!    0,  13,  73,  72,  68,  82,   0,   0,   0,   3, ...
%!    0,   0,   0,   3,   8,   2,   0,   0,   0, 217, ...
%!   74,  34, 232,   0,   0,   0,   1, 115,  82,  71, ...
%!   66,   0, 174, 206,  28, 233,   0,   0,   0,   4, ...
%!  103,  65,  77,  65,   0,   0, 177, 143,  11, 252, ...
%!   97,   5,   0,   0,   0,  32,  99,  72,  82,  77, ...
%!    0,   0, 122,  38,   0,   0, 128, 132,   0,   0, ...
%!  250,   0,   0,   0, 128, 232,   0,   0, 117,  48, ...
%!    0,   0, 234,  96,   0,   0,  58, 152,   0,   0, ...
%!   23, 112, 156, 186,  81,  60,   0,   0,   0,  25, ...
%!   73,  68,  65,  84,  24,  87,  99,  96,  96,  96, ...
%!  248, 255, 255,  63, 144,   4,  81, 111, 101,  84, ...
%!   16,  28, 160,  16,   0, 197, 214,  13,  34,  74, ...
%!  117, 213,  17,   0,   0,   0,   0,  73,  69,  78, ...
%!   68, 174,  66,  96, 130];
%! filename = [tmpnam() ".png"];
%! unwind_protect
%!   fid = fopen (filename, "wb");
%!   fwrite (fid, vpng);
%!   fclose (fid);
%!   A = imread (filename);
%! unwind_protect_cleanup
%!   unlink (filename);
%! end_unwind_protect
%! assert (A(:,:,1), uint8 ([0, 255, 0; 255, 237, 255; 0, 255, 0]));
%! assert (A(:,:,2), uint8 ([0, 255, 0; 255,  28, 255; 0, 255, 0]));
%! assert (A(:,:,3), uint8 ([0, 255, 0; 255,  36, 255; 0, 255, 0]));

