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

function imwrite (varargin)
  if (nargin < 2)
    print_usage ();
  endif

  if (ischar (varargin{2}))
    filename = varargin{2};
  elseif (nargin >= 3 && iscolormap (varargin{2}) && ! ischar (varargin{3}))
    filename = varargin{3}'
  else
    error ("imwrite: no FILENAME specified");
  endif
  varargout{1:nargout} = imageIO (@core_imwrite, "write", filename, varargin{:});

endfunction

%% Test input validation
%!error imwrite ()                            # Wrong # of args
%!error imwrite (1)                           # Wrong # of args
%!error imwrite ({"cell"}, "filename.jpg")    # Wrong class for img
%!error imwrite (1, [], "filename.jpg")       # Empty image map
%!error imwrite (1, 2, 3)                     # No filename specified
%!error imwrite (1, "filename")               # No fmt specified
%!error imwrite (1, "filename", "junk")       # Invalid fmt specified
%!error imwrite ([], "filename.jpg")          # Empty img matrix
%!error imwrite (spones (2), "filename.jpg")  # Invalid sparse img

