## Copyright (C) 2008-2012 John W. Eaton
## Copyright (C) 2013 Carnë Draug
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
## @deftypefnx {Function File} {} imwrite (@var{img}, @var{filename}, @var{ext})
## @deftypefnx {Function File} {} imwrite (@var{img}, @var{filename}, @var{ext}, @var{p1}, @var{v1}, @dots{})
## @deftypefnx {Function File} {} imwrite (@var{img}, @var{map}, @var{filename}, @dots{})
## Write images in various file formats.
##
## If @var{ext} is not supplied, the file extension of @var{filename} is used
## to determine the format.  The actual supported formats are dependent on
## options made during the build of Octave.  Use @code{imformats} to check
## the support of the different image formats.
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
## @seealso{imread, imfinfo, imformats}
## @end deftypefn

function imwrite (varargin)
  if (nargin < 2)
    print_usage ();
  endif
  [filename, ext] = imwrite_filename (varargin{2:end});

  fmt = imformats (ext);
  ## When there is no match, fmt will be a 1x1 structure with
  ## no fields, so we can't just use `isempty (fmt)'.
  if (isempty (fieldnames (fmt)))
    if (isempty (ext))
      error ("imwrite: no extension found for %s to identify the image format",
             filename);
    endif
    warning ("imwrite: unlisted image format %s (see imformats). Trying to save anyway.",
             ext);
    core_imwrite (varargin{:});
  else
    fmt.write (varargin{:});
  endif

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

