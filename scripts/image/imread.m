## Copyright (C) 2002 Andy Adler
## Copyright (C) 2005 Stefan van der Walt <stefan@sun.ac.za>
## Copyright (C) 2006 Thomas Weber <thomas.weber.mail@gmail.com>
## Copyright (C) 2008 Kristian Rumberg <kristianrumberg@gmail.com>
## Copyright (C) 2008 Thomas L. Scofield <scofield@calvin.edu>
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
## @deftypefn {Function File} {[@var{img}, @var{map}, @var{alpha}] =} imread (@var{filename})
## Read images from various file formats.
##
## The size and numeric class of the output depends on the
## format of the image.  A colour image is returned as an
## MxNx3 matrix.  Grey-level and black-and-white images are
## of size MxN.
## The colour depth of the image determines the numeric
## class of the output: "uint8" or "uint16" for grey
## and colour, and "logical" for black and white.
## @end deftypefn

function varargout = imread (filename, varargin)

  if (nargin < 1)
    print_usage ();
  endif

  if (! ischar (filename))
    error ("imread: filename must be a string")
  endif

  filename = tilde_expand (filename);

  fn = file_in_path (IMAGE_PATH, filename);

  if (isempty (fn))
    error ( "imread: cannot find %s", filename);
  endif

  try
    [varargout{1:nargout}] = __magick_read__ (fn, varargin{:});
  catch

    magick_error = lasterr ();

    img_field = false;
    x_field = false;
    map_field = false;

    try
      vars = load (fn);
      if (isstruct (vars))
	img_field = isfield (vars, "img");
	x_field = isfield (vars, "X");
	map_field = isfield (vars, "map");
      endif
    catch
      error ("imread: invalid image file: %s", magick_error)
    end_try_catch

    if (map_field && (img_field || x_field))
      varargout{2} = vars.map;
      if (img_field)
	varargout{1} = vars.img;
      else
	varargout{1} = vars.X;
      endif
    else
      error ("imread: invalid Octave image file format");
    endif

  end_try_catch

endfunction
