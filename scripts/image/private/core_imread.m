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

## This function does all the work of imread. It exists here as private
## function so that imread can use other functions if imformats is
## configured to. It is also needed so that imformats can create a
## function handle for it.

## Author: Carnë Draug <carandraug@octave.org>
## Author: Thomas L. Scofield <scofield@calvin.edu>
## Author: Kristian Rumberg <kristianrumberg@gmail.com>
## Author: Thomas Weber <thomas.weber.mail@gmail.com>
## Author: Stefan van der Walt <stefan@sun.ac.za>
## Author: Andy Adler

function varargout = core_imread (filename, varargin)

  if (nargin < 1)
    print_usage ("imread");
  elseif (! ischar (filename))
    error ("imread: FILENAME must be a string");
  endif

  filename  = tilde_expand (filename);
  fn        = file_in_path (IMAGE_PATH, filename);
  if (isempty (fn) && nargin >= 2 && ischar (varargin{1}))
    ## if we can't find the file, check if the next input is the file extension
    filename  = [filename "." varargin{1}];
    fn        = file_in_path (IMAGE_PATH, filename);
  endif
  if (isempty (fn))
    error ("imread: cannot find %s", filename);
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
      error ("imread: invalid image file: %s", magick_error);
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
%! fid = fopen ("test.png", "wb");
%! fwrite (fid, vpng);
%! fclose (fid);
%! A = imread ("test.png");
%! delete ("test.png");
%! assert (A(:,:,1), uint8 ([0, 255, 0; 255, 237, 255; 0, 255, 0]));
%! assert (A(:,:,2), uint8 ([0, 255, 0; 255,  28, 255; 0, 255, 0]));
%! assert (A(:,:,3), uint8 ([0, 255, 0; 255,  36, 255; 0, 255, 0]));

