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

function varargout = __imread__ (filename, varargin)

  if (nargin < 1)
    print_usage ("imread");
  elseif (! ischar (filename))
    error ("imread: FILENAME must be a string");
  endif

  ## keep track of the varargin offset we're looking at each moment
  offset    = 1;

  filename  = tilde_expand (filename);
  fn        = file_in_path (IMAGE_PATH, filename);
  if (isempty (fn) && nargin >= offset + 1 && ischar (varargin{offset}))
    ## if we can't find the file, check if the next input is the file extension
    filename  = [filename "." varargin{offset}];
    fn        = file_in_path (IMAGE_PATH, filename);
    offset++;
  endif
  if (isempty (fn))
    error ("imread: cannot find %s", filename);
  endif

  ## set default for options
  options = struct ("index", 1);

  ## Index is the only option that can be defined without the parameter/value
  ## pair style. When defining it here, the string "all" is invalid though.
  if (nargin >= offset + 1 && ! ischar (varargin{offset}))
    if (! is_valid_index_option (options.index))
      error ("imread: IDX must be a numeric vector");
    endif
    options.index = varargin{offset};
    offset++;
  endif

  if (rem (numel (varargin) - offset + 1, 2) != 0)
    error ("imread: no pair for all arguments (even number left)");
  endif

  for idx = offset:2:(numel (varargin) - offset + 1)

    switch (tolower (varargin{idx}))

      case {"frames", "index"},
        options.index = varargin{idx+1};
        if (! (is_valid_index_option (options.index)) &&
            ! (ischar (options.index) && strcmpi (options.index, "all")))
          error ("imread: value for %s must be a vector or the string `all'");
        endif

## FIXME: commented until it's implemented in __magick_read__
##      case "pixelregion",
##        options.region = varargin{idx+1};
##        if (! iscell (options.region) || numel (options.region) != 2)
##          error ("imread: value for %s must be a 2 element cell array",
##                 varargin{idx});
##        endif
##        for reg_idx = 1:2
##          if (numel (options.region{reg_idx}) == 3)
##            ## do nothing
##          elseif (numel (options.region{reg_idx}) == 2)
##            options.region{reg_idx}(3) = options.region{reg_idx}(2);
##            options.region{reg_idx}(2) = 1;
##          else
##            error ("imread: range for %s must be a 2 or 3 element vector",
##                   varargin{idx});
##          endif
##          options.region{reg_idx} = floor (options.region{reg_idx}(1)): ...
##                                    floor (options.region{reg_idx}(2)): ...
##                                    floor (options.region{reg_idx}(3));
##        endfor

      case "info",
        ## We ignore this option. This parameter exists in Matlab to
        ## speed up the reading of multipage TIFF.  It makes no difference
        ## for us since we're already quite efficient.

      otherwise
        error ("imread: invalid PARAMETER `%s'", varargin{idx});

    endswitch
  endfor

  try
    [varargout{1:nargout}] = __magick_read__ (fn, options);

  catch
    ## If we can't read it with Magick, maybe the image is in Octave's
    ## native image format.  This is from back before Octave had 'imread'
    ## and 'imwrite'. Then we had the functions 'loadimage' and 'saveimage'.
    ##
    ## This "image format" seems to be any file that can be read with
    ## load() and contains 2 variables.  The variable named "map" is a
    ## colormap and must exist whether the image is indexed or not. The
    ## other variable must be named "img" or "X" for a "normal" or
    ## indexed image.
    ##
    ## FIXME: this has been deprecated for the next major release (3.8 or 4.0).
    ##        If someone wants to revive this as yet another image format, a
    ##        separate Octave package can be written for it, that register the
    ##        format through imformats.

    magick_error = lasterr ();

    img_field = false;
    x_field   = false;
    map_field = false;

    try
      vars = load (fn);
      if (isstruct (vars))
        img_field = isfield (vars, "img");
        x_field   = isfield (vars, "X");
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
      persistent warned = false;
      if (! warned)
        warning ("Octave's native image format has been deprecated.");
        warned = true;
      endif
    else
      error ("imread: invalid Octave image file format");
    endif

  end_try_catch

endfunction

## Tests if the value passed to the Index or Frames is valid. This option
## can be defined in two places, but only in one place can it also be the
## string "all"
function bool = is_valid_index_option (arg)
  ## is the index option
  bool = false;
  if (isvector (arg) && isnumeric (arg) && isreal (arg))
    bool = true;
  endif
endfunction
