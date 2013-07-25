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

## This function does all the work of imwrite. It exists here as private
## function so that imwrite can use other functions if imformats is
## configured to. It is also needed so that imformats can create a
## function handle for it.

function __imwrite__ (img, varargin)

  if (nargin < 2 || ! (isnumeric (img) || islogical (img)))
    print_usage ("imwrite");
  endif

  [filename, ext, map, param_list] = imwrite_filename (varargin{:});

  if (rem (numel (param_list), 2) != 0)
    error ("imwrite: no pair for all arguments (even number left)");
  endif

  ## set default for options
  options        = struct ("writemode", "overwrite",
                           "quality",   75);

  for idx = 1:2:numel (param_list)

    switch (tolower (param_list{idx}))

      case "writemode",
        options.writemode = param_list{idx+1};
        if (! ischar (options.writemode) ||
            ! any (strcmpi (options.writemode, {"append", "overwrite"})))
          error ("imwrite: value for %s option must be \"append\" or \"overwrite\"",
                 param_list{idx});
        endif
        options.writemode = tolower (options.writemode);

      case "quality",
        options.quality = param_list{idx+1};
        if (! isnumeric (options.quality) || ! isscalar (options.quality) ||
            options.quality < 0 || options.quality > 100)
          error ("imwrite: value for %s option must be a scalar between 0 and 100",
                 param_list{idx});
        endif
        options.quality = round (options.quality);

      otherwise
        error ("imwrite: invalid PARAMETER `%s'", varargin{idx});

    endswitch
  endfor

  if (isempty (img))
    error ("imwrite: invalid empty image");
  elseif (issparse (img) || issparse (map))
    error ("imwrite: sparse images not supported");
  endif

  if (! isempty (map))
    if (! iscolormap (map))
      error ("imwrite: invalid MAP for indexed image");
    elseif (ndims (img) != 2 && ndims (img) != 4)
      error ("imwrite: indexed image must have 2 or 4 dimensions (found %i)", ndims (img));
    endif
    ## FIXME: we should really be writing indexed images but that needs
    ##        to be implemented in  __magick_write__(). So we convert
    ##        them to RGB and write them "normally".
    warned = false;
    if (! warned)
      warning ("imwrite: saving of indexed images is not yet implemented. Will save a RGB image.");
      warned = true;
    endif
    img = ind2rgb (img, map);
    map = [];
  endif

  if (ndims (img) > 4)
    error ("imwrite: invalid %d-dimensional image data", ndims (img));
  elseif (all (size (img, 3) != [1 3]))
    ## This test needs to be adjusted if one day we implement alternative
    ## colorspaces. In the mean time, we only have grayscale and RGB images,
    ## but CMYK means length 4 in the 3rd dimension.
    error ("imwrite: IMG 3rd dimension must be 1 or 3");
  endif

  ## FIXME: do we need to convert the image class?
  __magick_write__ (filename, ext, img, map, options);

endfunction
