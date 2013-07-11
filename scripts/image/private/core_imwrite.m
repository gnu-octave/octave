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

## This function does all the work of imwrite. It exists here as private
## function so that imwrite can use other functions if imformats is
## configured to. It is also needed so that imformats can create a
## function handle for it.

function core_imwrite (img, varargin)

  if (nargin < 2 || ! (isnumeric (img) || islogical (img)))
    print_usage ("imwrite");
  endif

  [filename, ext, map, param_list] = imwrite_filename (varargin{2:end});

  if (isempty (options))
    has_param_list = false;
  else
    has_param_list = true;
    for ii = 1:2:(length (param_list))
      options.(param_list{ii}) = param_list{ii + 1};
    endfor
  endif

  if (isempty (img))
    error ("imwrite: invalid empty image");
  endif

  if (issparse (img) || issparse (map))
    error ("imwrite: sparse images not supported");
  endif

  img_class = class (img);
  map_class = class (map);
  nd = ndims (img);

  if (isempty (map))
    if (any (strcmp (img_class, {"logical", "uint8", "uint16", "double"})))
      if ((nd == 2 || nd == 3) && strcmp (img_class, "double"))
        img = uint8 (img * 255);
      endif
      ## FIXME: should we handle color images with alpha channel here?
      if (nd == 3 && size (img, 3) < 3)
        error ("imwrite: invalid dimensions for truecolor image");
      endif
      ## FIXME: why nd>5? Shouldn't it be nd>4? What's the 5th dimension for?
      if (nd > 5)
        error ("imwrite: invalid %d-dimensional image data", nd);
      endif
    else
      error ("imwrite: %s: invalid class for truecolor image", img_class);
    endif
    if (has_param_list)
      __magick_write__ (filename, ext, img, options);
    else
      __magick_write__ (filename, ext, img);
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
    if (! iscolormap (map))
      error ("imwrite: invalid indexed image colormap");
    endif

    ## FIXME: we should really be writing indexed images here but
    ##        __magick_write__ needs to be fixed to handle them.

    [r, g, b] = ind2rgb (img, map);
    tmp = uint8 (cat (3, r, g, b) * 255);

    if (has_param_list)
      __magick_write__ (filename, ext, tmp, options);
      ## __magick_write__ (filename, ext, img, map, options);
    else
      __magick_write__ (filename, ext, tmp);
      ## __magick_write__ (filename, ext, img, map);
    endif
  endif

endfunction
