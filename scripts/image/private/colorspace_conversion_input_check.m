########################################################################
##
## Copyright (C) 2016-2023 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.
##
########################################################################

## Private function the functions that convert between color spaces, i.e.,
## rgb2ntsc, rgb2hsv, hsv2rgb, and ntsc2rgb.  All of these functions need to
## handle input in the same way.  The returned flags are meant to be handled
## by the complementary private function colorspace_conversion_revert()

function [in_arg, sz, is_im, is_nd] ...
            = colorspace_conversion_input_check (fcn, arg_name, in_arg)

  cls = class (in_arg);
  sz = size (in_arg);

  ## If we have an image convert it into a color map.
  if (! iscolormap (in_arg))
    if (! any (strcmp (cls, {"uint8", "int8", "int16", "uint16", ...
                             "single", "double"})))
      error ("%s: %s of invalid data type '%s'", fcn, arg_name, cls);
    elseif (size (in_arg, 3) != 3)
      error ("%s: %s must be a colormap or %s image", fcn, arg_name, arg_name);
    elseif (! isreal (in_arg) || ! isnumeric (in_arg))
      error ("%s: %s must be numeric and real", fcn, arg_name);
    endif
    is_im = true;

    ## For floating point values, R, G and B should be in the [0 1] range,
    ## otherwise they don't make any sense.  We accept those values
    ## anyways because we must return something for Matlab compatibility.
    ## User case is when a function returns an RGB image just slightly outside
    ## the range due to floating point rounding errors.

    ## Allow for ND images, i.e., multiple images on the 4th dimension.
    nd = ndims (in_arg);
    if (nd == 3)
      is_nd = false;
    elseif (nd == 4)
      is_nd = true;
      in_arg = permute (in_arg, [1 2 4 3]);
    elseif (nd > 4)
      error ("%s: invalid %s with more than 4 dimensions", fcn, arg_name);
    endif
    in_arg = reshape (in_arg, [numel(in_arg)/3 3]);
  else
    is_im = false;
    is_nd = false;
  endif

  ## Convert to floating point (remember to leave class single alone)
  if (isinteger (in_arg))
    int_max = double (intmax (cls));
    int_min = double (intmin (cls));
    if (int_min < 0)
      in_arg = (double (in_arg) - int_min) / (int_max - int_min);
    else
      in_arg = double (in_arg) / int_max;
    endif
  endif

endfunction
