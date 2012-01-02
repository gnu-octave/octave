## Copyright (C) 1994-2012 John W. Eaton
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
## @deftypefn {Function File} {} rgb2ntsc (@var{rgb})
## Transform a colormap or image from RGB to NTSC.
## @seealso{ntsc2rgb}
## @end deftypefn

## Author: Tony Richardson <arichard@stark.cc.oh.us>
## Created: July 1994
## Adapted-By: jwe

function yiq = rgb2ntsc (rgb)

  if (nargin != 1)
    print_usage ();
  endif

  ## If we have an image convert it into a color map.
  if (ismatrix (rgb) && ndims (rgb) == 3)
    is_image = true;
    Sz = size (rgb);
    rgb = [rgb(:,:,1)(:), rgb(:,:,2)(:), rgb(:,:,3)(:)];
    ## Convert to a double image.
    if (isinteger (rgb))
      C = class (rgb);
      low = double (intmin (C));
      high = double (intmax (C));
      rgb = (double (rgb) - low) / (high - low);
    endif
  else
    is_image = false;
  endif

  if (! ismatrix (rgb) || columns (rgb) != 3)
    error ("rgb2ntsc: argument must be a matrix of size Nx3 or NxMx3");
  endif

  ## Convert data
  trans = [ 0.299,  0.596,  0.211;
            0.587, -0.274, -0.523;
            0.114, -0.322,  0.312 ];

  yiq = rgb * trans;

  ## If input was an image, convert it back into one.
  if (is_image)
    yiq = reshape (yiq, Sz);
  endif

endfunction
