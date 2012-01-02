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
## @deftypefn {Function File} {} ntsc2rgb (@var{yiq})
## Transform a colormap or image from NTSC to RGB.
## @seealso{rgb2ntsc}
## @end deftypefn

## Author: Tony Richardson <arichard@stark.cc.oh.us>
## Created: July 1994
## Adapted-By: jwe

function rgb = ntsc2rgb (yiq)

  if (nargin != 1)
    print_usage ();
  endif

  ## If we have an image convert it into a color map.
  if (ismatrix (yiq) && ndims (yiq) == 3)
    is_image = true;
    Sz = size (yiq);
    yiq = [yiq(:,:,1)(:), yiq(:,:,2)(:), yiq(:,:,3)(:)];
    ## Convert to a double image.
    if (isinteger (yiq))
      C = class (yiq);
      low = double (intmin (C));
      high = double (intmax (C));
      yiq = (double (yiq) - low) / (high - low);
    endif
  else
    is_image = false;
  endif

  if (! ismatrix (yiq) || columns (yiq) != 3)
    error ("ntsc2rgb: argument must be a matrix of size Nx3 or NxMx3");
  endif

  ## Convert data
  trans = [ 1.0,      1.0,      1.0;
            0.95617, -0.27269, -1.10374;
            0.62143, -0.64681, 1.70062 ];

  rgb = yiq * trans;

  ## If input was an image, convert it back into one.
  if (is_image)
    rgb = reshape (rgb, Sz);
  endif

endfunction
