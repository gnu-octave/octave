## Copyright (C) 2015 Carnë Draug <carandraug+dev@gmail.com>
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

## Private function the functions that convert between color spaces, i.e.,
## rgb2ntsc, rgb2hsv, hsv2rgb, and ntsc2rgb.  This reverts a colormap type
## into the same shape and class as it was in the input.  The flags are meant
## to come from  complementary private function
## colorspace_conversion_input_check()

function rv = colorspace_conversion_revert (rv, cls, sz, is_im, is_nd, is_int)
  if (is_im)
    if (is_nd)
      rv = reshape (rv, [sz(1:2) sz(4) sz(3)]);
      rv = permute (rv, [1 2 4 3]);
    else
      rv = reshape (rv, sz);
    endif
  endif
  if (is_int)
    rv *= intmax (cls);
  endif
endfunction
