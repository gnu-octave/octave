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

## -*- texinfo -*-
## @deftypefn {} {@var{rv} =} colorspace_conversion_revert (@var{rv}, @var{sz}, @var{is_im}, @var{is_nd})
##
## Private function that converts between color spaces.
##
## This reverts a colormap type into the same shape and class as it was in the
## input.  The flags are meant to come from complementary private function
## @code{colorspace_conversion_input_check}.
## @seealso{hsv2rgb, rgb2grap, rgb2hsv, colorspace_conversion_input_check}
## @end deftypefn

function rv = colorspace_conversion_revert (rv, sz, is_im, is_nd)

  if (is_im)
    if (is_nd)
      rv = reshape (rv, [sz(1:2) sz(4) sz(3)]);
      rv = permute (rv, [1 2 4 3]);
    else
      rv = reshape (rv, sz);
    endif
  endif

endfunction
