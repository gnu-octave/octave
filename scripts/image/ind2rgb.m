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
## @deftypefn  {Function File} {@var{rgb} =} ind2rgb (@var{x})
## @deftypefnx {Function File} {@var{rgb} =} ind2rgb (@var{x}, @var{map})
## @deftypefnx {Function File} {[@var{R}, @var{G}, @var{B}] =} ind2rgb (@dots{})
## Convert an indexed image to red, green, and blue color components.
## If @var{map} is omitted, the current colormap is used for the conversion.
## When the colormap does not contain enough colors it is padded to the
## required length using the last color in the map.
##
## The output may be a single MxNx3 matrix where M is the number of rows in
## @var{x} and N is the number of columns in @var{x}.  Alternatively,
## individual red, green, and blue color matrices of size MxN may be
## returned.
## @seealso{rgb2ind, ind2gray, hsv2rgb, ntsc2rgb}
## @end deftypefn

## Author: Tony Richardson <arichard@stark.cc.oh.us>
## Created: July 1994
## Adapted-By: jwe

function [R, G, B] = ind2rgb (x, map)

  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif

  if (! isreal (x) || issparse (x)
      || (isfloat (x) && (any (x(:) < 1 || any (x(:) != fix (x(:)))))))
    error ("ind2rgb: X must be an indexed image");
  endif
  cls = class (x);
  if (! any (isa (x, {"logical", "uint8", "uint16", "single", "double"})))
    error ("ind2rgb: invalid data type '%s'", cls);
  endif

  if (nargin == 1)
    map = colormap ();
  elseif (! iscolormap (map))
    error ("ind2rgb: MAP must be a valid colormap");
  endif

  ## Do we have enough colors in the color map?
  maxidx = max (x(:));
  rm = rows (map);
  if (rm < maxidx)
    ## Pad with the last color in the map.
    pad = repmat (map(end,:), maxidx-rm, 1);
    map(end+1:maxidx, :) = pad;
  endif

  ## Compute result
  [row, col] = size (x);
  R = reshape (map(x(:), 1), row, col);
  G = reshape (map(x(:), 2), row, col);
  B = reshape (map(x(:), 3), row, col);

  ## Use 3D array if only one output is requested.
  if (nargout <= 1)
    R(:,:,2) = G;
    R(:,:,3) = B;
  endif

endfunction


%% FIXME: Need some functional tests or %!demo blocks

%%test input validation
%!error ind2rgb ()
%!error ind2rgb (1,2,3)
%!error <X must be an indexed image> ind2rgb ({1})
%!error <X must be an indexed image> ind2rgb (1+i)
%!error <X must be an indexed image> ind2rgb (sparse (1))
%!error <X must be an indexed image> ind2rgb (0)
%!error <X must be an indexed image> ind2rgb (1.1)
%!error <MAP must be a valid colormap> ind2rgb (1, {1})
%!error <MAP must be a valid colormap> ind2rgb (1, 1+i)
%!error <MAP must be a valid colormap> ind2rgb (1, ones (2,2,2))
%!error <MAP must be a valid colormap> ind2rgb (1, ones (2,4))
%!error <MAP must be a valid colormap> ind2rgb (1, [-1])
%!error <MAP must be a valid colormap> ind2rgb (1, [2])

