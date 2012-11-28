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
## @deftypefn  {Function File} {@var{I} =} ind2gray (@var{x})
## @deftypefnx {Function File} {@var{I} =} ind2gray (@var{x}, @var{map})
## Convert a color indexed image to a grayscale intensity image.
## If @var{map} is omitted, the current colormap is used to determine the
## intensities.
##
## The output @var{I} is of the same class as the input @var{x} and may be
## one of @code{uint8}, @code{uint16}, @code{single}, or @code{double}.
##
## Implementation Note: There are several ways of converting colors to
## grayscale intensities.  This functions uses the luminance value obtained
## from @code{rgb2ntsc} which is @code{I = 0.299*R + 0.587*G + 0.114*B}.  
## Other possibilities include the value component from @code{rgb2hsv} or
## using a single color channel from @code{ind2rgb}.
## @seealso{gray2ind, ind2rgb}
## @end deftypefn

## Author: Tony Richardson <arichard@stark.cc.oh.us>
## Created: July 1994
## Adapted-By: jwe

function I = ind2gray (x, map)

  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif

  if (! isreal (x) || issparse (x)
      || (isfloat (x) && (any (x(:) < 1 || any (x(:) != fix (x(:)))))))
    error ("ind2gray: X must be an indexed image");
  endif
  cls = class (x);
  if (! any (isa (x, {"logical", "uint8", "uint16", "single", "double"})))
    error ("ind2gray: invalid data type '%s'", cls);
  endif

  if (nargin == 1)
    map = colormap ();
  elseif (! iscolormap (map))
    error ("ind2gray: MAP must be a valid colormap");
  endif

  ## Convert colormap to luminance intensity values 
  map *= [0.299; 0.587; 0.114];

  ## Convert colormap to same class as that of input so that
  ## indexing in colormap will produce output of the same type as the input.
  if (isinteger (x)) 
    map *= intmax (cls);
  elseif (strcmp (cls, "single"))
    map = single (map);
  endif

  ## Replace indices in the input matrix with the indexed luminance value.
  I = reshape (map(x(:)), size (x));

endfunction


%!test
%! i2g = ind2gray (1:100, gray (100));
%! assert (i2g, 0:1/99:1, eps);
%! g2i = gray2ind (i2g, 100);
%! assert (g2i, uint8 (1:100));

%%test input validation
%!error ind2gray ()
%!error ind2gray (1,2,3)
%!error <X must be an indexed image> ind2gray ({1})
%!error <X must be an indexed image> ind2gray (1+i)
%!error <X must be an indexed image> ind2gray (sparse (1))
%!error <X must be an indexed image> ind2gray (0)
%!error <X must be an indexed image> ind2gray (1.1)
%!error <MAP must be a valid colormap> ind2gray (1, {1})
%!error <MAP must be a valid colormap> ind2gray (1, 1+i)
%!error <MAP must be a valid colormap> ind2gray (1, ones (2,2,2))
%!error <MAP must be a valid colormap> ind2gray (1, ones (2,4))
%!error <MAP must be a valid colormap> ind2gray (1, [-1])
%!error <MAP must be a valid colormap> ind2gray (1, [2])

