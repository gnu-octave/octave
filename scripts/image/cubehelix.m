########################################################################
##
## Copyright (C) 2014-2024 The Octave Project Developers
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
## @deftypefn  {} {@var{map} =} cubehelix ()
## @deftypefnx {} {@var{map} =} cubehelix (@var{n})
## @deftypefnx {} {@var{map} =} cubehelix (@var{n}, @var{start}, @var{rots}, @var{hue}, @var{gamma})
## Create cubehelix colormap.
##
## This colormap varies from black to white going though blue, green, and red
## tones while maintaining a monotonically increasing perception of intensity.
## This is achieved by traversing a color cube from black to white through
## a helix, hence the name cubehelix, while taking into account the perceived
## brightness of each channel according to the NTSC specifications from 1953.
##
## @example
## rgbplot (cubehelix (256))
## @end example
##
## The argument @var{n} must be a scalar.
## If @var{n} is not specified the length of the current colormap is used.  If
## there is no current colormap the default value of 256 is used.
##
## Reference: Green, D. A., 2011,
## @cite{A @nospell{colour} scheme for the display of astronomical intensity
## images}, Bulletin of the Astronomical Society of India, 39, 289.
##
## @seealso{colormap}
## @end deftypefn

function map = cubehelix (n, start = 0.5, rots = -1.5, hue = 1, gamma = 1)

  if (nargin > 0)
    if (! isscalar (n))
      error ("cubehelix: N must be a scalar");
    endif
    n = double (n);
  else
    hf = get (0, "currentfigure");
    if (! isempty (hf))
      n = rows (get (hf, "colormap"));
    else
      n = 256;
    endif
  endif

  if (n > 1)
    coeff = [ -0.14861  -0.29227   1.97294
               1.78277  -0.90649   0.00000];

    fract = ((0:n-1) / (n-1))';
    angle = 2 * pi * (start/3 + 1 + rots*fract);
    fract .^= gamma;
    amp   = hue * fract .* (1-fract) /2;
    map   = fract + amp .* ([cos(angle) sin(angle)] * coeff);

    ## Clip values (only in case users have changed values of hue or gamma)
    map(map < 0) = 0;
    map(map > 1) = 1;
  elseif (n > 0)
    map = [0, 0, 0];
  else
    map = zeros (0, 3);
  endif

endfunction


## A better demo of this colormap would be a 3-D plot in NTSC instead of
## RGB values.  That would really show what this colormap is about.
%!demo
%! ## Show the 'cubehelix' colormap profile and as an image
%! cmap = cubehelix (256);
%! subplot (2, 1, 1);
%!  rgbplot (cmap, "composite");
%! subplot (2, 1, 2);
%!  rgbplot (cmap);


%!assert (size (cubehelix ()), [256, 3])
%!assert (size (cubehelix (16)), [16, 3])

%!assert (cubehelix (1), [0,0,0])
%!assert (cubehelix (true), double ([0,0,0]))
%!assert (cubehelix (char (1)), double ([0,0,0]))
%!assert (cubehelix (int32 (1)), double ([0,0,0]))

%!assert (cubehelix (0), zeros (0, 3))
%!assert (cubehelix (-1), zeros (0, 3))

%!assert (cubehelix (2), [0, 0, 0; 1, 1, 1])
%!assert ([cubehelix]([1,end],:), [0, 0, 0; 1, 1, 1])

%!shared a
%! a = [0,                   0,                   0;
%!      0.17004232121057959, 0.43679759647517287, 0.22372555555555556;
%!      0.82995767878942041, 0.56320240352482713, 0.77627444444444445;
%!      1,                   1,                   1];

%!assert (cubehelix (4), a, eps)

%!shared  # Clear shared varibles to avoid echo on unrelated test errors.

## Input validation
%!error <function called with too many inputs> cubehelix (1, 2, 3, 4 ,5 ,6)
%!error <N must be a scalar> cubehelix ("foo")
%!error <N must be a scalar> cubehelix ([1, 2, 3])
%!error <N must be a scalar> cubehelix ({1, 2, 3})
