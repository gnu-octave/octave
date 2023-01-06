########################################################################
##
## Copyright (C) 1995-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{y} =} lin2mu (@var{x})
## @deftypefnx {} {@var{y} =} lin2mu (@var{x}, @var{n})
## Convert audio data from linear to mu-law.
##
## Linear values use floating point values in the range -1 @leq{} @var{x}
## @leq{} 1 if @var{n} is 0 (default), or @var{n}-bit signed integers if @var{n}
## is 8 or 16.  Mu-law values are 8-bit unsigned integers in the range
## 0 @leq{} @var{y} @leq{} 255.
## @seealso{mu2lin}
## @end deftypefn

function y = lin2mu (x, n = 0)

  if (nargin < 1)
    print_usage ();
  endif

  ## Convert to floating point integers per Matlab.
  x = double (x);

  if (nargin == 2)
    if (! isscalar (n) && ! isreal (n)
        || (n != 0 && n != 8 && n != 16))
      error ("lin2mu: N must be either 0, 8, or 16");
    elseif (isempty (n))
      n = 0;
    endif
  endif

  ## Transform real and n-bit format to 16-bit.
  if (n == 0)
    ## [-1,1] -> [-32768, 32768]
    x *= 32768;
  elseif (n != 16)
    x *= 256;
  endif

  ## Determine sign of x, set sign(0) = 1.
  sig = sign (x) + (x == 0);

  ## Take absolute value of x, but force it to be smaller than 32636;
  ## add bias.
  x = min (abs (x), 32635) + 132;

  ## Find exponent and fraction of binary representation.
  [f, e] = log2 (x);

  y = 64 * sig - 16 * e - fix (32 * f) + 335;

endfunction


## Test functionality
%!test
%! x = -1:1;
%! y = x';
%! assert (lin2mu (x), (lin2mu (y))');
%! assert (lin2mu (x), [0, 255, 128]);

%!assert (lin2mu ([0, 1, NaN, inf, -inf], 8), [255, 231, NaN, 128, 0])
%!assert (lin2mu ([]), [])
%!assert (lin2mu (0), 255)
%!assert (lin2mu (0, 0), 255)
%!assert (lin2mu (0, 8), 255)
%!assert (lin2mu (0, 16), 255)
%!assert (lin2mu (2, 8), 219)
%!assert (lin2mu (3, []), 128)
%!assert (lin2mu (3, 16), 255)
%!assert (lin2mu (repmat (-0.23, 1, 1000), 0), repmat (34, 1, 1000))
%!assert (lin2mu (ones (2, 2), 0), repmat (128, 2))

## Test input validation
%!error <Invalid call> lin2mu ()
%!error <N must be either 0, 8, or 16> lin2mu (1, 2)
%!error <N must be either 0, 8, or 16> lin2mu (1, [1,2])
%!error <N must be either 0, 8, or 16> lin2mu (1, ones (1, 2))
%!error <invalid conversion> lin2mu ({2:5})
