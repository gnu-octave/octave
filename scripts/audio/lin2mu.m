########################################################################
##
## Copyright (C) 1995-2022 The Octave Project Developers
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
## @deftypefn {} {@var{y} =} lin2mu (@var{x}, @var{n})
## Convert audio data from linear to mu-law.
##
## Mu-law values use 8-bit unsigned integers.  Linear values use @var{n}-bit
## signed integers or floating point values in the range -1 @leq{} @var{x}
## @leq{} 1 if @var{n} is 0.
##
## If @var{n} is not specified it defaults to 0, 8, or 16 depending on
## the range of values in @var{x}.
## @seealso{mu2lin}
## @end deftypefn


function y = lin2mu (x, n)

  if (nargin < 1)
    print_usage ();
  endif

  if (nargin == 1)
    range = max (abs (x (:)));
    if (range <= 1)
      n = 0;
    elseif (range <= 128)
      n = 8;
      warning ("lin2mu: no precision specified, so using %d", n);
    else
      n = 16;
    endif
  else
    if (n != 0 && n != 8 && n != 16)
      error ("lin2mu: N must be either 0, 8 or 16");
    endif
  endif

  ## Transform real and n-bit format to 16-bit.
  if (n == 0)
    ## [-1,1] -> [-32768, 32768]
    x = 32768 * x;
  elseif (n != 16)
    x = 2^(16-n) .* x;
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
%! assert (lin2mu (x), (lin2mu (y))')
%! assert (lin2mu (x), [0, 255, 128])

%!assert (lin2mu ([0, 1, NaN, inf, -inf], 8), [255, 231, NaN, 128, 0])
%!assert (lin2mu ([]), [])
%!assert (lin2mu (0), 255)
%!assert (lin2mu (0, 0), 255)
%!assert (lin2mu (0, 8), 255)
%!assert (lin2mu (0, 16), 255)
%!assert (lin2mu (2, 8), 219)
%!assert (lin2mu (3, []), 255)
%!assert (lin2mu (3, 16), 255)
%!assert (lin2mu (repmat (-0.23, 1, 1000), 0), repmat (34, 1, 1000))
%!assert (lin2mu (ones (2, 2), 0), repmat (128, 2))

## Test input validation
%!error <Invalid call> lin2mu ()
%!warning <no precision specified, so using 8> assert (lin2mu (2), 219)
%!error <N must be either 0, 8 or 16> lin2mu (1, 2)
%!error <N must be either 0, 8 or 16> lin2mu (1, [1,2])
%!error <N must be either 0, 8 or 16> lin2mu (1, ones (1, 2))
%!error <abs: not defined for cell> lin2mu ({2:5})
