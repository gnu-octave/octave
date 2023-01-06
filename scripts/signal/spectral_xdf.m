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
## @deftypefn  {} {@var{sde} =} spectral_xdf (@var{x})
## @deftypefnx {} {@var{sde} =} spectral_xdf (@var{x}, @var{win})
## @deftypefnx {} {@var{sde} =} spectral_xdf (@var{x}, @var{win}, @var{b})
## Return the spectral density estimator given a data vector @var{x}, window
## name @var{win}, and bandwidth, @var{b}.
##
## The window name, e.g., @qcode{"triangle"} or @qcode{"rectangle"} is used to
## search for a function called @code{@var{win}_sw}.
##
## If @var{win} is omitted, the triangle window is used.
##
## If @var{b} is omitted, @code{1 / sqrt (length (@var{x}))} is used.
## @seealso{spectral_adf}
## @end deftypefn

function sde = spectral_xdf (x, win, b)

  if (nargin < 1)
    print_usage ();
  endif

  xr = length (x);

  if (columns (x) > 1)
    x = x';
  endif

  if (nargin < 3)
    b = 1 / ceil (sqrt (xr));
  endif

  if (nargin == 1)
    w = triangle_sw (xr, b);
  elseif (! ischar (win))
    error ("spectral_xdf: WIN must be a string");
  else
    win = str2func ([win "_sw"]);
    w = feval (win, xr, b);
  endif

  x -= sum (x) / xr;

  sde = (abs (fft (x)) / xr).^2;
  sde = real (ifft (fft (sde) .* fft (w)));

  sde = [(zeros (xr, 1)), sde];
  sde(:, 1) = (0 : xr-1)' / xr;

endfunction


## Test input validation
%!error <Invalid call> spectral_xdf ()
%!error <WIN must be a string> spectral_xdf (1, 2)
%!error <unable to find function for @invalid_sw> spectral_xdf (1, "invalid")
