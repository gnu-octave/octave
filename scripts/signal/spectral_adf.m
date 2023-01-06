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
## @deftypefn  {} {@var{sde} =} spectral_adf (@var{c})
## @deftypefnx {} {@var{sde} =} spectral_adf (@var{c}, @var{win})
## @deftypefnx {} {@var{sde} =} spectral_adf (@var{c}, @var{win}, @var{b})
## Return the spectral density estimator given a vector of autocovariances
## @var{c}, window name @var{win}, and bandwidth, @var{b}.
##
## The window name, e.g., @qcode{"triangle"} or @qcode{"rectangle"} is
## used to search for a function called @code{@var{win}_lw}.
##
## If @var{win} is omitted, the triangle window is used.
##
## If @var{b} is omitted, @code{1 / sqrt (length (@var{x}))} is used.
## @seealso{spectral_xdf}
## @end deftypefn

function sde = spectral_adf (c, win, b)

  if (nargin < 1)
    print_usage ();
  endif

  cr = length (c);

  if (columns (c) > 1)
    c = c';
  endif

  if (nargin < 3)
    b = 1 / ceil (sqrt (cr));
  endif

  if (nargin == 1)
    w = triangle_lw (cr, b);
  elseif (! ischar (win))
    error ("spectral_adf: WIN must be a string");
  else
    win = str2func ([win "_lw"]);
    w = feval (win, cr, b);
  endif

  c .*= w;

  sde = 2 * real (fft (c)) - c(1);
  sde = [(zeros (cr, 1)), sde];
  sde(:, 1) = (0 : cr-1)' / cr;

endfunction


## Test input validation
%!error <Invalid call> spectral_adf ()
%!error <WIN must be a string> spectral_adf (1, 2)
%!error <unable to find function for @invalid_lw> spectral_adf (1, "invalid")
