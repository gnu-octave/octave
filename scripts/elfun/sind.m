########################################################################
##
## Copyright (C) 2006-2023 The Octave Project Developers
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
## @deftypefn {} {@var{y} =} sind (@var{x})
## Compute the sine for each element of @var{x} in degrees.
##
## The function is more accurate than @code{sin} for large values of @var{x}
## and for multiples of 180 degrees (@code{@var{x}/180} is an integer) where
## @code{sind} returns 0 rather than a small value on the order of eps.
## @seealso{asind, sin}
## @end deftypefn

function y = sind (x)

  if (nargin < 1)
    print_usage ();
  endif

  if (! isnumeric (x))
    error ("sind: X must be numeric");
  endif

  x_iscomplex = iscomplex (x);
  if (x_iscomplex)
    xi = imag (x);
  endif
  x = real (x);

  ## Wrap multiples so that new domain is [-180, 180)
  x = mod (x-180, 360) - 180;

  if (x_iscomplex)
    y = sin (complex (x, xi) / 180 * pi);
    ## Integer multiples of pi must be exactly zero
    y(x == -180) = complex (0, imag (y(x == -180)));
  else
    y = sin (x / 180 * pi);
    ## Integer multiples of pi must be exactly zero
    y(x == -180) = 0;
  endif

endfunction


%!assert (sind (10:20:360), sin ([10:20:360] * pi/180), 5*eps)
%!assert (sind ([-360, -180, 0, 180, 360]) == 0)
%!assert (sind ([-270, -90, 90, 270]), [1, -1, 1, -1])
%!assert (sind ([-Inf, NaN, +Inf, 0]), [NaN, NaN, NaN, 0])
%!assert (sind (+23) == -sind (-23))
%!assert (sind (1e6), -0.984807753012208, 5*eps)
%!assert (sind (180 + 180i), -i*sinh (pi))
%!assert (sind (1e6 + 180i), -11.415845458288851 + 2.0054175437381652i, 5*eps)

%!error <Invalid call> sind ()
%!error <X must be numeric> sind ("abc")
