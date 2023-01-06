########################################################################
##
## Copyright (C) 2020-2023 The Octave Project Developers
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
## @deftypefn {} {@var{y} =} cospi (@var{x})
## Compute cosine (@var{x} * pi) for each element of @var{x} accurately.
##
## The ordinary @code{cos} function uses IEEE floating point numbers and may
## produce results that are very close (within a few eps) of the correct
## value, but which are not exact.  The @code{cospi} function is more accurate
## and returns 0 exactly for half-integer values of @var{x} (e.g., @dots{},
## -3/2, -1/2, 1/2, 3/2, @dots{}), and +1/-1 for integer values.
##
## Example @*
## comparison of @code{cos} and @code{cospi} for half-integer values of @var{x}
##
## @example
## @group
## cos ([-3/2, -1/2, 1/2, 3/2] * pi)
## @result{}
##      -1.8370e-16   6.1232e-17   6.1232e-17  -1.8370e-16
##
## cospi ([-3/2, -1/2, 1/2, 3/2])
## @result{}
##        0   0   0   0
## @end group
## @end example
##
## @seealso{sinpi, cos}
## @end deftypefn

function y = cospi (x)

  if (nargin < 1)
    print_usage ();
  endif

  ## Advance phase by pi/2 so that algorithm from sinpi can be used.
  ## Wrap integer multiples so that new domain is [-1, 1).
  x = mod (x - 0.5, 2) - 1;

  ## Integer multiples of pi must be exactly zero.
  x(x == -1) = 0;

  y = sin (x * pi);

endfunction


%!assert (cospi ([-3/2, -1/2, 1/2, 3/2]) == 0)
%!assert (cospi ([-2, -1, 0, 1, 2]), [1, -1, 1, -1, 1])
%!assert (cospi (100 + [0.1:0.1:0.9]), cos ([0.1:0.1:0.9]*pi), 2*eps (100))

%!error <Invalid call> cospi ()
