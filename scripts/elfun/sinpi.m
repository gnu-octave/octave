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
## @deftypefn {} {@var{y} =} sinpi (@var{x})
## Compute sine (@var{x} * pi) for each element of @var{x} accurately.
##
## The ordinary @code{sin} function uses IEEE floating point numbers and may
## produce results that are very close (within a few eps) of the correct
## value, but which are not exact.  The @code{sinpi} function is more accurate
## and returns 0 exactly for integer values of @var{x} and +1/-1 for
## half-integer values (e.g., @dots{}, -3/2, -1/2, 1/2, 3/2, @dots{}).
##
## Example @*
## comparison of @code{sin} and @code{sinpi} for integer values of @var{x}
##
## @example
## @group
## sin ([0, 1, 2, 3] * pi)
## @result{}
##      0   1.2246e-16  -2.4493e-16   3.6739e-16
##
## sinpi ([0, 1, 2, 3])
## @result{}
##        0   0   0   0
## @end group
## @end example
##
## @seealso{cospi, sin}
## @end deftypefn

function y = sinpi (x)

  if (nargin < 1)
    print_usage ();
  endif

  ## Wrap integer multiples so that new domain is [-1, 1)
  x = mod (x-1, 2) - 1;

  ## Integer multiples of pi must be exactly zero
  x(x == -1) = 0;

  y = sin (x * pi);

endfunction


%!assert (sinpi ([-1, -2, 0, 1, 2]) == 0)
%!assert (sinpi ([-3/2, -1/2, 1/2, 3/2]), [1, -1, 1, -1])
%!assert (sinpi (100 + [0.1:0.1:0.9]), sin ([0.1:0.1:0.9]*pi), 2*eps (100))

%!error <Invalid call> sinpi ()
