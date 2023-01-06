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
## @deftypefn {} {@var{n} =} nextpow2 (@var{x})
## Compute the exponent for the smallest power of two larger than the input.
##
## For each element in the input array @var{x}, return the first integer
## @var{n} such that
## @tex
## $2^n \ge |x|$.
## @end tex
## @ifnottex
## 2^n @geq{} abs (x).
## @end ifnottex
##
## @seealso{pow2, log2}
## @end deftypefn

function n = nextpow2 (x)

  if (nargin < 1)
    print_usage ();
  endif

  if (! isnumeric (x))
    error ("nextpow2: X must be numeric");
  endif

  n = ceil (log2 (abs (x)));
  n(x == 0) = 0;  # special case

endfunction


%!assert (nextpow2 (16), 4)
%!assert (nextpow2 (17), 5)
%!assert (nextpow2 (31), 5)
%!assert (nextpow2 (-16), 4)
%!assert (nextpow2 (-17), 5)
%!assert (nextpow2 (-31), 5)
%!assert (nextpow2 (1:17), [0 1 2 2 3 3 3 3 4 4 4 4 4 4 4 4 5])
%!assert <*62947> (nextpow2 (0.5), -1)
%!assert <*62947> (nextpow2 (0.6), 0)
## Special cases
%!assert (nextpow2 (0), 0)
%!assert (nextpow2 (1), 0)
%!assert (nextpow2 (Inf), Inf)
%!assert (nextpow2 (-Inf), Inf)
%!assert (nextpow2 (NaN), NaN)
%!assert (nextpow2 ([1, Inf, 3, -Inf, 9, NaN]), [0, Inf, 2, Inf, 4, NaN])

## Test input validation
%!error <Invalid call> nextpow2 ()
%!error <X must be numeric> nextpow2 ("t")
