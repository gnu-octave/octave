########################################################################
##
## Copyright (C) 1995-2024 The Octave Project Developers
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
## Compute the exponent of the next power of two not smaller than the input.
##
## For each element in the input array @var{x}, return the smallest integer
## @var{n} such that
## @tex
## $2^n \ge |x|$.
## @end tex
## @ifnottex
## @code{2^@var{n} @geq{} abs (@var{x})}.
## @end ifnottex
## For input elements equal to zero, return zero.
##
## @seealso{pow2, log2}
## @end deftypefn

function n = nextpow2 (x)

  if (nargin < 1)
    print_usage ();
  endif

  if (! isnumeric (x))
    error ("nextpow2: X must be numeric");
  elseif (! isreal (x))
    error ("nextpow2: X must be real");
  endif

  [f, n] = log2 (abs (x));
  n(f == 0.5)--;

  if (isfloat (x))
    idx_nan = isnan (x);
    n(idx_nan) = x(idx_nan);
    n(isinf (x)) = Inf;
  else
    n = cast (n, class (x));
    if (intmax (x) > flintmax ())
      n((2 .^ n) < abs (x))++;
    endif
  endif

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
%!assert (nextpow2 (NA), NA)
%!assert (nextpow2 ([1, Inf, 3, -Inf, 9, NaN, NA]), ...
%!                  [0, Inf, 2,  Inf, 4, NaN, NA])

%!test
%! p = (-1074:1023).';
%! x = 2 .^ p;
%! x = [x, x + eps(x)];
%! x = [x, -x];
%! n = nextpow2 (x);
%! assert (n(:, 1), p);
%! assert (n(:, 3), p);
%! assert (n(:, 2), p + 1);
%! assert (n(:, 4), p + 1);

%!assert (nextpow2 (realmax ()), 1024)
%!assert (nextpow2 (-realmax ()), 1024)

%!test
%! p = single (-149:127).';
%! x = 2 .^ p;
%! x = [x, x + eps(x)];
%! x = [x, -x];
%! n = nextpow2 (x);
%! assert (n(:, 1), p);
%! assert (n(:, 3), p);
%! assert (n(:, 2), p + 1);
%! assert (n(:, 4), p + 1);

%!assert (nextpow2 (realmax ('single')), single (128))
%!assert (nextpow2 (-realmax ('single')), single (128))

%!test
%! p = int32 (0:30).';
%! x = 2 .^ p;
%! x = [x, x + 1];
%! x = [x, -x];
%! n = nextpow2 (x);
%! assert (n(:, 1), p);
%! assert (n(:, 3), p);
%! assert (n(:, 2), p + 1);
%! assert (n(:, 4), p + 1);

%!assert (nextpow2 (int32 (0)), int32 (0))
%!assert (nextpow2 (intmin ('int32')), int32 (31))
%!assert (nextpow2 (intmax ('int32')), int32 (31))

%!assert (nextpow2 (uint32 (0)), uint32 (0))
%!assert (nextpow2 (intmax ('uint32')), uint32 (32))

%!test
%! p = int64 (0:62).';
%! x = 2 .^ p;
%! x = [x, x + 1];
%! x = [x, -x];
%! n = nextpow2 (x);
%! assert (n(:, 1), p);
%! assert (n(:, 3), p);
%! assert (n(:, 2), p + 1);
%! assert (n(:, 4), p + 1);

%!assert (nextpow2 (int64 (0)), int64 (0))
%!assert (nextpow2 (intmin ('int64')), int64 (63))
%!assert (nextpow2 (intmax ('int64')), int64 (63))

%!assert (nextpow2 (uint64 (0)), uint64 (0))
%!assert (nextpow2 (intmax ('uint64')), uint64 (64))

## Test input validation
%!error <Invalid call> nextpow2 ()
%!error <X must be numeric> nextpow2 ("t")
%!error <X must be real> nextpow2 (1 + 2i)
