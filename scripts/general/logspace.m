########################################################################
##
## Copyright (C) 1993-2020 The Octave Project Developers
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
## @deftypefn  {} {} logspace (@var{a}, @var{b})
## @deftypefnx {} {} logspace (@var{a}, @var{b}, @var{n})
## @deftypefnx {} {} logspace (@var{a}, pi, @var{n})
## Return a row vector with @var{n} elements logarithmically spaced from
## @tex
## $10^{a}$ to $10^{b}$.
## @end tex
## @ifnottex
## 10^@var{a} to 10^@var{b}.
## @end ifnottex
##
## If @var{n} is unspecified it defaults to 50.
##
## If @var{b} is equal to
## @tex
## $\pi$,
## @end tex
## @ifnottex
## pi,
## @end ifnottex
## the points are between
## @tex
## $10^{a}$ and $\pi$,
## @end tex
## @ifnottex
## 10^@var{a} and pi,
## @end ifnottex
## @emph{not}
## @tex
## $10^{a}$ and $10^{\pi}$,
## @end tex
## @ifnottex
## 10^@var{a} and 10^pi,
## @end ifnottex
## in order to be compatible with the corresponding @sc{matlab} function.
##
## Also for compatibility with @sc{matlab}, return the right-hand side of
## the range
## @tex
## ($10^{b}$)
## @end tex
## @ifnottex
## (10^@var{b})
## @end ifnottex
## when just a single value is requested.
## @seealso{linspace}
## @end deftypefn

function retval = logspace (a, b, n = 50)

  if (nargin < 2)
    print_usage ();
  endif

  if (! (isscalar (a) && isscalar (b) && isscalar (n)))
    error ("logspace: arguments A, B, and N must be scalars");
  endif

  npoints = fix (n);

  if (b == pi)
    b = log10 (pi);
  endif

  retval = 10 .^ (linspace (a, b, npoints));

endfunction


%!test
%! x1 = logspace (1, 2);
%! x2 = logspace (1, 2, 10.1);
%! x3 = logspace (1, -2, 10);
%! x4 = logspace (1, pi, 10);
%! assert (size (x1) == [1, 50] && abs (x1(1) - 10) < eps && abs (x1(50) - 100) < eps);
%! assert (size (x2) == [1, 10] && abs (x2(1) - 10) < eps && abs (x2(10) - 100) < eps);
%! assert (size (x3) == [1, 10] && abs (x3(1) - 10) < eps && abs (x3(10) - 0.01) < eps);
%! assert (size (x4) == [1, 10] && abs (x4(1) - 10) < eps && abs (x4(10) - pi) < sqrt (eps));

## Edge cases
%!assert (logspace (Inf, Inf, 3), [Inf, Inf, Inf])
%!assert (logspace (-Inf, Inf, 3), [0, 1, Inf])
%!assert (logspace (Inf + 1i, Inf + 1i, 3), repmat (complex (-Inf,Inf), [1, 3]))
%!assert (logspace (-Inf + 1i, Inf + 1i, 3), [0, NaN + NaN * 1i, complex(-Inf, Inf)])
%!assert (logspace (0, Inf, 3), [1, Inf, Inf])
%!assert (logspace (0, -Inf, 3), [1, 0, 0])
%!assert (logspace (Inf, -Inf, 3), [Inf, 1, 0])

## FIXME: These are bizarre corner cases for Matlab compatibility.  See
## bug #56933.  This is marked as "Won't Fix", but if linspace is updated at
## some point then these tests can be re-instated.
##%!assert (logspace (-Inf, 0, 3), [0, NaN, 1])
##%!assert (logspace (Inf, 0, 3), [Inf, NaN, 1])

## Test input validation
%!error logspace ()
%!error logspace (1, 2, 3, 4)
%!error logspace ([1, 2; 3, 4], 5, 6)
%!error logspace (1, [1, 2; 3, 4], 6)
%!error logspace (1, 2, [1, 2; 3, 4])
