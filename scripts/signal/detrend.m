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
## @deftypefn {} {@var{y} =} detrend (@var{x}, @var{p})
## If @var{x} is a vector, @code{detrend (@var{x}, @var{p})} removes the
## best fit of a polynomial of order @var{p} from the data @var{x}.
##
## If @var{x} is a matrix, @code{detrend (@var{x}, @var{p})} does the same
## for each column in @var{x}.
##
## The second argument @var{p} is optional.  If it is not specified, a value of
## 1 is assumed.  This corresponds to removing a linear trend.
##
## The order of the polynomial can also be given as a string, in which case
## @var{p} must be either @qcode{"constant"} (corresponds to @code{@var{p}=0})
## or @qcode{"linear"} (corresponds to @code{@var{p}=1}).
## @seealso{polyfit}
## @end deftypefn

function y = detrend (x, p = 1)

  if (nargin < 1)
    print_usage ();
  endif

  if (! isnumeric (x) || ndims (x) > 2)
    error ("detrend: X must be a numeric vector or matrix");
  endif

  if (ischar (p) && strcmpi (p, "constant"))
    p = 0;
  elseif (ischar (p) && strcmpi (p, "linear"))
    p = 1;
  elseif (! isscalar (p) || p < 0 || p != fix (p))
    error ('detrend: P must be "constant", "linear", or a positive integer');
  endif

  [m, n] = size (x);
  if (m == 1)
    x = x.';
  endif

  r = rows (x);
  b = ((1 : r).' * ones (1, p + 1)) .^ (ones (r, 1) * (0 : p));
  y = x - b * (b \ x);

  if (m == 1)
    y = y.';
  endif

endfunction


%!test
%! N = 32;
%! x = (0:1:N-1)/N + 2;
%! y = detrend (x);
%! assert (abs (y(:)) < 20*eps);

%!test
%! N = 32;
%! t = (0:1:N-1)/N;
%! x = t .* t + 2;
%! y = detrend (x,2);
%! assert (abs (y(:)) < 30*eps);

%!test
%! N = 32;
%! t = (0:1:N-1)/N;
%! x = [t;4*t-3].';
%! y = detrend (x);
%! assert (abs (y(:)) < 20*eps);

%!test
%! N = 32;
%! x = ((0:1:N-1)/N + 2) * 1i;
%! y = detrend (x);
%! assert (abs (y(:)) < 20*eps);

## Test input validation
%!error <Invalid call> detrend ()
%!error detrend ("a")
%!error detrend (true)
%!error detrend (1, "invalid")
%!error detrend (1, -1)
%!error detrend (1, 1.25)
