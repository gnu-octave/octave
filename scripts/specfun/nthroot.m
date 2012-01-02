## Copyright (C) 2004-2012 Paul Kienzle
## Copyright (C) 2010 VZLU Prague
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.
##
## Original version by Paul Kienzle distributed as free software in the
## public domain.

## -*- texinfo -*-
## @deftypefn {Function File} {} nthroot (@var{x}, @var{n})
##
## Compute the n-th root of @var{x}, returning real results for real
## components of @var{x}.  For example:
##
## @example
## @group
## nthroot (-1, 3)
## @result{} -1
## (-1) ^ (1 / 3)
## @result{} 0.50000 - 0.86603i
## @end group
## @end example
##
## @var{x} must have all real entries.  @var{n} must be a scalar.
## If @var{n} is an even integer and @var{X} has negative entries, an
## error is produced.
## @seealso{realsqrt, sqrt, cbrt}
## @end deftypefn

function y = nthroot (x, n)

  if (nargin != 2)
    print_usage ();
  endif

  if (any (iscomplex (x(:))))
    error ("nthroot: X must not contain complex values");
  endif

  if (! isscalar (n) || n == 0)
    error ("nthroot: N must be a nonzero scalar");
  endif

  if (n == 3)
    y = cbrt (x);
  elseif (n == -3)
    y = 1 ./ cbrt (x);
  elseif (n < 0)
    y = 1 ./ nthroot (x, -n);
  else
    ## Compute using power.
    if (n == fix (n) && mod (n, 2) == 1)
      y = abs (x) .^ (1/n) .* sign (x);
    elseif (any (x(:) < 0))
      error ("nthroot: if X contains negative values, N must be an odd integer");
    else
      y = x .^ (1/n);
    endif

    if (finite (n) && n > 0 && n == fix (n))
      ## Correction.
      y = ((n-1)*y + x ./ (y.^(n-1))) / n;
      y = merge (finite (y), y, x);
    endif

  endif

endfunction

%!assert (nthroot(-32,5), -2);
%!assert (nthroot(81,4), 3);
%!assert (nthroot(Inf,4), Inf);
%!assert (nthroot(-Inf,7), -Inf);
%!assert (nthroot(-Inf,-7), 0);

%% Test input validation
%!error (nthroot ())
%!error (nthroot (1))
%!error (nthroot (1,2,3))
%!error (nthroot (1+j,2))
%!error (nthroot (1,[1 2]))
%!error (nthroot (1,0))
%!error (nthroot (-1,2))

