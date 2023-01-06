########################################################################
##
## Copyright (C) 2014-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{bw} =} bandwidth (@var{A}, @var{type})
## @deftypefnx {} {[@var{lower}, @var{upper}] =} bandwidth (@var{A})
## Compute the bandwidth of @var{A}.
##
## The @var{type} argument is the string @qcode{"lower"} for the lower
## bandwidth and @qcode{"upper"} for the upper bandwidth.  If no @var{type} is
## specified return both the lower and upper bandwidth of @var{A}.
##
## The lower/upper bandwidth of a matrix is the number of
## subdiagonals/superdiagonals with nonzero entries.
##
## @seealso{isbanded, isdiag, istril, istriu}
## @end deftypefn

function [lower, upper] = bandwidth (A, type)

  if (nargin < 1)
    print_usage ();
  endif

  if (! isnumeric (A) && ! islogical (A) || ndims (A) != 2)
    error ("bandwidth: A must be a 2-D numeric or logical matrix");
  elseif (nargin == 2 && ! (strcmp (type, "lower") || strcmp (type, "upper")))
    error ('bandwidth: TYPE must be "lower" or "upper"');
  endif

  if (nargin == 1)
    [i, j] = find (A);
    if (isempty (i))
      lower = upper = 0;
    else
      lower = max (0, max (i - j));
      upper = max (0, max (j - i));
    endif
  else
    [i, j] = find (A);
    if (isempty (i))
      lower = 0;
    elseif (strcmp (type, "lower"))
      lower = max (0, max (i - j));
    else
      lower = max (0, max (j - i));
    endif
  endif

endfunction


%!test
%! [a,b] = bandwidth (speye (100));
%! assert ([a,b], [0,0]);
%! assert (bandwidth (speye (100), "upper"), 0);
%! assert (bandwidth (speye (100), "lower"), 0);

%!test
%! A = [2 3 0 0 0; 1 2 3 0 0; 0 1 2 3 0; 0 0 1 2 3; 0 0 0 1 2];
%! [a,b] = bandwidth (A);
%! assert ([a,b], [1,1]);
%! assert (bandwidth (A, "lower"), 1);
%! assert (bandwidth (A, "upper"), 1);

%!assert (bandwidth ([], "lower"), 0)
%!assert (bandwidth ([], "upper"), 0)
%!assert (bandwidth ([]), 0)
%!assert (bandwidth (zeros (3,3), "lower"), 0)
%!assert (bandwidth (zeros (3,3), "upper"), 0)
%!assert (bandwidth (zeros (3,3)), 0)
%!assert (bandwidth (ones (5,5), "lower"), 4)
%!assert (bandwidth (ones (5,5), "upper"), 4)
%!assert (bandwidth (ones (5,5)), 4)

%!assert (bandwidth ([0,1,2,0]), 0)

%!test
%! [a,b] = bandwidth ([]);
%! assert ([a,b], [0,0]);
%!test
%! [a,b] = bandwidth (zeros (3,3));
%! assert ([a,b], [0,0]);
%!test
%! [a,b] = bandwidth (ones (5,5));
%! assert ([a,b], [4,4]);

## Test input validation
%!error <Invalid call> bandwidth ()
%!error <A must be a 2-D numeric or logical> bandwidth ("string", "lower")
%!error <A must be a 2-D numeric or logical> bandwidth (ones (3,3,3), "lower")
%!error <TYPE must be "lower" or "upper"> bandwidth (ones (2), "uper")
%!error <TYPE must be "lower" or "upper"> bandwidth (ones (2), "uppper")
