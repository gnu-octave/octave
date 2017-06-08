## Copyright (C) 1994-2017 John W. Eaton
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {} {@var{b} =} deconv (@var{y}, @var{a})
## @deftypefnx {} {[@var{b}, @var{r}] =} deconv (@var{y}, @var{a})
## Deconvolve two vectors (polynomial division).
##
## @code{[@var{b}, @var{r}] = deconv (@var{y}, @var{a})} solves for @var{b} and
## @var{r} such that @code{@var{y} = conv (@var{a}, @var{b}) + @var{r}}.
##
## If @var{y} and @var{a} are polynomial coefficient vectors, @var{b} will
## contain the coefficients of the polynomial quotient and @var{r} will be
## a remainder polynomial of lowest order.
## @seealso{conv, residue}
## @end deftypefn

## Author: Tony Richardson <arichard@stark.cc.oh.us>
## Created: June 1994
## Adapted-By: jwe

function [b, r] = deconv (y, a)

  if (nargin != 2)
    print_usage ();
  endif

  if (! (isvector (y) && isvector (a)))
    error ("deconv: Y and A must be vectors");
  endif

  ## Ensure A is oriented as Y.
  if ((isrow (y) && iscolumn (a)) || (iscolumn (y) && isrow (a)))
    a = a.';
  endif

  la = length (a);
  ly = length (y);

  lb = ly - la + 1;

  if (ly > la)
    x = zeros (size (y) - size (a) + 1);
    x(1) = 1;
    b = filter (y, a, x);
  elseif (ly == la)
    b = filter (y, a, 1);
  else
    b = 0;
  endif

  if (isargout (2))
    lc = la + length (b) - 1;
    if (ly == lc)
      r = y - conv (a, b);
    else
      ## Respect the orientation of Y.
      if (rows (y) <= columns (y))
        r = [(zeros (1, lc - ly)), y] - conv (a, b);
      else
        r = [(zeros (lc - ly, 1)); y] - conv (a, b);
      endif
      if (ly < la)
        ## Trim the remainder to be the length of Y.
        r = r(end-(length(y)-1):end);
      endif
    endif
  endif

endfunction


%!test
%! [b, r] = deconv ([3, 6, 9, 9], [1, 2, 3]);
%! assert (b, [3, 0]);
%! assert (r, [0, 0, 0, 9]);

%!test
%! [b, r] = deconv ([3, 6], [1, 2, 3]);
%! assert (b, 0);
%! assert (r, [3, 6]);

%!test
%! [b, r] = deconv ([3, 6], [1; 2; 3]);
%! assert (b, 0);
%! assert (r, [3, 6]);

%!test
%! [b,r] = deconv ([3; 6], [1; 2; 3]);
%! assert (b, 0);
%! assert (r, [3; 6]);

%!test
%! [b, r] = deconv ([3; 6], [1, 2, 3]);
%! assert (b, 0);
%! assert (r, [3; 6]);

%!assert (deconv ((1:3)',[1, 1]), [1; 1])

## Test input validation
%!error deconv (1)
%!error deconv (1,2,3)
%!error <Y .* must be vector> deconv ([3, 6], [1, 2; 3, 4])
%!error <A must be vector> deconv ([3, 6], [1, 2; 3, 4])
