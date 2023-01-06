########################################################################
##
## Copyright (C) 1994-2023 The Octave Project Developers
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
    [b, r] = filter (y, a, x);
    r *= a(1);
  elseif (ly == la)
    [b, r] = filter (y, a, 1);
    r *= a(1);
  else
    b = 0;
    r = y;
  endif

  if (isargout (2))
    if (ly >= la)
      r = [zeros(ly - la + 1, 1); r(1:la - 1)];
      ## Respect the orientation of Y
      r = reshape (r, size (y));
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

%!test
%! y = (10:-1:1);
%! a = (4:-1:1);
%! [b, r] = deconv (y, a);
%! assert (conv (a, b) + r, y, eps);

%!test <*51221>
%! a = [1.92306958582241e+15, 3.20449986572221e+24, 1.34271290136344e+32, ...
%!     2.32739765751038e+38];
%! b = [7.33727670161595e+27, 1.05919311870816e+36, 4.56169848520627e+42];
%! [div, rem] = deconv (a, b);
%! assert (rem, [0, 0, -2.89443678763879e+32  -1.58695290534499e+39], -10*eps);
%! a(2) = 3.204499865722215e+24;
%! [div, rem] = deconv (a, b);
%! assert (rem, [0, 0, -2.89443678763879e+32  -1.58695290534499e+39], -10*eps);

%!test
%! [b, r] = deconv ([1, 1], 1);
%! assert (r, [0, 0]);

%!test
%! [b, r] = deconv ([1; 1], 1);
%! assert (r, [0; 0]);
