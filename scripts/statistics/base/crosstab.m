## Copyright (C) 1995-2017 Kurt Hornik
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
## @deftypefn {} {@var{t} =} table (@var{x}, @var{y})
## Create a cross-tabulation (contingency table) @var{t} from data vectors.
##
## The inputs @var{x}, @var{y} must be vectors of equal length with a data
## type of numeric, logical, or char.
##
## Currently, only 1- and 2-dimensional tables are supported.
## @end deftypefn

function t = crosstab (x, y)

  if (nargin != 2)
    print_usage ();
  endif

  if (! (   isvector (x) && isreal (x)
         && isvector (y) && isreal (y)
         && (numel (x) == numel (y))))
    error ("crosstab: X and Y must be real vectors of the same length");
  endif

  x = x(:);
  y = y(:);
  v = unique (x);
  w = unique (y);
  for i = 1 : length (v)
    for j = 1 : length (w)
      t(i,j) = sum ((x == v(i) | isnan (v(i)) * isnan (x)) &
                    (y == w(j) | isnan (w(j)) * isnan (y)));
    endfor
  endfor

endfunction


## Test input validation
%!error crosstab ()
%!error crosstab (1)
%!error crosstab (1, 2, 3)
%!error <X .* must be .* vector> crosstab (ones (2), [1 1])
%!error <Y must be .* vector> crosstab ([1 1], ones (2))
%!error <X .* must be real> crosstab ({true, true}, [1 1])
%!error <Y must be real> crosstab ([1 1], {true, true})
%!error <X and Y must be .* of the same length> crosstab ([1], [1 1])
%!error <X and Y must be .* of the same length> crosstab ([1 1], [1])
