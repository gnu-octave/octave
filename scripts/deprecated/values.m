## Copyright (C) 1995-2012 Kurt Hornik
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

## -*- texinfo -*-
## @deftypefn {Function File} {} values (@var{x})
## Return the different values in a column vector, arranged in ascending
## order.
##
## As an example, @code{values([1, 2, 3, 1])} returns the vector
## @code{[1, 2, 3]}.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Extract unique elements

## Deprecated in version 3.4

function v = values (x)

  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:deprecated-function",
             "values is obsolete and will be removed from a future version of Octave; please use unique instead");
  endif

  if (nargin != 1)
    print_usage ();
  endif

  if (! (isvector (x)))
    error ("values: X must be a vector");
  endif

  i = any (isnan (x));
  ## HACK!
  x = x(find(!isnan (x)));
  n = length (x);
  x = reshape (x, n, 1);
  s = sort (x);
  v = s([1; (find (s(2:n) > s(1:n-1)) + 1)]);
  if (i)
    v = [v; NaN];
  endif

endfunction
