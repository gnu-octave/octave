## Copyright (C) 1996-2012 Kurt Hornik
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
## @deftypefn {Function File} {} cut (@var{x}, @var{breaks})
## Create categorical data from numerical or continuous data by
## cutting into intervals.
##
## If @var{breaks} is a scalar, the data is cut into that many
## equal-width intervals.  If @var{breaks} is a vector of break points,
## the category has @code{length (@var{breaks}) - 1} groups.
##
## The returned value is a vector of the same size as @var{x} telling
## which group each point in @var{x} belongs to.  Groups are labelled
## from 1 to the number of groups; points outside the range of
## @var{breaks} are labelled by @code{NaN}.
## @seealso{histc}
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Cut data into intervals

function group = cut (x, breaks)

  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:deprecated-function",
             "cut is obsolete and will be removed from a future version of Octave; please use histc instead");
  endif

  if (nargin != 2)
    print_usage ();
  endif

  if (!isvector (x))
    error ("cut: X must be a vector");
  endif
  if isscalar (breaks)
    breaks = linspace (min (x), max (x), breaks + 1);
    breaks(1) = breaks(1) - 1;
  elseif isvector (breaks)
    breaks = sort (breaks);
  else
    error ("cut: BREAKS must be a scalar or vector");
  endif

  group = NaN (size (x));
  m = length (breaks);
  if any (k = find ((x >= min (breaks)) & (x < max (breaks))))
    n = length (k);
    group(k) = sum ((ones (m, 1) * reshape (x(k), 1, n))
                    >= (reshape (breaks, m, 1) * ones (1, n)));
  endif

endfunction
