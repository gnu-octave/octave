## Copyright (C) 1995-2011 Kurt Hornik
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
## @deftypefn {Function File} {} dmult (@var{a}, @var{b})
## This function has been deprecated.  Use the direct syntax @code{diag(A)*B}
## which is more readable and now also more efficient.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Rescale the rows of a matrix

## Deprecated in version 3.2

function M = dmult (a, B)

  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:deprecated-function",
             "dmult is obsolete and will be removed from a future version of Octave; please use the straightforward (and now efficient) syntax \"diag(A)*B\"");
  endif

  if (nargin != 2)
    print_usage ();
  endif
 if (! isvector (a))
    error ("dmult: a must be a vector of length rows (B)");
  endif
  a = a(:);
  sb = size (B);
  sb(1) = 1;
  M = repmat (a(:), sb) .* B;
endfunction
