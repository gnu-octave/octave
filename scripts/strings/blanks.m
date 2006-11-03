## Copyright (C) 1996, 2006 Kurt Hornik
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} blanks (@var{n})
## Return a string of @var{n} blanks.
## @seealso{repmat}
## @end deftypefn

## Author: Kurt Hornik <Kurt.Hornik@wu-wien.ac.at>
## Adapted-By: jwe

function s = blanks (n)

  if (nargin != 1)
    print_usage ();
  elseif (! (isscalar (n) && n == round (n)))
    error ("blanks: n must be a non-negative integer");
  endif

  s(1,1:n) = " ";

endfunction

## There really isn't that much to test here
%!assert(blanks (0), "")
%!assert(blanks (5), "     ")
%!assert(blanks (10), "          ")
