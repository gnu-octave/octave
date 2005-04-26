## Copyright (C) 1996 Kurt Hornik
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
## @deftypefn {Function File} {} substr (@var{s}, @var{beg}, @var{len})
## Return the substring of @var{s} which starts at character number
## @var{beg} and is @var{len} characters long.
##
## If OFFSET is negative, extraction starts that far from the end of
## the string.  If LEN is omitted, the substring extends to the end
## of S.
##
##   For example,
##
## @example
## substr ("This is a test string", 6, 9)
##      @result{} "is a test"
## @end example
##
## @quotation
## This function is patterned after AWK.  You can get the same result by
## @code{@var{s} (@var{beg} : (@var{beg} + @var{len} - 1))}.
## @end quotation
## @end deftypefn

## Author: Kurt Hornik <Kurt.Hornik@ci.tuwien.ac.at>
## Adapted-By: jwe

function t = substr (s, offset, len)

  if (nargin < 2 || nargin > 3)
    usage ("substr (s, offset, len)");
  endif

  if (isstr (s))
    nc = columns (s);
    if (abs (offset) > 0 && abs (offset) <= nc)
      if (offset > 0)
        beg = offset;
      else
        beg = nc + offset + 1;
      endif
      if (nargin == 2)
        eos = nc;
      else
        eos = beg + len - 1;
      endif
      if (eos <= nc)
        t = s (:, beg:eos);
      else
        error ("substr: length = %d out of range", len);
      endif
    else
      error ("substr: offset = %d out of range", offset);
    endif
  else
    error ("substr: expecting string argument");
  endif

endfunction
