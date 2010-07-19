## Copyright (C) 1996, 1999, 2000, 2004, 2005, 2006, 2007, 2008,
##               2009 Kurt Hornik
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
## @deftypefn {Function File} {} substr (@var{s}, @var{offset}, @var{len})
## Return the substring of @var{s} which starts at character number
## @var{offset} and is @var{len} characters long.
##
## If @var{offset} is negative, extraction starts that far from the end of
## the string.  If @var{len} is omitted, the substring extends to the end
## of S.
##
## For example,
##
## @example
## @group
## substr ("This is a test string", 6, 9)
##      @result{} "is a test"
## @end group
## @end example
##
## This function is patterned after AWK@.  You can get the same result by
## @code{@var{s}(@var{offset} : (@var{offset} + @var{len} - 1))}.
## @end deftypefn

## Author: Kurt Hornik <Kurt.Hornik@wu-wien.ac.at>
## Adapted-By: jwe

function t = substr (s, offset, len)

  if (nargin < 2 || nargin > 3)
    print_usage ();
  endif

  if (ischar (s))
    nc = columns (s);
    if (abs (offset) > 0 && abs (offset) <= nc)
      if (offset <= 0)
        offset += nc + 1;
      endif
      if (nargin == 2)
        eos = nc;
      else
        eos = offset + len - 1;
      endif
      if (eos <= nc)
        t = s (:, offset:eos);
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

%!assert(strcmp (substr ("This is a test string", 6, 9), "is a test"));

%!error substr ();

%!error substr ("foo", 2, 3, 4);

