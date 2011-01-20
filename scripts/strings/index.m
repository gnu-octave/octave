## Copyright (C) 1996-2011 Kurt Hornik
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
## @deftypefn  {Function File} {} index (@var{s}, @var{t})
## @deftypefnx {Function File} {} index (@var{s}, @var{t}, @var{direction})
## Return the position of the first occurrence of the string @var{t} in the
## string @var{s}, or 0 if no occurrence is found.  For example:
##
## @example
## @group
## index ("Teststring", "t")
##      @result{} 4
## @end group
## @end example
##
## If @var{direction} is @samp{"first"}, return the first element found.
## If @var{direction} is @samp{"last"}, return the last element found.
## The @code{rindex} function is equivalent to @code{index} with
## @var{direction} set to @samp{"last"}.
##
## @strong{Caution:}  This function does not work for arrays of
## character strings.
## @seealso{find, rindex}
## @end deftypefn

## Author: Kurt Hornik <Kurt.Hornik@wu-wien.ac.at>
## Adapted-By: jwe

function n = index (s, t, direction)

  ## This is patterned after the AWK function of the same name.

  if (nargin < 2 || nargin > 3)
    print_usage ();
  elseif (nargin < 3)
    direction = "first";
  endif
  direction = lower (direction);

  f = strfind (s, t);
  if (iscell (f))
    f(cellfun ("isempty", f)) = {0};
  elseif (isempty (f))
    f = 0;
  endif

  if (strcmp (direction, "last"))
    if (iscell (f))
      n = cellfun (@min, f);
    else
      n = f(end);
    endif
  elseif (strcmp (direction, "first"))
    if (iscell (f))
      n = cellfun (@max, f);
    else
      n = f(1);
    endif
  else
    error ("index: DIRECTION must be either \"first\" or \"last\"");
  endif
endfunction

## Test the function out
%!assert(index("astringbstringcstring", "s"), 2)
%!assert(index("astringbstringcstring", "st"), 2)
%!assert(index("astringbstringcstring", "str"), 2)
%!assert(index("astringbstringcstring", "string"), 2)
%!assert(index("abc---", "abc+++"), 0)

## test everything out in reverse
%!assert(index("astringbstringcstring", "s", "last"), 16)
%!assert(index("astringbstringcstring", "st", "last"), 16)
%!assert(index("astringbstringcstring", "str", "last"), 16)
%!assert(index("astringbstringcstring", "string", "last"), 16)
%!assert(index("abc---", "abc+++", "last"), 0)


%!assert(index ("foobarbaz", "b") == 4 && index ("foobarbaz", "z") == 9);

%!error index ();

%!error index ("foo", "bar", 3);

