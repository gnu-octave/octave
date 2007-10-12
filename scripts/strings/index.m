## Copyright (C) 1996, 1999, 2000, 2002, 2004, 2005, 2006, 2007
##               Kurt Hornik
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
## @deftypefn {Function File} {} index (@var{s}, @var{t})
## @deftypefnx {Function File} {} index (@var{s}, @var{t}, @var{direction})
## Return the position of the first occurrence of the string @var{t} in the
## string @var{s}, or 0 if no occurrence is found.  For example,
##
## @example
## index ("Teststring", "t")
##      @result{} 4
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

  if (! (ischar (s) && ischar (t)))
    error ("index: expecting character string arguments");
  elseif (! strcmp (direction, {"first", "last"}))
    error ("index: direction must be either \"first\" or \"last\"");
  endif

  l_s = length (s);
  l_t = length (t);

  n = 0;
  if (l_s == 0 || l_s < l_t)
    ## zero length source, or target longer than source
    ## return 0
    v = [];

  elseif (l_t == 0)
    ## zero length target: return first
    v = 1;

  elseif (l_t == 1)
    ## length one target: simple find
    v = find (s==t, 1, direction);

  elseif (l_t == 2)
    ## length two target: find first at i and second at i+1
    v = find (s (1:l_s-1) == t(1) & s(2:l_s) == t(2), 1, direction);

  else
    ## length three or more: match the first three by find then go through
    ## the much smaller list to determine which of them are real matches
    limit = l_s - l_t + 1;
    v = find (s (1:limit) == t(1)
	      & s (2:limit+1) == t(2)
	      & s (3:limit+2) == t(3));
    if (strcmp (direction, "last"))
      v = v(length(v):-1:1);
    endif

    if (l_t > 3)

      ## force strings to be both row vectors or both column vectors
      if (all (size (s) != size (t)))
	t = t.';
      endif

      ## search index vector for a match
      ind = 0:l_t-1;
      ## return 0 if loop terminates without finding any match
      for idx = 1:length(v)
	if (s (v(idx) + ind) == t)
	  n = v(idx);
	  break;
	endif
      endfor
      v = [];
    endif

  endif

  if (n == 0 && ! isempty (v))
    ## return the first found if n is not already set and v is not empty
    n = v(1);
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
