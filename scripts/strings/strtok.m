## Copyright (C) 2000-2011 Paul Kienzle
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
## @deftypefn {Function File} {[@var{tok}, @var{rem}] =} strtok (@var{str}, @var{delim})
## 
## Find all characters up to but not including the first character which
## is in the string delim.  If @var{rem} is requested, it contains the
## remainder of the string, starting at the first delimiter.  Leading
## delimiters are ignored.  If @var{delim} is not specified, space is
## assumed.  For example: 
##
## @example
## @group
## strtok ("this is the life")
##      @result{} "this"
##
## [tok, rem] = strtok ("14*27+31", "+-*/")
##      @result{}
##         tok = 14
##         rem = *27+31
## @end group
## @end example
## @seealso{index, strsplit}
## @end deftypefn

## FIXME: check what to do for a null delimiter

function [tok, rem] = strtok (str, delim)

  if (nargin<1 || nargin > 2)
    print_usage ();
  endif

  if (nargin < 2 || isempty (delim))
    delim = "\t\n\v\f\r "; 
  endif

  if (isempty (str))
    tok = rem = "";
  elseif (length (delim) > 3)
    start = 1;
    len = length (str);
    while (start <= len)
      if (all (str(start) != delim))
        break; 
      endif
      start++;
    endwhile
    stop = start;
    while (stop <= len)
      if (any (str(stop) == delim))
        break; 
      endif
      stop++;
    endwhile
    tok = str(start:stop-1);
    rem = str(stop:len);
  else
    if (length (delim) == 1)
      idx = find (str == delim);
    elseif (length (delim) == 2)
      idx = find (str == delim(1) | str == delim(2));
    else
      idx = find (str == delim(1) | str == delim(2) | str == delim(3));
    endif
    if (isempty (idx))
      tok = str;
      rem = "";
    else
      ## Find first non-leading delimiter.
      skip = find (idx(:)' != 1:length(idx));
      if (isempty (skip))
        tok = str(idx(length(idx))+1:length(str));
        rem = "";
      else
        tok = str(skip(1):idx(skip(1))-1);
        rem = str(idx(skip(1)):length(str));
      endif
    endif
  endif

endfunction

%!demo
%! strtok("this is the life")
%! % split at the first space, returning "this"

%!demo
%! s = "14*27+31"
%! while 1
%!   [t,s] = strtok(s, "+-*/");
%!   printf("<%s>", t);
%!   if isempty(s), break; endif
%!   printf("<%s>", s(1));
%! endwhile
%! printf("\n");
%! % ----------------------------------------------------
%! % Demonstrates processing of an entire string split on
%! % a variety of delimiters. Tokens and delimiters are 
%! % printed one after another in angle brackets.  The
%! % string is:

%!# test the tokens for all cases
%!assert(strtok(""), "");             # no string
%!assert(strtok("this"), "this");     # no delimiter in string
%!assert(strtok("this "), "this");    # delimiter at end
%!assert(strtok("this is"), "this");  # delimiter in middle
%!assert(strtok(" this"), "this");    # delimiter at start
%!assert(strtok(" this "), "this");   # delimiter at start and end
%!assert(strtok(" "), ""(1:0));            # delimiter only

%!# test the remainder for all cases
%!test [t,r] = strtok(""); assert(r, "");
%!test [t,r] = strtok("this"); assert(r, char (zeros (1, 0)));
%!test [t,r] = strtok("this "); assert(r, " ");
%!test [t,r] = strtok("this is"); assert(r, " is");
%!test [t,r] = strtok(" this"); assert(r, char (zeros (1, 0)));
%!test [t,r] = strtok(" this "); assert(r, " ");
%!test [t,r] = strtok(" "); assert(r, char (zeros (1, 0)));

%!# simple check with 2 and 3 delimeters
%!assert(strtok("this is", "i "), "th");
%!assert(strtok("this is", "ij "), "th");

%!# test all cases for 4 delimiters since a different 
%!# algorithm is used when more than 3 delimiters
%!assert(strtok("","jkl "), "");
%!assert(strtok("this","jkl "), "this");
%!assert(strtok("this ","jkl "), "this");
%!assert(strtok("this is","jkl "), "this");
%!assert(strtok(" this","jkl "), "this");
%!assert(strtok(" this ","jkl "), "this");
%!assert(strtok(" ","jkl "), ""(1:0));

%!# test 'bad' string orientations
%!assert(strtok(" this "'), "this"');   # delimiter at start and end
%!assert(strtok(" this "',"jkl "), "this"');

%!# test with TAB, LF, VT, FF, and CR
%!test
%! for ch = "\t\n\v\f\r"
%!   [t, r] = strtok (cstrcat ("beg", ch, "end"));
%!   assert (t, "beg");
%!   assert (r, cstrcat (ch, "end"))
%! endfor
