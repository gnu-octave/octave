## Copyright (C) 2006-2011 William Poetra Yoga Hadisoeseno
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
## @deftypefn {Function File} {} strtrunc (@var{s}, @var{n})
## Truncate the character string @var{s} to length @var{n}.  If @var{s}
## is a char matrix, then the number of columns is adjusted.
##
## If @var{s} is a cell array of strings, then the operation is performed
## on its members and the new cell array is returned.
## @end deftypefn

function s = strtrunc (s, n)

  if (nargin != 2)
    print_usage ();
  endif

  if (ischar (s))
    s_was_char = true;
    s = {s};
  else
    s_was_char = false;
  endif

  if (iscellstr (s))
    for i = 1:(numel (s))
      s{i} = s{i}(:,1:(min (n, columns (s{i}))));
    endfor
  else
    error ("strtrunc: S must be a character string or a cell array of strings");
  endif

  if (s_was_char)
    s = s{:};
  endif

endfunction

%!error <Invalid call to strtrunc> strtrunc ();
%!error <S must be a character string or a cell array of strings> strtrunc (1, 1)
%!assert (strtrunc("abcdefg", 4), "abcd");
%!assert (strtrunc("abcdefg", 10), "abcdefg");
%!assert (strtrunc({"abcdef", "fedcba"}, 3), {"abc", "fed"});
