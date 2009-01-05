## Copyright (C) 2004, 2006, 2007 by Alois Schloegl
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
## @deftypefn {Function File} {@var{idx} =} strfind (@var{str}, @var{pattern})
## @deftypefnx {Function File} {@var{idx} =} strfind (@var{cellstr}, @var{pattern})
## Search for @var{pattern} in the string @var{str} and return the
## starting index of every such occurrence in the vector @var{idx}.
## If there is no such occurrence, or if @var{pattern} is longer
## than @var{str}, then @var{idx} is the empty array @code{[]}.
##
## If the cell array of strings @var{cellstr} is specified instead of the
## string @var{str}, then @var{idx} is a cell array of vectors, as specified
## above. Examples:
##
## @example
## @group
## strfind ("abababa", "aba")
##      @result{} [1, 3, 5]
##
## strfind (@{"abababa", "bebebe", "ab"@}, "aba")
##      @result{} ans =
##         @{
##           [1,1] =
##
##              1   3   5
##
##           [1,2] = [](1x0)
##           [1,3] = [](1x0)
##         @}
## @end group
## @end example
## @seealso{findstr, strmatch, strcmp, strncmp, strcmpi, strncmpi, find}
## @end deftypefn

## Author: alois schloegl <a.schloegl@ieee.org>
## Created: 1 November 2004
## Adapted-By: William Poetra Yoga Hadisoeseno <williampoetra@gmail.com>

function idx = strfind (text, pattern)

  if (nargin != 2)
    print_usage ();
  elseif (! ischar (pattern))
    error ("strfind: pattern must be a string value");
  endif

  lp = length (pattern);

  if (ischar (text))
    idx = __strfind_string__ (text, pattern, lp);
  elseif (iscellstr (text))
    idx = cell (size (text));
    for i = 1:(numel (text))
      idx{i} = __strfind_string__ (text{i}, pattern, lp);
    endfor
  else
    error ("strfind: text must be a string or cell array of strings");
  endif

### endfunction

function idx = __strfind_string__ (text, pattern, lp)

  idx = 1:(length (text) - lp + 1);
  k = 0;
  while (k < lp && ! isempty (idx))
    idx = idx(text(idx + k) == pattern(++k));
  endwhile

### endfunction

%!error <Invalid call to strfind> strfind ();
%!error <Invalid call to strfind> strfind ("foo", "bar", 1);
%!error <pattern must be a string value> strfind ("foo", 100);
%!error <text must be a string or cell array of string> strfind (100, "foo");

%!assert (strfind ("abababa", "aba"), [1, 3, 5]);
%!assert (strfind ({"abababa", "bla", "bla"}, "a"), {[1, 3, 5, 7], 3, 3});
