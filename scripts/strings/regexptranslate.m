## Copyright (C) 2008  David Bateman
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
## @deftypefn {Function File} {} regexptranslate (@var{op}, @var{s})
## Translate a string for use in a regular expression. This might
## include either wildcard replacement or special character escaping.
## The behavior can be controlled by the @var{op} that can have the
## values
##
## @table @asis
## @item "wildcard"
## The wildcard characters @code{.}, @code{*} and @code{?} are replaced
## with wildcards that are appropriate for a regular expression.
##
## @item "escape"
## The characters @code{$.?[]}, that have special meaning for regular
## expressions are escaped so that they are treated literally.
## @end table
## @end deftypefn

function y = regexptranslate (op, x)
  
  if (ischar (op))
    op = tolower (op);
    if (strcmp ("wildcard", op))
      y = regexprep (regexprep (regexprep (x, "\\.", "\\."), "\\*",
				".*"), "\\?", ".");
    elseif (strcmp ("escape", op))
      ch = {'\$', '\.', '\?', '\[', '\]'};
      y = x;
      for i = 1 : length (ch)
	y = regexprep (y, ch{i}, ch{i});
      endfor
    else
      error ("regexptranslate: unexpected operation");
    endif
  else
    error ("regexptranslate: expecting operation to be a string");
  endif
endfunction

%!assert (regexptranslate ("wildcard", "/a*b?c."), "/a.*b.c\\.")
%!assert (regexptranslate ("escape", '$.?[]'), '\$\.\?\[\]')
