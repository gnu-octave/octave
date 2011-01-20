## Copyright (C) 2008-2011 David Bateman
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
## Translate a string for use in a regular expression.  This might
## include either wildcard replacement or special character escaping.
## The behavior can be controlled by the @var{op} that can have the
## values
##
## @table @asis
## @item "wildcard"
## The wildcard characters @code{.}, @code{*} and @code{?} are replaced
## with wildcards that are appropriate for a regular expression.
## For example:
##
## @example
## @group
## regexptranslate ("wildcard", "*.m")
##      @result{} ".*\.m"
## @end group
## @end example
##
## @item "escape"
## The characters @code{$.?[]}, that have special meaning for regular
## expressions are escaped so that they are treated literally.  For example:
##
## @example
## @group
## regexptranslate ("escape", "12.5")
##      @result{} "12\.5"
## @end group
## @end example
##
## @end table
## @seealso{regexp, regexpi, regexprep}
## @end deftypefn

function y = regexptranslate (op, s)

  if nargin != 2
    print_usage ();
  endif

  if (ischar (op))
    op = tolower (op);
    if (strcmp ("wildcard", op))
      y = regexprep (regexprep (regexprep (s, "\\.", "\\."), "\\*",
                                ".*"), "\\?", ".");
    elseif (strcmp ("escape", op))
      ch = {'\$', '\.', '\?', '\[', '\]'};
      y = s;
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

%!error <Invalid call to regexptranslate> regexptranslate ();
%!error <Invalid call to regexptranslate> regexptranslate ("wildcard");
%!error <Invalid call to regexptranslate> regexptranslate ("a", "b", "c");
%!error <unexpected operation> regexptranslate ("foo", "abc");
%!error <expecting operation to be a string> regexptranslate (10, "abc");
%!assert (regexptranslate ("wildcard", "/a*b?c."), "/a.*b.c\\.")
%!assert (regexptranslate ("escape", '$.?[]'), '\$\.\?\[\]')
