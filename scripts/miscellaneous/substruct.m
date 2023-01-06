########################################################################
##
## Copyright (C) 2006-2023 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.
##
########################################################################

## -*- texinfo -*-
## @deftypefn {} {@var{s} =} substruct (@var{type}, @var{subs}, @dots{})
## Create a subscript structure for use with @code{subsref} or @code{subsasgn}.
##
## For example:
##
## @example
## @group
## idx = substruct ("()", @{3, ":"@})
##   @result{} idx =
##        scalar structure containing the fields:
##          type = ()
##          subs =
##          @{
##            [1,1] =  3
##            [1,2] = :
##          @}
## x = [1, 2, 3;
##      4, 5, 6;
##      7, 8, 9];
## subsref (x, idx)
##   @result{}   7   8   9
## @end group
## @end example
## @seealso{subsref, subsasgn}
## @end deftypefn

function s = substruct (varargin)

  if (nargin < 2 || mod (nargin, 2) != 0)
    print_usage ();
  endif

  typ = varargin(1:2:nargin);
  sub = varargin(2:2:nargin);
  braces = strcmp (typ, "()") | strcmp (typ, "{}");
  dots = strcmp (typ, ".");
  if (all (braces | dots))
    cells = cellfun ("isclass", sub, "cell");
    chars = cellfun ("isclass", sub, "char");
    if (any (braces & ! cells))
      error ("substruct: for TYPE == () or {}, SUBS must be a cell array");
    elseif (any (dots & ! chars))
      error ("substruct: for TYPE == ., SUBS must be a character string");
    endif
  else
    error ('substruct: TYPE must be one of "()", "{}", or ""');
  endif

  s = struct ("type", typ, "subs", sub);

endfunction


%!test
%! x(1,1).type = "()";
%! x(1,2).type = "{}";
%! x(1,3).type = ".";
%! x(1,1).subs = {1,2,3};
%! x(1,2).subs = {":"};
%! x(1,3).subs = "foo";
%! y = substruct ("()", {1,2,3}, "{}", {":"}, ".", "foo");
%! assert (x,y);

## Test input validation
%!error <Invalid call> substruct ()
%!error <Invalid call> substruct (1)
%!error <Invalid call> substruct (1, 2, 3)
%!error substruct ("x", 1)
%!error substruct ("()", [1,2,3])
%!error substruct (".", {1,2,3})
