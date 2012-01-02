## Copyright (C) 2000-2012 Paul Kienzle
## Copyright (C) 2008-2009 Jaroslav Hajek
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
## @deftypefn  {Function File} {} setdiff (@var{a}, @var{b})
## @deftypefnx {Function File} {} setdiff (@var{a}, @var{b}, "rows")
## @deftypefnx {Function File} {[@var{c}, @var{i}] =} setdiff (@var{a}, @var{b})
## Return the elements in @var{a} that are not in @var{b}, sorted in
## ascending order.  If @var{a} and @var{b} are both column vectors
## return a column vector, otherwise return a row vector.
## @var{a}, @var{b} may be cell arrays of string(s).
##
## Given the optional third argument @samp{"rows"}, return the rows in
## @var{a} that are not in @var{b}, sorted in ascending order by rows.
##
## If requested, return @var{i} such that @code{c = a(i)}.
## @seealso{unique, union, intersect, setxor, ismember}
## @end deftypefn

## Author: Paul Kienzle
## Adapted-by: jwe

function [c, i] = setdiff (a, b, varargin)

  if (nargin < 2 || nargin > 3)
    print_usage ();
  endif

  [a, b] = validargs ("setdiff", a, b, varargin{:});

  if (nargin > 2)
    if (nargout > 1)
      [c, i] = unique (a, "rows");
    else
      c = unique (a, "rows");
    endif
    if (! isempty (c) && ! isempty (b))
      ## Form a and b into combined set.
      b = unique (b, "rows");
      [dummy, idx] = sortrows ([c; b]);
      ## Eliminate those elements of a that are the same as in b.
      dups = find (all (dummy(1:end-1,:) == dummy(2:end,:), 2));
      c(idx(dups),:) = [];
      if (nargout > 1)
        i(idx(dups),:) = [];
      endif
    endif
  else
    if (nargout > 1)
      [c, i] = unique (a);
    else
      c = unique (a);
    endif
    if (! isempty (c) && ! isempty (b))
      ## Form a and b into combined set.
      b = unique (b);
      [dummy, idx] = sort ([c(:); b(:)]);
      ## Eliminate those elements of a that are the same as in b.
      if (iscellstr (dummy))
        dups = find (strcmp (dummy(1:end-1), dummy(2:end)));
      else
        dups = find (dummy(1:end-1) == dummy(2:end));
      endif
      c(idx(dups)) = [];
      if (nargout > 1)
        i(idx(dups)) = [];
      endif
      ## Reshape if necessary.
      if (size (c, 1) != 1 && size (b, 1) == 1)
        c = c.';
      endif
    endif
  endif

endfunction

%!assert(setdiff(["bb";"zz";"bb";"zz"],["bb";"cc";"bb"],"rows"), "zz")
%!assert(setdiff(["b";"z";"b";"z"],["b";"c";"b"],"rows"), "z")
%!assert(setdiff(["b";"z";"b";"z"],["b";"c";"b"]), "z")
%!assert(setdiff([1, 1; 2, 2; 3, 3; 4, 4], [1, 1; 2, 2; 4, 4], "rows"), [3 3])
%!assert(setdiff([1; 2; 3; 4], [1; 2; 4], "rows"), 3)
%!assert(setdiff([1, 2; 3, 4], [1, 2; 3, 6], "rows"), [3, 4])
%!assert(setdiff({"one","two";"three","four"},{"one","two";"three","six"}), {"four"})

%!test
%! a = [3, 1, 4, 1, 5]; b = [1, 2, 3, 4];
%! [y, i] = setdiff (a, b.');
%! assert(y, [5]);
%! assert(y, a(i));
