########################################################################
##
## Copyright (C) 2000-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{c} =} setdiff (@var{a}, @var{b})
## @deftypefnx {} {@var{c} =} setdiff (@var{a}, @var{b}, "rows")
## @deftypefnx {} {@var{c} =} setdiff (@dots{}, "sorted")
## @deftypefnx {} {@var{c} =} setdiff (@dots{}, "stable")
## @deftypefnx {} {@var{c} =} setdiff (@dots{}, "legacy")
## @deftypefnx {} {[@var{c}, @var{ia}] =} setdiff (@dots{})
## Return the unique elements in @var{a} that are not in @var{b}.
##
## If @var{a} is a row vector return a row vector; Otherwise, return a
## column vector.  The inputs may also be cell arrays of strings.
##
## If the optional input @qcode{"rows"} is given then return the rows in
## @var{a} that are not in @var{b}.  The inputs must be 2-D numeric matrices to
## use this option.
##
## The optional argument @qcode{"sorted"}/@qcode{"stable"} controls the order
## in which unique values appear in the output.  The default is
## @qcode{"sorted"} and values in the output are placed in ascending order.
## The alternative @qcode{"stable"} preserves the order found in the input.
##
## If requested, return the index vector @var{ia} such that
## @code{@var{c} = @var{a}(@var{ia})}.
##
## Programming Note: The input flag @qcode{"legacy"} changes the algorithm
## to be compatible with @sc{matlab} releases prior to R2012b.
##
## @seealso{unique, union, intersect, setxor, ismember}
## @end deftypefn

function [c, ia] = setdiff (a, b, varargin)

  if (nargin < 2 || nargin > 4)
    print_usage ();
  endif

  [a, b] = validsetargs ("setdiff", a, b, varargin{:});

  by_rows = any (strcmp ("rows", varargin));
  optlegacy = any (strcmp ("legacy", varargin));

  if (optlegacy)
    isrowvec = ! iscolumn (a) || ! iscolumn (b);
  else
    isrowvec = isrow (a);
  endif

  if (by_rows)
    if (nargout > 1)
      [c, ia] = unique (a, varargin{:});
    else
      c = unique (a, varargin{:});
    endif
    if (! isempty (c) && ! isempty (b))
      ## Form A and B into combined set.
      b = unique (b, varargin{:});
      [csort, idx] = sortrows ([c; b]);
      ## Eliminate those elements of A that are the same as in B.
      dups = find (all (csort(1:end-1,:) == csort(2:end,:), 2));
      c(idx(dups),:) = [];
      if (nargout > 1)
        ia(idx(dups),:) = [];
      endif
    endif
  else
    if (nargout > 1)
      [c, ia] = unique (a, varargin{:});
    else
      c = unique (a, varargin{:});
    endif
    if (! isempty (c) && ! isempty (b))
      ## Form a and b into combined set.
      b = unique (b);
      [csort, idx] = sort ([c(:); b(:)]);
      ## Eliminate those elements of a that are the same as in b.
      if (iscellstr (csort))
        dups = find (strcmp (csort(1:end-1), csort(2:end)));
      else
        dups = find (csort(1:end-1) == csort(2:end));
      endif
      c(idx(dups)) = [];

      ## Reshape if necessary for Matlab compatibility.
      if (isrowvec)
        c = c(:).';
      else
        c = c(:);
      endif

      if (nargout > 1)
        ia(idx(dups)) = [];
        if (optlegacy && isrowvec)
          ia = ia(:).';
        endif
      endif
    endif
  endif

endfunction


%!assert (setdiff (["bb";"zz";"bb";"zz"], ["bb";"cc";"bb"], "rows"), "zz")
%!assert (setdiff (["b";"z";"b";"z"], ["b";"c";"b"], "rows"), "z")
%!assert (setdiff (["b";"z";"b";"z"], ["b";"c";"b"]), "z")
%!assert (setdiff ([1, 1; 2, 2; 3, 3; 4, 4], [1, 1; 2, 2; 4, 4], "rows"), [3 3])
%!assert (setdiff ([1; 2; 3; 4], [1; 2; 4], "rows"), 3)
%!assert (setdiff ([1, 2; 3, 4], [1, 2; 3, 6], "rows"), [3, 4])
%!assert (setdiff ({"one","two";"three","four"}, {"one","two";"three","six"}),
%!        {"four"})

## Test multi-dimensional input
%!test
%! a = rand (3,3,3);
%! b = a(1);
%! assert (setdiff (a, b), sort (a(2:end)'));

## Test "rows"
%!test
%! a = [7 9 7; 0 0 0; 7 9 7; 5 5 5; 1 4 5];
%! b = [0 0 0; 5 5 5];
%! [c, ia] = setdiff (a, b, "rows");
%! assert (c, [1, 4 ,5; 7, 9 7]);
%! assert (ia, [5; 1]);

%!test
%! a = [7 9 7; 0 0 0; 7 9 7; 5 5 5; 1 4 5];
%! b = [0 0 0; 5 5 5];
%! [c, ia] = setdiff (a, b, "rows", "stable");
%! assert (c, [7, 9 7; 1, 4 ,5]);
%! assert (ia, [1; 5]);

## Test sorting order
%!test
%! a = [5, 1, 4, 1, 3];
%! b = [1; 2; 4];
%! [c, ia] = setdiff (a, b, "sorted");
%! assert (c, [3, 5]);
%! assert (ia, [5; 1]);

%!test
%! a = [5, 1, 4, 1, 3];
%! b = [1; 2; 4];
%! [c, ia] = setdiff (a, b, "stable");
%! assert (c, [5, 3]);
%! assert (ia, [1; 5]);

## Test output orientation compatibility
%!assert <*42577> (setdiff ([1:5], 2), [1,3,4,5])
%!assert <*42577> (setdiff ([1:5]', 2), [1;3;4;5])
%!assert <*42577> (setdiff ([1:5], [2:3]), [1,4,5])
%!assert <*42577> (setdiff ([1:5], [2:3]'), [1,4,5])
%!assert <*42577> (setdiff ([1:5]', [2:3]), [1;4;5])
%!assert <*42577> (setdiff ([1:5]', [2:3]'), [1;4;5])

## Test "legacy" option
%!test
%! a = [3, 6, 2, 1, 5, 1, 1];
%! b = [2, 4, 6];
%! [c, ia] = setdiff (a, b);
%! assert (c, [1, 3, 5]);
%! assert (ia, [4; 1; 5]);
%! [c, ia] = setdiff (a, b, "legacy");
%! assert (c, [1, 3, 5]);
%! assert (ia, [7, 1, 5]);

## "legacy" + "rows" compatibility
%!test
%! a = [7 9 7; 0 0 0; 7 9 7; 5 5 5; 1 4 5];
%! b = [0 0 0; 5 5 5];
%! [c, ia] = setdiff (a, b, "rows");
%! assert (c, [1, 4 ,5; 7, 9 7]);
%! assert (ia, [5; 1]);
%! [c, ia] = setdiff (a, b, "rows", "legacy");
%! assert (c, [1, 4 ,5; 7, 9 7]);
%! assert (ia, [5; 3]);

## Output orientation with "legacy" option
%!assert (size (setdiff ([1:5], [2:3], "legacy")), [1, 3])
%!assert (size (setdiff ([1:5]', [2:3], "legacy")), [1, 3])
%!assert (size (setdiff ([1:5], [2:3]', "legacy")), [1, 3])
%!assert (size (setdiff ([1:5]', [2:3]', "legacy")), [3, 1])
