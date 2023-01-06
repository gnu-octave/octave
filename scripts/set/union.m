########################################################################
##
## Copyright (C) 1994-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{c} =} union (@var{a}, @var{b})
## @deftypefnx {} {@var{c} =} union (@var{a}, @var{b}, "rows")
## @deftypefnx {} {@var{c} =} union (@dots{}, "sorted")
## @deftypefnx {} {@var{c} =} union (@dots{}, "stable")
## @deftypefnx {} {@var{c} =} union (@dots{}, "legacy")
## @deftypefnx {} {[@var{c}, @var{ia}, @var{ib}] =} union (@dots{})
##
## Return the unique elements that are in either @var{a} or @var{b}.
##
## If @var{a} and @var{b} are both row vectors then return a row vector;
## Otherwise, return a column vector.  The inputs may also be cell arrays of
## strings.
##
## If the optional input @qcode{"rows"} is given then return rows that are in
## either @var{a} or @var{b}.  The inputs must be 2-D numeric matrices to use
## this option.
##
## The optional argument @qcode{"sorted"}/@qcode{"stable"} controls the order
## in which unique values appear in the output.  The default is
## @qcode{"sorted"} and values in the output are placed in ascending order.
## The alternative @qcode{"stable"} preserves the order found in the input.
##
## The optional outputs @var{ia} and @var{ib} are column index vectors such
## that @code{@var{a}(@var{ia})} and @code{@var{b}(@var{ib})} are disjoint sets
## whose union is @var{c}.
##
## Programming Note: The input flag @qcode{"legacy"} changes the algorithm
## to be compatible with @sc{matlab} releases prior to R2012b.
##
## @seealso{unique, intersect, setdiff, setxor, ismember}
## @end deftypefn

function [y, ia, ib] = union (a, b, varargin)

  if (nargin < 2 || nargin > 4)
    print_usage ();
  endif

  [a, b] = validsetargs ("union", a, b, varargin{:});

  by_rows = any (strcmp ("rows", varargin));
  optlegacy = any (strcmp ("legacy", varargin));

  if (optlegacy)
    isrowvec = ! iscolumn (a) || ! iscolumn (b);
  else
    isrowvec = isrow (a) && isrow (b);
  endif

  if (by_rows)
    y = [a; b];
  else
    y = [a(:); b(:)];
    ## Adjust output orientation for Matlab compatibility
    if (isrowvec)
      y = y.';
    endif
  endif

  if (nargout <= 1)
    y = unique (y, varargin{:});
  else
    [y, idx] = unique (y, varargin{:});
    if (by_rows)
      na = rows (a);
    else
      na = numel (a);
    endif
    ia = idx(idx <= na);
    ib = idx(idx > na) - na;
  endif

endfunction


%!assert (union ([1, 2, 4], [2, 3, 5]), [1, 2, 3, 4, 5])
%!assert (union ([1; 2; 4], [2, 3, 5]), [1; 2; 3; 4; 5])
%!assert (union ([1; 2; 4], [2; 3; 5]), [1; 2; 3; 4; 5])
%!assert (union ([1, 2, 3], [5; 7; 9]), [1; 2; 3; 5; 7; 9])
%!assert (union ([1 2; 2 3; 4 5], [2 3; 3 4; 5 6], "rows"),
%!        [1 2; 2 3; 3 4; 4 5; 5 6])

## Test multi-dimensional arrays
%!test
%! a = rand (3,3,3);
%! b = a;
%! b(1,1,1) = 2;
%! assert (union (a, b), sort ([a(1:end)'; 2]));

%!test
%! a = [3, 1, 4, 1, 5];
%! b = [1; 2; 3; 4];
%! [y, ia, ib] = union (a, b);
%! assert (y, [1; 2; 3; 4; 5]);
%! assert (y, sort ([a(ia)'; b(ib)']));

## Test "stable" sorting order
%!assert (union ([1, 2, 4], [2, 3, 5], "stable"), [1, 2, 4, 3, 5])
%!assert (union ([1, 2, 4]', [2, 3, 5], "stable"), [1; 2; 4; 3; 5])
%!assert (union ([1, 2, 4], [2, 3, 5]', "stable"), [1; 2; 4; 3; 5])

%!test
%! a = [3, 1, 4, 1, 5];
%! b = [1; 2; 3; 4];
%! [y, ia, ib] = union (a, b, "stable");
%! assert (y, [3; 1; 4; 5; 2]);
%! assert (ia, [1; 2; 3; 5]);
%! assert (ib, [2]);

## Test indexing outputs
%!test
%! a = [1, 4, 2];
%! b = [2, 3, 5];
%! [~, ia, ib] = union (a, b);
%! assert (ia, [1; 3; 2]);
%! assert (ib, [2; 3]);
%! [~, ia, ib] = union (a, b, "stable");
%! assert (ia, [1; 2; 3]);
%! assert (ib, [2; 3]);

%!test
%! a = [1 2; 4 5; 2 3];
%! b = [2 3; 3 4; 5 6];
%! [~, ia, ib] = union (a, b, "rows");
%! assert (ia, [1; 3; 2]);
%! assert ([2; 3]);
%! [~, ia, ib] = union (a, b, "rows", "stable");
%! assert (ia, [1; 2; 3]);
%! assert ([2; 3]);

## Test "legacy" option
%!test
%! a = [5, 7, 1];
%! b = [3, 1, 1];
%! [c, ia, ib] = union (a,b);
%! assert (c, [1, 3, 5, 7]);
%! assert (ia, [3; 1; 2]);
%! assert (ib, [1]);
%! [c, ia, ib] = union (a,b, "legacy");
%! assert (c, [1, 3, 5, 7]);
%! assert (ia, [1, 2]);
%! assert (ib, [3, 1]);

%!test  # "legacy" + "rows"
%! A = [1 2; 3 4; 5 6; 3 4; 7 8];
%! B = [3 4; 7 8; 9 10];
%! [c, ia, ib] = union (A, B, "rows");
%! assert (c, [1, 2; 3, 4; 5, 6; 7, 8; 9, 10]);
%! assert (ia, [1; 2; 3; 5]);
%! assert (ib, [3]);
%! [c, ia, ib] = union (A, B, "rows", "legacy");
%! assert (c, [1, 2; 3, 4; 5, 6; 7, 8; 9, 10]);
%! assert (ia, [1; 3]);
%! assert (ib, [1; 2; 3]);

## Test orientation of output
%!shared x,y
%! x = 1:3;
%! y = 2:5;

%!assert (size (union (x, y)), [1 5])
%!assert (size (union (x', y)), [5 1])
%!assert (size (union (x, y')), [5 1])
%!assert (size (union (x', y')), [5 1])
%!assert (size (union (x, y, "legacy")), [1, 5])
%!assert (size (union (x', y, "legacy")), [1, 5])
%!assert (size (union (x, y', "legacy")), [1, 5])
%!assert (size (union (x', y', "legacy")), [5, 1])

## Clear shared variables
%!shared

## Test empty cell string array unions
%!assert (union ({}, []), cell (0,1))
%!assert (union ([], {}), cell (0,1))
%!assert (union ([], {'a', 'b'}), {'a';'b'})
%!assert (union ({'a', 'b'}, []), {'a';'b'})
%!assert (union (['a', 'b'], {}), {'ab'})
%!assert (union ({}, ['a', 'b']), {'ab'})

## Test common input validation for set routines contained in validsetargs
%!error <cell array of strings cannot be combined> union ({"a"}, 1)
%!error <A and B must be arrays or cell arrays> union (@sin, 1)
%!error <cells not supported with "rows"> union ({"a"}, {"b"}, "rows")
%!error <cells not supported with "rows"> union ({"a"}, {"b"}, "rows","legacy")
%!error <A and B must be arrays or cell arrays> union (@sin, 1, "rows")
%!error <A and B must be arrays or cell arrays> union (@sin,1,"rows","legacy")
%!error <A and B must be 2-dimensional matrices> union (rand (2,2,2), 1, "rows")
%!error <A and B must be 2-dimensional matrices> union (1, rand (2,2,2), "rows")
%!error <A and B must be 2-dimensional matrices>
%! union (rand (2,2,2), 1, "rows", "legacy");
%!error <A and B must be 2-dimensional matrices>
%! union (1, rand (2,2,2), "rows", "legacy");
%!error <number of columns in A and B must match> union ([1 2], 1, "rows")
%!error <number of columns in A and B must match> union (1, [1 2], "rows")
%!error <number of columns in A and B must match>
%! union ([1 2], 1, "rows", "legacy");
%!error <number of columns in A and B must match>
%! union (1, [1 2], "rows", "legacy");
%!error <invalid option: columns> union (1, 2, "columns")
%!error <invalid option: columns> union (1, 2, "legacy", "columns")
%!error <only one of "sorted", "stable", or "legacy" may be specified>
%! union (1, 2, "sorted", "stable");
%!error <only one of "sorted", "stable", or "legacy" may be specified>
%! union (1, 2, "sorted", "legacy");
