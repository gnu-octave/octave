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
## @deftypefn  {} {@var{c} =} setxor (@var{a}, @var{b})
## @deftypefnx {} {@var{c} =} setxor (@var{a}, @var{b}, "rows")
## @deftypefnx {} {@var{c} =} setxor (@dots{}, "sorted")
## @deftypefnx {} {@var{c} =} setxor (@dots{}, "stable")
## @deftypefnx {} {@var{c} =} setxor (@dots{}, "legacy")
## @deftypefnx {} {[@var{c}, @var{ia}, @var{ib}] =} setxor (@dots{})
##
## Return the unique elements exclusive to sets @var{a} or @var{b}.
##
## If @var{a} and @var{b} are both row vectors then return a row vector;
## Otherwise, return a column vector.  The inputs may also be cell arrays of
## strings.
##
## If the optional input @qcode{"rows"} is given then return the rows exclusive
## to sets @var{a} and @var{b}.  The inputs must be 2-D numeric matrices to use
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
## @seealso{unique, union, intersect, setdiff, ismember}
## @end deftypefn

function [c, ia, ib] = setxor (a, b, varargin)

  if (nargin < 2 || nargin > 4)
    print_usage ();
  endif

  [a, b] = validsetargs ("setxor", a, b, varargin{:});

  by_rows = any (strcmp ("rows", varargin));
  optsorted = ! any (strcmp ("stable", varargin));
  optlegacy = any (strcmp ("legacy", varargin));

  if (optlegacy)
    isrowvec = ! iscolumn (a) || ! iscolumn (b);
  else
    isrowvec = isrow (a) && isrow (b);
  endif

  ## Form A and B into sets.
  if (nargout > 1 || ! optsorted)
    [a, ia] = unique (a, varargin{:});
    [b, ib] = unique (b, varargin{:});
  else
    a = unique (a, varargin{:});
    b = unique (b, varargin{:});
  endif

  if (isempty (a))
    c = b;
  elseif (isempty (b))
    c = a;
  else
    ## Reject duplicates.
    if (by_rows)
      na = rows (a);  nb = rows (b);
      [c, i] = sortrows ([a; b]);
      n = rows (c);
      idx = find (all (c(1:n-1, :) == c(2:n, :), 2));
      if (optsorted)
        if (! isempty (idx))
          c([idx, idx+1],:) = [];
          i([idx, idx+1],:) = [];
        endif
      else
        c = [a; b];
        c(i([idx, idx+1]), :) = [];
        if (nargout > 1)
          i([idx, idx+1]) = [];
          ## FIXME: Is there a way to avoid a call to sort?
          i = sort (i);
        endif
      endif
    else
      na = numel (a);  nb = numel (b);
      [c, i] = sort ([a(:); b(:)]);
      if (iscell (c))
        idx = find (strcmp (c(1:end-1), c(2:end)));
      else
        idx = find (c(1:end-1) == c(2:end));
      endif
      if (optsorted)
        if (! isempty (idx))
          c([idx, idx+1]) = [];
          i([idx, idx+1]) = [];
        endif
      else
        c = [a(:); b(:)];
        c(i([idx, idx+1])) = [];
        if (nargout > 1)
          i([idx, idx+1]) = [];
          ## FIXME: Is there a way to avoid a call to sort?
          i = sort (i);
        endif
      endif

      ## Adjust output orientation for Matlab compatibility
      if (isrowvec)
        c = c.';
      endif
    endif

    if (nargout > 1)
      ia = ia(i(i <= na));
      ib = ib(i(i > na) - na);
      if (optlegacy && isrowvec && ! by_rows)
        ia = ia(:).';
        ib = ib(:).';
      endif
    endif
  endif

endfunction


%!assert (setxor ([3,1,2], [4,3,2]), [1,4])
%!assert (setxor ({'a'}, {'a', 'b'}), {'b'})
%!assert (setxor ([5, NaN, NaN], [NaN, NaN, 5]), [NaN NaN NaN NaN])

%!test
%! a = [3, 1, 4, 1, 5];
%! b = [1; 2; 3; 4];
%! [c, ia, ib] = setxor (a, b);
%! assert (c, [2; 5]);
%! assert (ia, [5]);
%! assert (ib, [2]);

## Test multi-dimensional arrays
%!test
%! a = rand (3,3,3);
%! b = a;
%! b(1,1,1) = 2;
%! assert (intersect (a, b), sort (a(2:end)'));

## Test "rows" input
%!test
%! a = [1 2; 4 5; 1 3];
%! b = [1 1; 1 2; 4 5; 2 10];
%! [c, ia, ib] = setxor (a, b, "rows");
%! assert (c, [1 1; 1 3; 2 10]);
%! assert (ia, [3]);
%! assert (ib, [1; 4]);

## Test "stable" sort order
%!test
%! a = [3, 1, 4, 1, 5];
%! b = [1; 2; 3; 4];
%! [c, ia, ib] = setxor (a, b, "stable");
%! assert (c, [5; 2]);
%! assert (ia, [5]);
%! assert (ib, [2]);

%!test
%! a = [1 2; 4 5; 1 3];
%! b = [1 1; 1 2; 4 5; 2 10];
%! [c, ia, ib] = setxor (a, b, "rows", "stable");
%! assert (c, [1 3; 1 1; 2 10]);
%! assert (ia, [3]);
%! assert (ib, [1; 4]);

## Test various empty matrix inputs
%!assert (setxor (1, []), 1)
%!assert (setxor ([], 1), 1)

%!test
%! [c, ia, ib] = setxor ([3 1], []);
%! assert (c, [1 3]);
%! assert (ia, [2; 1]);
%! assert (ib, []);
%!test
%! [c, ia, ib] = setxor ([], [3 1]);
%! assert (c, [1 3]);
%! assert (ia, []);
%! assert (ib, [2; 1]);

%!test
%! a = [2 1; 4 3];  b = [];
%! [c, ia, ib] = setxor (a, b);
%! assert (c, [1; 2; 3; 4]);
%! assert (ia, [3; 1; 4; 2]);
%! assert (isempty (ib));

%!test
%! a = [];  b = [2 1; 4 3];
%! [c, ia, ib] = setxor (a, b);
%! assert (c, [1; 2; 3; 4]);
%! assert (isempty (ia));
%! assert (ib, [3; 1; 4; 2]);

## Test orientation of output
%!shared x,y
%! x = 1:3;
%! y = 2:5;

%!assert (size (setxor (x, y)), [1 3])
%!assert (size (setxor (x', y)), [3 1])
%!assert (size (setxor (x, y')), [3 1])
%!assert (size (setxor (x', y')), [3 1])
%!assert (size (setxor (x, y, "legacy")), [1, 3])
%!assert (size (setxor (x', y, "legacy")), [1, 3])
%!assert (size (setxor (x, y', "legacy")), [1, 3])
%!assert (size (setxor (x', y', "legacy")), [3, 1])

## Test "legacy" input
%!test
%! a = [5 1 3 3 3];
%! b = [4 1 2 2];
%! [c,ia,ib] = setxor (a,b);
%! assert (c, [2, 3, 4, 5]);
%! assert (ia, [3; 1]);
%! assert (ib, [3; 1]);
%! [c,ia,ib] = setxor (a,b, "legacy");
%! assert (c, [2, 3, 4, 5]);
%! assert (ia, [5, 1]);
%! assert (ib, [4, 1]);

%!test  # "legacy" + "rows"
%! A = [1 2; 3 4; 5 6; 3 4; 7 8];
%! B = [3 4; 7 8; 9 10];
%! [c, ia, ib] = setxor (A, B, "rows");
%! assert (c, [1, 2; 5, 6; 9, 10]);
%! assert (ia, [1; 3]);
%! assert (ib, [3]);
%! [c, ia, ib] = setxor (A, B, "rows", "legacy");
%! assert (c, [1, 2; 5, 6; 9, 10]);
%! assert (ia, [1; 3]);
%! assert (ib, [3]);
