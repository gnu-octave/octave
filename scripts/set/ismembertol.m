########################################################################
##
## Copyright (C) 2023 The Octave Project Developers
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
## @deftypefn  {} {@var{tf} =} ismembertol (@var{a}, @var{s})
## @deftypefnx {} {@var{tf} =} ismembertol (@var{a}, @var{s}, @var{tol})
## @deftypefnx {} {@var{tf} =} ismembertol (@var{a}, @var{s}, @var{name}, @var{value})
## @deftypefnx {} {[@var{tf}, @var{s_idx}] =} ismembertol (@dots{})
## Check if values are members of a set within a tolerance.
##
## This functions returns a logical matrix @var{tf} with the same shape as
## @var{a} which is true (1) where the element in @var{a} is close to @var{s}
## within a tolerance @var{tol} and false (0) if it is not.  If @var{tol} is
## not specified, a default tolerance of @code{1e-6} is used.
##
## If a second output argument is requested then the index into @var{s} of each
## matching element is also returned.
##
## The inputs @var{a} and @var{s} must be numeric values.
##
## @example
## @group
## a = [3, 10, 1];
## s = [0:9];
## [tf, s_idx] = ismembertol (a, s)
##      @result{} tf = [1, 0, 1]
##      @result{} s_idx = [4, 0, 2]
## @end group
## @end example
##
## Optional property/value pairs may be given to change the function's
## behavior.  The property may be one of following strings:
##
## @table @asis
## @item @qcode{"ByRows"}
## If set to @code{false} (default), all elements in @var{a} and @var{s} are
## treated separately.  If set to @code{true}, @var{tf} will be @code{true}
## for each row in @var{a} that matches a row in @var{s} within the given
## tolerance.  Two rows, @var{u} and @var{v}, are within tolerance if they
## fulfill the condition @code{all (abs (u-v) <= tol*max (abs ([a;s])))}.
##
## @item @qcode{"OutputAllIndices"}
## If set to @code{false} (default), @var{s_idx} contains indices for one
## of the matches.  If set to @code{true}, @var{s_idx} is a cell array
## containing the indices for all elements in @var{s} that are within tolerance
## of the corresponding value in @var{a}.
##
## @item @qcode{"DataScale"}
## The provided value @var{DS} is used to change the scale factor in the
## tolerance test to @code{abs (u-v) <= tol*@var{DS}}.  By default, the maximum
## absolute value in @var{a} and @var{s} is used as the scale factor.
## @end table
##
## Example:
##
## @example
## @group
## s = [1:6].' * pi;
## a = 10.^log10 (x);
## [tf, s_idx] = ismembertol (a, s);
## @end group
## @end example
##
## @seealso{ismember, lookup, unique, union, intersect, setdiff, setxor}
## @end deftypefn

function [tf, s_idx] = ismembertol (a, s, varargin)

  if (nargin < 2 || nargin > 9)
    print_usage ();
  endif

  if (nargin < 3 || ! isnumeric (varargin{1}))
    # defaut tolerance
    tol = 1e-6;
  else
    tol = varargin{1};
    varargin(1) = [];
  endif

  if (! isnumeric (a) || ! isnumeric (s))
    error ("ismembertol: A and S must contain numeric values");
  endif

  if (nargin > 2 ...
      && (! iscellstr (varargin(1:2:end)) ...
          || any (! ismember (lower (varargin(1:2:end)), ...
                              {"outputallindices", "byrows", "datascale"}))))
    error ("ismembertol: unsupported property");
  endif

  by_rows_idx = find (strcmpi ("ByRows", varargin));
  by_rows = (! isempty (by_rows_idx) && logical (varargin{by_rows_idx+1}) );

  if (by_rows && columns (a) != columns (s))
    error ("ismembertol: number of columns in A and S must match for 'ByRows'");
  endif

  all_indices_idx = find (strcmpi ("OutputAllIndices", varargin));
  all_indices = (! isempty (all_indices_idx) ...
                 && logical (varargin{all_indices_idx+1}) );

  data_scale_idx = find (strcmpi ("DataScale", varargin));
  data_scale = (! isempty (data_scale_idx) ...
                && isnumeric (varargin{data_scale_idx+1}) );
  if (data_scale)
    DS = varargin{data_scale_idx+1};
  else
    DS = max (abs ([a(:);s(:)]));
  endif

  if (! by_rows)
    sa = size (a);
    s = s(:);
    a = a(:);
    ## Check sort status, because we expect the array will often be sorted.
    if (issorted (s))
      is = [];
    else
      [s, is] = sort (s);
    endif

    ## Remove NaNs from table because lookup can't handle them
    if (isreal (s) && ! isempty (s) && isnan (s(end)))
      s = s(1:(end - sum (isnan (s))));
    endif

    if (! data_scale)
      DS = max (abs ([a(:);s(:)]));
    endif

    [s_i, s_j] = find (abs (transpose (s) - a) < tol * DS);
    if (! all_indices)
      s_idx = zeros (size (a));
      [~, I] = unique (s_i);
      s_j = s_j(I);
      s_idx(s_i(I)) = s_j;
      tf = logical (s_idx);
      if (! isempty (is))
        s_idx(tf) = is(s_idx(tf));
      endif
      s_idx = reshape (s_idx, sa);
      tf = reshape (tf, sa);
    else # all_indices
      s_idx = cell (size(a));
      tf = zeros (size(a));
      C = unique (s_j);
      for ic = C.'
        ii = find (s_j == ic);
        for sii = s_i(ii).'
          if (! isempty (is))
            s_idx{sii} = [s_idx{sii}, is(ic)];
          else
            s_idx{sii} = [s_idx{sii}, ic];
          endif
        endfor

        tf(ic) = 1;
      endfor
    endif

  else  # "ByRows"
    if (isempty (a) || isempty (s))
      tf = false (rows (a), 1);
      s_idx = zeros (rows (a), 1);
    else
      if (rows (s) == 1)
        tf = all (bsxfun (@eq, a, s), 2);
        s_idx = double (tf);
      else
        ## Two rows, u and v, are within tolerance if
        ## all(abs(u-v) <= tol*max(abs([A;B]))).
        na = rows (a);
        if (! all_indices)
          s_idx = zeros (na, 1);
        else
          s_idx = cell (na, 1);
        endif
        if (length (DS) == 1)
          DS = repmat (DS, 1, columns (a));
        endif
        for i = 1:na
          if (! all_indices)
            s_i = find ( all (abs (a(i,:) - s) < tol * DS, 2), 1);
            if (! isempty (s_i))
              s_idx(i) = s_i;
            endif
          else
            s_i = find (all (abs (a(i,:) - s) < tol * DS, 2));
            if (! isempty (s_i))
              s_idx{i} = s_i;
            endif
          endif
        endfor
        if (! all_indices)
          tf = logical (s_idx);
        else
          tf = cellfun(@(x) ! isempty (x) && all (x(:)!=0), s_idx);
        endif
      endif
    endif
  endif

endfunction

%!demo
%! ## Group random data
%! A = rand (1000, 2);
%! B = [(0:.2:1).', 0.5*ones(6,1)];
%! [LIA, LocAllB] = ismembertol (B, A, 0.1, 'ByRows', true, 'OutputAllIndices', true, 'DataScale', [1,Inf]);
%! plot (B(:,1), B(:,2), 'x');
%! hold on
%! for k = 1:length (LocAllB)
%!   plot (A(LocAllB{k},1), A(LocAllB{k},2), '.');
%! endfor

%!assert (isempty (ismembertol ([], [1, 2])), true)

%!test
%! [result, s_idx] = ismembertol ([1; 2], []);
%! assert (result, [false; false]);
%! assert (s_idx, [0; 0]);

%!test
%! [result, s_idx] = ismembertol ([], [1, 2]);
%! assert (result, logical ([]));
%! assert (s_idx, []);

%!test
%! [result, s_idx] = ismembertol ([1 2 3 4 5], [3]);
%! assert (result, logical ([0 0 1 0 0]));
%! assert (s_idx , [0 0 1 0 0]);

%!test
%! [result, s_idx] = ismembertol ([1 6], [1 2 3 4 5 1 6 1]);
%! assert (result, [true, true]);
%! assert (s_idx(2), 7);

%!test
%! [result, s_idx] = ismembertol ([3,10,1], [0,1,2,3,4,5,6,7,8,9]);
%! assert (result, [true, false, true]);
%! assert (s_idx, [4, 0, 2]);

%!test
%! [result, s_idx] = ismembertol ([1:3; 5:7; 4:6], [0:2; 1:3; 2:4; 3:5; 4:6], "ByRows", true);
%! assert (result, [true; false; true]);
%! assert (s_idx, [2; 0; 5]);

%!test
%! [result, s_idx] = ismembertol ([1.1,1.2,1.3; 2.1,2.2,2.3; 10,11,12], ...
%!                                [1.1,1.2,1.3; 10,11,12; 2.12,2.22,2.32], "ByRows", true);
%! assert (result, [true; false; true]);
%! assert (s_idx, [1; 0; 2]);

%!test
%! [result, s_idx] = ismembertol ([1:3; 5:7; 4:6; 0:2; 1:3; 2:4], [1:3], "ByRows", true);
%! assert (result, logical ([1 0 0 0 1 0].'));
%! assert (s_idx, [1 0 0 0 1 0].');

%!test
%! [tf, s_idx] = ismembertol ([5, 4-3j, 3+4j], [5, 4-3j, 3+4j]);
%! assert (tf, logical ([1 1 1]));
%! assert (s_idx, [1 2 3]);

%!test
%! [tf, s_idx] = ismembertol ([5, 4-3j, 3+4j], 5);
%! assert (tf, logical ([1 0 0]));
%! assert (s_idx, [1 0 0]);

%!test
%! [tf, s_idx] = ismembertol ([5, 5, 5], 4-3j);
%! assert (tf, logical ([0 0 0]));
%! assert (s_idx, [0 0 0]);

%!test
%! [tf, s_idx] = ismembertol ([5, 4-3j, 3+4j; 5, 4-3j, 3+4j], [5, 5, 5], "ByRows", true);
%! assert (tf, logical ([0; 0]));
%! assert (s_idx, [0; 0]);

%!test
%! [tf, s_idx] = ismembertol ([5, 5, 5], [5, 4-3j, 3+4j; 5, 5, 5], "ByRows", true);
%! assert (tf, true);
%! assert (s_idx, 2);

%!test
%! tf = ismembertol ([5, 4-3j, 3+4j], 5);
%! assert (tf, logical ([1 0 0]));
%! [~, s_idx] = ismembertol ([5, 4-3j, 3+4j], 5);
%! assert (s_idx, [1 0 0]);

%!test
%! [tf, s_idx] = ismembertol (-1-1j, [-1-1j, -1+3j, -1+1j]);
%! assert (tf, true);
%! assert (s_idx, 1);

%!test
%! [tf, s_idx] = ismembertol ([0.9 1.9 3.1 4.2], [1 2 3], 0.1);
%! assert (tf, [true true true false]);
%! assert (s_idx, [1 2 3 0]);

%!test
%! [tf, s_idx] = ismembertol ([1:10] + 0.01 * (rand (1,10) - 0.5), [1:10], 0.01);
%! assert (tf, true (1, 10));
%! assert (s_idx, [1:10]);

## Test input validation
%!error <Invalid call> ismembertol ()
%!error <Invalid call> ismembertol (1)
%!error <unsupported property> ismembertol (1,2,3,4)
%!error <must contain numeric values> ismembertol ([], {1, 2})
%!error <must contain numeric values> ismembertol ({[]}, {1, 2})
%!error <must contain numeric values> ismembertol ({}, {1, 2})
%!error <must contain numeric values> ismembertol ({1}, {'1', '2'})
%!error <must contain numeric values> ismembertol ({'1'}, {'1', '2'}, 'ByRows', true)
%!error <number of columns .* must match> ismembertol ([1 2 3], [5 4 3 1], 'ByRows', true)
