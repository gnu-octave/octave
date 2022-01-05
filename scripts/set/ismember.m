########################################################################
##
## Copyright (C) 2000-2026 The Octave Project Developers
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
## @deftypefn  {} {@var{tf} =} ismember (@var{a}, @var{s})
## @deftypefnx {} {@var{tf} =} ismember (@var{a}, @var{s}, "rows")
## @deftypefnx {} {[@var{tf}, @var{s_idx}] =} ismember (@dots{})
## @deftypefnx {} {[@var{tf}, @var{s_idx}] =} ismember (@dots{}, "legacy")
##
## Return a logical matrix @var{tf} with the same shape as @var{a} which is
## @code{true} (@code{1}) if the element in @var{a} is found in @var{s} and
## @code{false} (@code{0}) if it is not.
##
## If a second output argument is requested then the index into @var{s} of each
## matching element is also returned.  In the case of multiple matches, the
## index of the first match is returned.  If the input option @qcode{"legacy"}
## is provided, then the behavior is changed to be compatible with that of
## @sc{matlab} 2012b and earlier, and the last match is returned instead.
##
## For example:
##
## @example
## @group
## a = [3, 10, 1];
## s = [0, 0, 1, 1, 2, 2, 3, 3, 4, 4];
## [tf, s_idx] = ismember (a, s)
##      @xresult{} tf = [1, 0, 1]
##      @xresult{} s_idx = [7, 0, 3]
## [tf2, s_idx2] = ismember (a, s, "legacy")
##      @xresult{} tf2 = [1, 0, 1]
##      @xresult{} s_idx2 = [8, 0, 4]
## @end group
## @end example
##
## The inputs @var{a} and @var{s} may also be cell arrays.
##
## @example
## @group
## a = @{"abc"@};
## s = @{"abc", "def"@};
## [tf, s_idx] = ismember (a, s)
##      @xresult{} tf = 1
##      @xresult{} s_idx = 1
## @end group
## @end example
##
## If the optional argument @qcode{"rows"} is given then compare rows in
## @var{a} with rows in @var{s}.  The inputs must be 2-D matrices with the
## same number of columns to use this option.
##
## @example
## @group
## a = [1:3; 5:7; 4:6];
## s = [0:2; 1:3; 2:4; 3:5; 4:6];
## [tf, s_idx] = ismember (a, s, "rows")
##      @xresult{} tf = logical ([1; 0; 1])
##      @xresult{} s_idx = [2; 0; 5];
## @end group
## @end example
##
## Matlab compatibility note: The @qcode{"legacy"} option currently only
## affects whether the first or last instance of a match is returned in
## @var{s_idx}.  Other impacts, including changes to char array whitespace
## handling and handling of certain classes are not yet implemented.
##
## @seealso{lookup, unique, union, intersect, setdiff, setxor, ismembertol}
## @end deftypefn

function [tf, s_idx] = ismember (a, s, varargin)

  if (nargin < 2 || nargin > 4)
    print_usage ();
  endif

  by_rows = any (strcmpi ("rows", varargin));
  optlegacy = any (strcmpi ("legacy", varargin));

  ## lookup() uses absolute values for complex input so we handle the
  ## real and imaginary parts separately (bug #52437) unless 'rows', which
  ## does not use lookup() so it can be handled normally.
  if (! by_rows && (iscomplex (a) || iscomplex (s)))
    real_argout = cell (nargout, 1);
    imag_argout = cell (nargout, 1);
    [real_argout{:}] = ismember (real (a), real (s), varargin{:});
    [imag_argout{:}] = ismember (imag (a), imag (s), varargin{:});
    tf = real_argout{1} & imag_argout{1};
    if (nargout > 1)
      s_idx = zeros (size (real_argout{2}));
      if (optlegacy)
        s_idx(tf) = min (real_argout{2}(tf), imag_argout{2}(tf));
      else
        s_idx(tf) = max (real_argout{2}(tf), imag_argout{2}(tf));
      endif
    endif
    return;
  endif

  ## lookup() does not handle logical values
  if (islogical (a))
    a = uint8 (a);
  endif
  if (islogical (s))
    s = uint8 (s);
  endif

  ## Matlab-compatible behavior (R2016b).  See bug #51187.
  if (ischar (a) && rows (a) == 1 && iscell (s))
    a = {a};
  endif

  ## Another Matlab-compatible behavior.  See bug #53924.
  if (isnumeric (a) && ischar (s))
    s = double (s);
  elseif (ischar (a) && isnumeric (s))
    a = double (a);
  endif

  if (nargin > 2 && (nargin - by_rows - optlegacy != 2)) 
    error ('ismember: only "rows" and "legacy" are valid options');
  endif
  [a, s] = validsetargs ("ismember", a, s, varargin{:});

  if (! by_rows)
    s = s(:);
    ## Check sort status, because we expect the array will often be sorted.
    ## FIXME: currently fails for some cellstring inputs for s, as they do
    ##  not get vectorized, and matrices cannot by issorted (bug #61765)
    if (issorted (s))
      is = [];
    else
      [s, is] = sort (s);
    endif

    ## Remove NaNs from table because lookup can't handle them
    if (isreal (s) && ! isempty (s) && isnan (s(end)))
      s = s(1:(end - sum (isnan (s))));
    endif

    if (nargout > 1)
      if (! optlegacy)
        s = s(end : -1: 1);
        if (! (isempty (s) || isempty (a)))
          if (isempty (is))
            is = [numel(s) : -1 : 1];
          else
            is = is(1 : numel (s))(end : -1 : 1);
          endif
        endif
      endif

      s_idx = lookup (s, a, "m");
      tf = logical (s_idx);
      if (! isempty (is))
        s_idx(tf) = is(s_idx(tf));
      endif

    else
      tf = lookup (s, a, "b");
    endif

  else  # "rows" argument
    if (isempty (a) || isempty (s))
      tf = false (rows (a), 1);
      if (nargout > 1)
        s_idx = zeros (rows (a), 1);
      endif

    else
      if (rows (s) == 1)
        tf = all (a == s, 2);
        if (nargout > 1)
          s_idx = double (tf);
        endif
      else
        ## FIXME: lookup does not support "rows", so we just use unique.
        na = rows (a);
        if (optlegacy)
          [~, ii, jj] = unique ([a; s], "rows", "last");
          jj = ii(jj(1:na));
          tf = jj > na;
          if (nargout > 1)
            s_idx = max (0, jj - na);
          endif

        else
          [~, ii, jj] = unique ([s; a], "rows", "first");
          nj = numel(jj) - (na-1);
          jj = ii(jj(nj:end));
          tf = jj < nj;
          if (nargout > 1)
            s_idx = jj;
            s_idx (jj>nj) = 0;
          endif
        endif
      endif
    endif
  endif

endfunction


%!assert (ismember ({""}, {"abc", "def"}), false)
%!assert (ismember ("abc", {"abc", "def"}), true)
%!assert (isempty (ismember ([], [1, 2])), true)
%!assert (isempty (ismember ({}, {'a', 'b'})), true)
%!assert (isempty (ismember ([], 'a')), true)
%!assert (ismember ("", {"abc", "def"}), false)
%!assert (ismember (1, 'abc'), false)
%!assert (ismember ("abc", 1), [false false false])
%!assert (ismember ("abc", 99), [false false true])
%!fail ("ismember ([], {1, 2})")
%!fail ("ismember ({[]}, {1, 2})")
%!fail ("ismember ({}, {1, 2})")
%!fail ("ismember ({1}, {'1', '2'})")
%!fail ("ismember ({'1'}, {'1' '2'},'rows')") ##MATLAB gives warning, outputs non-rows answer
%!fail ("ismember ([1 2 3], [5 4 3 1], 'rows')")
%!assert (ismember ({"foo", "bar"}, {"foobar"}), [false false])
%!assert (ismember ({"foo"}, {"foobar"}), false)
%!assert (ismember ({"bar"}, {"foobar"}), false)
%!assert (ismember ({"bar"}, {"foobar", "bar"}), true)
%!assert (ismember ({"foo", "bar"}, {"foobar", "bar"}), [false true])
%!assert (ismember ({"xfb", "f", "b"}, {"fb", "b"}), [false false true])
%!assert (ismember ("1", "0123456789."), true)

%!test
%! [result, s_idx] = ismember ([1, 2], []);
%! assert (result, [false false]);
%! assert (s_idx, [0, 0]);

%!test
%! [result, s_idx] = ismember ([], [1, 2]);
%! assert (result, logical ([]));
%! assert (s_idx, []);

%!test
%! [result, s_idx] = ismember ({"a", "b"}, "");
%! assert (result, [false false]);
%! assert (s_idx, [0, 0]);

%!test
%! [result, s_idx] = ismember ({"a", "b"}, {});
%! assert (result, [false false]);
%! assert (s_idx, [0, 0]);

%!test
%! [result, s_idx] = ismember ("", {"a", "b"});
%! assert (result, false);
%! assert (s_idx, 0);

%!test
%! [result, s_idx] = ismember ({}, {"a", "b"});
%! assert (result, logical ([]));
%! assert (s_idx, []);

%!test
%! [result, s_idx] = ismember ([1 2 3 4 5], [3]);
%! assert (result, logical ([0 0 1 0 0]));
%! assert (s_idx , [0 0 1 0 0]);

%!test
%! [result, s_idx] = ismember ([1 6], [1 2 3 4 5 1 6 1]);
%! assert (result, [true true]);
%! assert (s_idx, [1, 7]);

%!test
%! [result, s_idx] = ismember ([3,10,1], [0,1,2,3,4,5,6,7,8,9]);
%! assert (result, [true false true]);
%! assert (s_idx, [4, 0, 2]);

%!test
%! [result, s_idx] = ismember ("1.1", "0123456789.1");
%! assert (result, [true true true]);
%! assert (s_idx, [2, 11, 2]);

%!test
%! [result, s_idx] = ismember ([1:3; 5:7; 4:6], [0:2; 1:3; 2:4; 3:5; 4:6], "rows");
%! assert (result, [true; false; true]);
%! assert (s_idx, [2; 0; 5]);

%!test
%! [result, s_idx] = ismember ([1.1,1.2,1.3; 2.1,2.2,2.3; 10,11,12], [1.1,1.2,1.3; 10,11,12; 2.12,2.22,2.32], "rows");
%! assert (result, [true; false; true]);
%! assert (s_idx, [1; 0; 2]);

%!test
%! [result, s_idx] = ismember ([1:3; 5:7; 4:6; 0:2; 1:3; 2:4], [1:3], "rows");
%! assert (result, logical ([1 0 0 0 1 0]'));
%! assert (s_idx, [1 0 0 0 1 0]');

%!test <*51187>
%! assert (ismember ('b ', {'a ', 'b '}), true);

%!test <*51187>
%! abc = ['a '; 'b '; 'c '];
%! assert (ismember (abc, {abc}), [false; false; false]);

%!test <*52437>
%! [tf, s_idx] = ismember ([5, 4-3j, 3+4j], [5, 4-3j, 3+4j]);
%! assert (tf, logical ([1, 1, 1]));
%! assert (s_idx, [1, 2, 3]);
%!
%! [tf, s_idx] = ismember ([5, 4-3j, 3+4j], 5);
%! assert (tf, logical ([1, 0, 0]));
%! assert (s_idx, [1, 0, 0]);
%!
%! [tf, s_idx] = ismember ([5, 5, 5], 4-3j);
%! assert (tf, logical ([0, 0, 0]));
%! assert (s_idx, [0, 0, 0]);
%!
%! [tf, s_idx] = ismember ([5, 4-3j, 3+4j; 5i, 6, 6i], [5, 6]);
%! assert (tf, logical ([1, 0, 0; 0, 1, 0]));
%! assert (s_idx, [1, 0, 0; 0, 2, 0]);
%!
%! [tf, s_idx] = ismember ([5, 4-3j, 3+4j; 5, 4-3j, 3+4j], [5, 5, 5], "rows");
%! assert (tf, logical ([0; 0]));
%! assert (s_idx, [0; 0]);
%!
%! [tf, s_idx] = ismember ([5, 5, 5], [5, 4-3j, 3+4j; 5, 5, 5], "rows");
%! assert (tf, true);
%! assert (s_idx, 2);
%!
%! tf = ismember ([5, 4-3j, 3+4j], 5);
%! assert (tf, logical ([1, 0, 0]));
%! [~, s_idx] = ismember ([5, 4-3j, 3+4j], 5);
%! assert (s_idx, [1, 0, 0]);
%!
%! [tf, s_idx] = ismember (-1-1j, [-1-1j, -1+3j, -1+1j]);
%! assert (tf, true);
%! assert (s_idx, 1);

%!test <*55659>
%! A = [5 3 4 2];
%! B = [2 4 4 4 6 8];
%! [result, s_idx] = ismember (A, B)
%! assert (result, [false, false, true, true])
%! assert (s_idx, [0, 0, 2, 1]);
%! [result, s_idx] = ismember (A, B, "legacy");
%! assert (result, [false, false, true, true]);
%! assert (s_idx, [0, 0, 4, 1]);
%!
%! [result, s_idx] = ismember ([1 6], [1 2 3 4 5 1 6 1], "legacy");
%! assert (result, [true true]);
%! assert (s_idx, [8, 7]);
%!
%! [result, s_idx] = ismember ([5, 4-3j, 3+4j; 5i, 6, 6i], [5, 6], "legacy");
%! assert (result, logical ([1, 0, 0; 0, 1, 0]));
%! assert (s_idx, [1, 0, 0; 0, 2, 0]);
%!
%! [result, s_idx] = ismember ([5, 4-3j, 3+4j; 5i, 6, 6i], [5, 6, 5]);
%! assert (result, logical ([1, 0, 0; 0, 1, 0]));
%! assert (s_idx, [1, 0, 0; 0, 2, 0]);
%!
%! [result, s_idx] = ismember ([5, 4-3j, 3+4j; 5i, 6, 6i], [5, 6, 5], "legacy");
%! assert (result, logical ([1, 0, 0; 0, 1, 0]));
%! assert (s_idx, [3, 0, 0; 0, 2, 0]);
%!
%! [result, s_idx] = ismember ([3,10,1], [0,0,1,1,2,2,3,3,4,4]);
%! assert (result, [true false true]);
%! assert (s_idx, [7, 0, 3]);
%!
%! [result, s_idx] = ismember ([3,10,1], [0,0,1,1,2,2,3,3,4,4], "legacy");
%! assert (result, [true false true]);
%! assert (s_idx, [8, 0, 4]);
%!
%! [result, s_idx] = ismember ([3,10,1], [0,0,1,NaN,1,1,2,2,3,3,NaN,3,4,4]);
%! assert (result, [true false true]);
%! assert (s_idx, [9, 0, 3]);
%!
%! [result, s_idx] = ismember ([3,10,1], [0,0,1,NaN,1,1,2,2,3,3,NaN,3,4,4], "legacy");
%! assert (result, [true false true]);
%! assert (s_idx, [12, 0, 6]);
%!
%! [result, s_idx] = ismember ("1.1", "0123456789.1", "legacy");
%! assert (result, [true true true]);
%! assert (s_idx, [12, 11, 12]);
%!
%! [result, s_idx] = ismember ([1:3; 5:7; 4:6], [0:2; 1:3; 2:4; 3:5; 4:6; 1:3], "rows");
%! assert (result, [true; false; true]);
%! assert (s_idx, [2; 0; 5]);
%!
%! [result, s_idx] = ismember ([1:3; 5:7; 4:6], [0:2; 1:3; 2:4; 3:5; 4:6; 1:3], "rows", "legacy");
%! assert (result, [true; false; true]);
%! assert (s_idx, [6; 0; 5]);

%!xtest <61765>
%! ## issorted() can't handle char matrix().  Returns [1;0;0].  
%! abc = [' a'; ' b'; ' c'];
%! [result, s_idx] = ismember (abc, {abc});
%! assert (result, [false; false; false]);
%! assert (s_idx, [0; 0; 0]);

%!xtest <61765>
%! ## issorted() can't handle cellstr array.
%! ## Maybe also legacy handling of char arrays.
%! abc = ['a '; 'b '; 'c '];
%! [result, s_idx] = ismember (abc, {abc}, "legacy");
%! assert (result, [true; true; true]);
%! assert (s_idx, [1; 2; 3]);

## Test input validation
%!error <Invalid call> ismember ()
%!error <Invalid call> ismember (1)
%!error <Invalid call> ismember (1,2,3,4,5)
%!error <only "rows" and "legacy" are valid options> ismember (1,2, "foo", "legacy")
%!error <only "rows" and "legacy" are valid options> ismember (1,2, "rows", "foo")
%!error <only "rows" and "legacy" are valid options> ismember (1,2, "foo", "bar")
%!error <only "rows" and "legacy" are valid options> ismember (1,2, "sorted")
%!error <only "rows" and "legacy" are valid options> ismember (1,2, "stable")
