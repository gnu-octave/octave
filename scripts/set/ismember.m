## Copyright (C) 2000-2012 Paul Kienzle
## Copyright (C) 2009 Jaroslav Hajek
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
## @deftypefn  {Function File} {@var{tf} =} ismember (@var{A}, @var{s})
## @deftypefnx {Function File} {[@var{tf}, @var{S_idx}] =} ismember (@var{A}, @var{s})
## @deftypefnx {Function File} {[@var{tf}, @var{S_idx}] =} ismember (@var{A}, @var{s}, "rows")
## Return a logical matrix @var{tf} with the same shape as @var{A} which is
## true (1) if @code{A(i,j)} is in @var{s} and false (0) if it is not.  If a
## second output argument is requested, the index into @var{s} of each of the
## matching elements is also returned.
##
## @example
## @group
## a = [3, 10, 1];
## s = [0:9];
## [tf, s_idx] = ismember (a, s)
##      @result{} tf = [1, 0, 1]
##      @result{} s_idx = [4, 0, 2]
## @end group
## @end example
##
## The inputs, @var{A} and @var{s}, may also be cell arrays.
##
## @example
## @group
## a = @{'abc'@};
## s = @{'abc', 'def'@};
## [tf, s_idx] = ismember (a, s)
##      @result{} tf = [1, 0]
##      @result{} s_idx = [1, 0]
## @end group
## @end example
##
## With the optional third argument @code{"rows"}, and matrices
## @var{A} and @var{s} with the same number of columns, compare rows in
## @var{A} with the rows in @var{s}.
##
## @example
## @group
## a = [1:3; 5:7; 4:6];
## s = [0:2; 1:3; 2:4; 3:5; 4:6];
## [tf, s_idx] = ismember(a, s, "rows")
##      @result{} tf = logical ([1; 0; 1])
##      @result{} s_idx = [2; 0; 5];
## @end group
## @end example
##
## @seealso{unique, union, intersect, setxor, setdiff}
## @end deftypefn

## Author: Paul Kienzle <pkienzle@users.sf.net>
## Author: Søren Hauberg <hauberg@gmail.com>
## Author: Ben Abbott <bpabbott@mac.com>
## Adapted-by: jwe
## Reimplemented using lookup & unique: Jaroslav Hajek <highegg@gmail.com>

function [tf, a_idx] = ismember (A, s, varargin)

  if (nargin < 2 || nargin > 3)
    print_usage ();
  endif

  ## lookup() does not handle logical values
  if (islogical (A))
    A = uint8 (A);
  endif
  if (islogical (s))
    s = uint8 (s);
  endif

  [A, s] = validargs ("ismember", A, s, varargin{:});

  if (nargin == 2)
    s = s(:);
    ## We do it this way, because we expect the array to be often sorted.
    if (issorted (s))
      is = [];
    else
      [s, is] = sort (s);
    endif

    ## sort out NaNs in table
    if (isreal (s) && ! isempty (s) && isnan (s(end)))
        s = s(1:end - sum (isnan (s)));
    endif

    if (nargout > 1)
      a_idx = lookup (s, A, "m");
      tf = logical (a_idx);
      if (! isempty (is))
        a_idx(tf) = is (a_idx(tf));
      endif
    else
      tf = lookup (s, A, "b");
    endif

  else

    if (isempty (A) || isempty (s))
      tf = false (rows (A), 1);
      a_idx = zeros (rows (A), 1);
    else

      ## FIXME: lookup does not support "rows", so we just use unique.
      [xx, ii, jj] = unique ([A; s], "rows", "last");
      na = rows (A);
      jj = ii(jj(1:na));
      tf = jj > na;

      if (nargout > 1)
        a_idx = max (0, jj - na);
      endif

    endif
  endif

endfunction

%!assert (ismember ({''}, {'abc', 'def'}), false);
%!assert (ismember ('abc', {'abc', 'def'}), true);
%!assert (isempty (ismember ([], [1, 2])), true);
%!assert (isempty (ismember ({}, {'a', 'b'})), true);
%!assert (ismember ('', {'abc', 'def'}), false);
%!fail ('ismember ([], {1, 2})');
%!fail ('ismember ({[]}, {1, 2})');
%!fail ('ismember ({}, {1, 2})');
%!fail ('ismember ({1}, {''1'', ''2''})');
%!fail ('ismember (1, ''abc'')');
%!fail ('ismember ({''1''}, {''1'', ''2''},''rows'')');
%!fail ('ismember ([1 2 3], [5 4 3 1], ''rows'')');
%!assert (ismember ({'foo', 'bar'}, {'foobar'}), logical ([0, 0]));
%!assert (ismember ({'foo'}, {'foobar'}), false);
%!assert (ismember ({'bar'}, {'foobar'}), false);
%!assert (ismember ({'bar'}, {'foobar', 'bar'}), true);
%!assert (ismember ({'foo', 'bar'}, {'foobar', 'bar'}), logical ([0, 1]));
%!assert (ismember ({'xfb', 'f', 'b'}, {'fb', 'b'}), logical ([0, 0, 1]));
%!assert (ismember ("1", "0123456789."), true);

%!test
%! [result, a_idx] = ismember ([1, 2], []);
%! assert (result, logical ([0, 0]))
%! assert (a_idx, [0, 0]);

%!test
%! [result, a_idx] = ismember ([], [1, 2]);
%! assert (result, logical ([]))
%! assert (a_idx, []);

%!test
%! [result, a_idx] = ismember ({'a', 'b'}, '');
%! assert (result, logical ([0, 0]))
%! assert (a_idx, [0, 0]);

%!test
%! [result, a_idx] = ismember ({'a', 'b'}, {});
%! assert (result, logical ([0, 0]))
%! assert (a_idx, [0, 0]);

%!test
%! [result, a_idx] = ismember ('', {'a', 'b'});
%! assert (result, false)
%! assert (a_idx, 0);

%!test
%! [result, a_idx] = ismember ({}, {'a', 'b'});
%! assert (result, logical ([]))
%! assert (a_idx, []);

%!test
%! [result, a_idx] = ismember([1 2 3 4 5], [3]);
%! assert (all (result == logical ([0 0 1 0 0])) && all (a_idx == [0 0 1 0 0]));

%!test
%! [result, a_idx] = ismember([1 6], [1 2 3 4 5 1 6 1]);
%! assert (all (result == logical ([1 1])) && a_idx(2) == 7);

%!test
%! [result, a_idx] = ismember ([3,10,1], [0,1,2,3,4,5,6,7,8,9]);
%! assert (all (result == logical ([1, 0, 1])) && all (a_idx == [4, 0, 2]));

%!test
%! [result, a_idx] = ismember ("1.1", "0123456789.1");
%! assert (all (result == logical ([1, 1, 1])) && all (a_idx == [12, 11, 12]));

%!test
%! [result, a_idx] = ismember([1:3; 5:7; 4:6], [0:2; 1:3; 2:4; 3:5; 4:6], 'rows');
%! assert (all (result == logical ([1; 0; 1])) && all (a_idx == [2; 0; 5]));

%!test
%! [result, a_idx] = ismember([1.1,1.2,1.3; 2.1,2.2,2.3; 10,11,12], [1.1,1.2,1.3; 10,11,12; 2.12,2.22,2.32], 'rows');
%! assert (all (result == logical ([1; 0; 1])) && all (a_idx == [1; 0; 2]));

