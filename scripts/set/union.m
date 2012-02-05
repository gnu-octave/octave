## Copyright (C) 1994-2012 John W. Eaton
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
## @deftypefn  {Function File} {} union (@var{a}, @var{b})
## @deftypefnx {Function File} {} union (@var{a}, @var{b}, "rows")
## Return the set of elements that are in either of the sets @var{a} and
## @var{b}.  @var{a}, @var{b} may be cell arrays of string(s).
## For example:
##
## @example
## @group
## union ([1, 2, 4], [2, 3, 5])
##     @result{} [1, 2, 3, 4, 5]
## @end group
## @end example
##
## If the optional third input argument is the string "rows" each row of
## the matrices @var{a} and @var{b} will be considered an element of sets.
## For example:
##
## @example
## @group
## union ([1, 2; 2, 3], [1, 2; 3, 4], "rows")
##    @result{}  1   2
##        2   3
##        3   4
## @end group
## @end example
##
## @deftypefnx {Function File} {[@var{c}, @var{ia}, @var{ib}] =} union (@var{a}, @var{b})
##
## Return index vectors @var{ia} and @var{ib} such that @code{a(ia)} and
## @code{b(ib)} are disjoint sets whose union is @var{c}.
##
## @seealso{intersect, setdiff, unique}
## @end deftypefn

## Author: jwe

function [y, ia, ib] = union (a, b, varargin)

  if (nargin < 2 || nargin > 3)
    print_usage ();
  endif

  [a, b] = validargs ("union", a, b, varargin{:});

  if (nargin == 2)
    y = [a(:); b(:)];
    na = numel (a); nb = numel (b);
    if (size (a, 1) == 1 || size (b, 1) == 1)
      y = y.';
    endif
  else
    y = [a; b];
    na = rows (a); nb = rows (b);
  endif

  if (nargout == 1)
    y = unique (y, varargin{:});
  else
    [y, i] = unique (y, varargin{:});
    ia = i(i <= na);
    ib = i(i > na) - na;
  endif

endfunction

%!assert(all (all (union ([1, 2, 4], [2, 3, 5]) == [1, 2, 3, 4, 5])));

%!assert(all (all (union ([1; 2; 4], [2, 3, 5]) == [1, 2, 3, 4, 5])));

%!assert(all (all (union ([1, 2, 3], [5; 7; 9]) == [1, 2, 3, 5, 7, 9])));

%!error union (1);

%!error union (1, 2, 3);

%!test
%! a = [3, 1, 4, 1, 5]; b = [1, 2, 3, 4];
%! [y, ia, ib] = union (a, b.');
%! assert(y, [1, 2, 3, 4, 5]);
%! assert(y, sort([a(ia), b(ib)]));
