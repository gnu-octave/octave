## Copyright (C) 2008-2012 Jaroslav Hajek
## Copyright (C) 2000, 2006-2007 Paul Kienzle
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by the
## Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but WITHOUT
## ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
## FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
## for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {Function File} {} setxor (@var{a}, @var{b})
## @deftypefnx {Function File} {} setxor (@var{a}, @var{b}, 'rows')
## @deftypefnx {Function File} {[@var{c}, @var{ia}, @var{ib}] =} setxor (@var{a}, @var{b})
##
## Return the elements exclusive to @var{a} or @var{b}, sorted in ascending
## order.  If @var{a} and @var{b} are both column vectors return a column
## vector, otherwise return a row vector.
## @var{a}, @var{b} may be cell arrays of string(s).
##
## With three output arguments, return index vectors @var{ia} and @var{ib}
## such that @code{a(ia)} and @code{b(ib)} are disjoint sets whose union
## is @var{c}.
##
## @seealso{unique, union, intersect, setdiff, ismember}
## @end deftypefn

function [c, ia, ib] = setxor (a, b, varargin)

  if (nargin < 2 || nargin > 3)
    print_usage ();
  endif

  [a, b] = validargs ("setxor", a, b, varargin{:});

  ## Form A and B into sets.
  if (nargout > 1)
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
    if (nargin > 2)
      na = rows (a); nb = rows (b);
      [c, i] = sortrows ([a; b]);
      n = rows (c);
      idx = find (all (c(1:n-1) == c(2:n), 2));
      if (! isempty (idx))
        c([idx, idx+1],:) = [];
        i([idx, idx+1],:) = [];
      endif
    else
      na = numel (a); nb = numel (b);
      [c, i] = sort ([a(:); b(:)]);
      n = length (c);
      if (iscell (c))
        idx = find (strcmp (c(1:n-1), c(2:n)));
      else
        idx = find (c(1:n-1) == c(2:n));
      endif
      if (! isempty (idx))
        c([idx, idx+1]) = [];
        i([idx, idx+1]) = [];
      endif
      if (size (a, 1) == 1 || size (b, 1) == 1)
        c = c.';
      endif
    endif
  endif
  if (nargout > 1)
    ia = ia(i(i <= na));
    ib = ib(i(i > na) - na);
  endif

endfunction

%!assert(setxor([1,2,3],[2,3,4]),[1,4])
%!assert(setxor({'a'}, {'a', 'b'}), {'b'});
%!test
%! a = [3, 1, 4, 1, 5]; b = [1, 2, 3, 4];
%! [y, ia, ib] = setxor (a, b.');
%! assert(y, [2, 5]);
%! assert(y, sort([a(ia), b(ib)]));

