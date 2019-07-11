## Copyright (C) 2000-2019 Paul Kienzle
## Copyright (C) 2008-2009 Jaroslav Hajek
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

## -*- texinfo -*-
## @deftypefn  {} {@var{c} =} intersect (@var{a}, @var{b})
## @deftypefnx {} {@var{c} =} intersect (@var{a}, @var{b}, "rows")
## @deftypefnx {} {@var{c} =} intersect (@dots{}, "legacy")
## @deftypefnx {} {[@var{c}, @var{ia}, @var{ib}] =} intersect (@dots{})
##
## Return the unique elements common to both @var{a} and @var{b} sorted in
## ascending order.
##
## If @var{a} and @var{b} are both row vectors then return a row vector;
## Otherwise, return a column vector.  The inputs may also be cell arrays of
## strings.
##
## If the optional input @qcode{"rows"} is given then return the common rows of
## @var{a} and @var{b}.  The inputs must be 2-D matrices to use this option.
##
## If requested, return column index vectors @var{ia} and @var{ib} such that
## @code{@var{c} = @var{a}(@var{ia})} and @code{@var{c} = @var{b}(@var{ib})}.
##
## Programming Note: The input flag @qcode{"legacy"} changes the algorithm
## to be compatible with @sc{matlab} releases prior to R2012b.
##
## @seealso{unique, union, setdiff, setxor, ismember}
## @end deftypefn

function [c, ia, ib] = intersect (a, b, varargin)

  if (nargin < 2 || nargin > 4)
    print_usage ();
  endif

  [a, b] = validsetargs ("intersect", a, b, varargin{:});

  if (isempty (a) || isempty (b))
    ## Special case shortcuts algorithm.
    ## Lots of type checking required for Matlab compatibility.
    if (isnumeric (a) && isnumeric (b))
      c = [];
    elseif (iscell (b))
      c = {};
    else
      c = "";
    endif
    ia = ib = [];
  else
    by_rows = any (strcmp ("rows", varargin));
    optlegacy = any (strcmp ("legacy", varargin));

    if (optlegacy)
      isrowvec = ! iscolumn (a) || ! iscolumn (b);
    else
      isrowvec = isrow (a) && isrow (b);
    endif

    ## Form A and B into sets
    if (nargout > 1)
      [a, ja] = unique (a, varargin{:});
      ja = ja(:);
      [b, jb] = unique (b, varargin{:});
      jb = jb(:);
    else
      a = unique (a, varargin{:});
      b = unique (b, varargin{:});
    endif

    if (by_rows)
      c = [a; b];
      if (nargout > 1)
        [c, ic] = sortrows (c);
      else
        c = sortrows (c);
      endif
      ii = find (all (c(1:end-1,:) == c(2:end,:), 2));
      c = c(ii,:);
      len_a = rows (a);
    else
      c = [a(:); b(:)];
      if (nargout > 1)
        [c, ic] = sort (c);         # [a(:);b(:)](ic) == c
      else
        c = sort (c);
      endif
      if (iscellstr (c))
        ii = find (strcmp (c(1:end-1), c(2:end)));
      else
        ii = find (c(1:end-1) == c(2:end));
      endif
      c = c(ii);
      len_a = length (a);
    endif

    ## Adjust output orientation for Matlab compatibility
    if (isrowvec)
      c = c.';
    endif

    if (nargout > 1)
      ia = ja(ic(ii));            # a(ia) == c
      ib = jb(ic(ii+1) - len_a);  # b(ib) == c
      if (optlegacy && isrowvec)
        ia = ia.';
        ib = ib.';
      endif
    endif

  endif

endfunction


## Test orientation of output
%!shared a,b
%! a = 1:4;
%! b = 2:5;

%!assert (size (intersect (a, b)), [1, 3])
%!assert (size (intersect (a', b)), [3, 1])
%!assert (size (intersect (a, b')), [3, 1])
%!assert (size (intersect (a', b')), [3, 1])
%!assert (size (intersect (a, b, "legacy")), [1, 3])
%!assert (size (intersect (a', b, "legacy")), [1, 3])
%!assert (size (intersect (a, b', "legacy")), [1, 3])
%!assert (size (intersect (a', b', "legacy")), [3, 1])

## Clear shared variables
%!shared

## Test multi-dimensional arrays
%!test
%! a = rand (3,3,3);
%! b = a;
%! b(1,1,1) = 2;
%! assert (intersect (a, b), sort (a(2:end)'));

## Test the routine for index vectors ia and ib
%!test
%! a = [3 2 4 5 7 6 5 1 0 13 13];
%! b = [3 5 12 1 1 7];
%! [c,ia,ib] = intersect (a, b);
%! assert (c, [1, 3, 5, 7]);
%! assert (ia, [8; 1; 4; 5]);
%! assert (ib, [4; 1; 2; 6]);
%! assert (a(ia), c);
%! assert (b(ib), c);
%!test
%! a = [1,1,2;1,4,5;2,1,7];
%! b = [1,4,5;2,3,4;1,1,2;9,8,7];
%! [c,ia,ib] = intersect (a, b, "rows");
%! assert (c, [1,1,2;1,4,5]);
%! assert (ia, [1;2]);
%! assert (ib, [3;1]);
%! assert (a(ia,:), c);
%! assert (b(ib,:), c);
%!test
%! a = [1 1 1 2 2 2];
%! b = [1 2 3 4 5 6];
%! c = intersect (a, b);
%! assert(c, [1,2]);
%!test
%! a = [1 2 3 4; 5 6 7 8; 9 10 11 12];
%! [b, ia, ib] = intersect (a, a, "rows");
%! assert (b, a);
%! assert (ia, [1:3]');
%! assert (ib, [1:3]');

## Test "legacy" argument
%!test
%! a = [7 1 7 7 4]; 
%! b = [7 0 4 4 0];
%! [c, ia, ib] = intersect (a, b);
%! assert (c, [4, 7]);
%! assert (ia, [5; 1]);
%! assert (ib, [3; 1]);
%! [c, ia, ib] = intersect (a, b, "legacy");
%! assert (c, [4, 7]);
%! assert (ia, [5, 4]);
%! assert (ib, [4, 1]);

## Test return type of empty intersections
%!assert (intersect (['a', 'b'], {}), {})
%!assert (intersect ([], {'a', 'b'}), {})
%!assert (intersect ([], {}), {})
%!assert (intersect ({'a', 'b'}, []), {})
%!assert (intersect ([], ['a', 'b']), "")
%!assert (intersect ({}, []), {})
%!assert (intersect (['a', 'b'], []), "")
