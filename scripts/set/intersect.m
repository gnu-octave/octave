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
## @deftypefn  {Function File} {} intersect (@var{a}, @var{b})
## @deftypefnx {Function File} {[@var{c}, @var{ia}, @var{ib}] =} intersect (@var{a}, @var{b})
##
## Return the elements in both @var{a} and @var{b}, sorted in ascending
## order.  If @var{a} and @var{b} are both column vectors return a column
## vector, otherwise return a row vector.
## @var{a}, @var{b} may be cell arrays of string(s).
##
## Return index vectors @var{ia} and @var{ib} such that @code{a(ia)==c} and
## @code{b(ib)==c}.
##
## @end deftypefn
## @seealso{unique, union, setxor, setdiff, ismember}

function [c, ia, ib] = intersect (a, b, varargin)

  if (nargin < 2 || nargin > 3)
    print_usage ();
  endif

  [a, b] = validargs ("intersect", a, b, varargin{:});

  if (isempty (a) || isempty (b))
    c = ia = ib = [];
  else
    ## form a and b into sets
    if (nargout > 1)
      [a, ja] = unique (a, varargin{:});
      [b, jb] = unique (b, varargin{:});
    else
      a = unique (a, varargin{:});
      b = unique (b, varargin{:});
    endif

    if (nargin > 2)
      c = [a; b];
      [c, ic] = sortrows (c);
      ii = find (all (c(1:end-1,:) == c(2:end,:), 2));
      c = c(ii,:);
      len_a = rows (a);
    else
      c = [a(:); b(:)];
      [c, ic] = sort (c);               ## [a(:);b(:)](ic) == c
      if (iscellstr (c))
        ii = find (strcmp (c(1:end-1), c(2:end)));
      else
        ii = find (c(1:end-1) == c(2:end));
      endif
      c = c(ii);
      len_a = length (a);
    endif

    if (nargout > 1)
      ia = ja(ic(ii));                  ## a(ia) == c
      ib = jb(ic(ii+1) - len_a);        ## b(ib) == c
    endif

    if (nargin == 2 && (size (b, 1) == 1 || size (a, 1) == 1))
      c = c.';
    endif
  endif

endfunction


%!# Test the routine for index vectors ia and ib
%!test
%! a = [3 2 4 5 7 6 5 1 0 13 13];
%! b = [3 5 12 1 1 7];
%! [c,ia,ib] = intersect(a,b);
%! assert(c,[1 3 5 7]);
%! assert(ia,[8 1 7 5]);
%! assert(ib,[5 1 2 6]);
%! assert(a(ia),c);
%! assert(b(ib),c);
%!test
%! a = [1,1,2;1,4,5;2,1,7];
%! b = [1,4,5;2,3,4;1,1,2;9,8,7];
%! [c,ia,ib] = intersect(a,b,'rows');
%! assert(c,[1,1,2;1,4,5]);
%! assert(ia,[1;2]);
%! assert(ib,[3;1]);
%! assert(a(ia,:),c);
%! assert(b(ib,:),c);
%!test
%! a = [1 1 1 2 2 2];
%! b = [1 2 3 4 5 6];
%! c = intersect(a,b);
%! assert(c, [1,2]);
%!test
%! a = [1 2 3 4; 5 6 7 8; 9 10 11 12];
%! [b, ia, ib] = intersect(a, a, "rows");
%! assert(b, a);
%! assert(ia, [1:3]');
%! assert(ib, [1:3]');
