## Copyright (C) 2008, 2009 Jaroslav Hajek
## Copyright (C) 2000, 2001, 2005, 2006, 2007 Paul Kienzle
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
## @deftypefn {Function File} {} unique (@var{x})
## @deftypefnx {Function File} {} unique (@var{x}, "rows")
## @deftypefnx {Function File} {} unique (@dots{}, "first")
## @deftypefnx {Function File} {} unique (@dots{}, "last")
## @deftypefnx {Function File} {[@var{y}, @var{i}, @var{j}] =} unique (@dots{})
## Return the unique elements of @var{x}, sorted in ascending order.
## If @var{x} is a row vector, return a row vector, but if @var{x}
## is a column vector or a matrix return a column vector.
## @var{x} can be a cell array of strings.
##
## If the optional argument @code{"rows"} is supplied, return the unique
## rows of @var{x}, sorted in ascending order.
##
## If requested, return index vectors @var{i} and @var{j} such that
## @code{x(i)==y} and @code{y(j)==x}.
##
## Additionally, one of @code{"first"} or @code{"last"} may be given as
## an argument.  If @code{"last"} is specified, return the highest
## possible indices in @var{i}, otherwise, if @code{"first"} is
## specified, return the lowest.  The default is @code{"last"}.
## @seealso{union, intersect, setdiff, setxor, ismember}
## @end deftypefn

function [y, i, j] = unique (x, varargin)

  if (nargin < 1)
    print_usage ();
  endif

  if (nargin > 1)

    ## parse options
    if (iscellstr (varargin))
      varargin = unique (varargin);
      optfirst = strmatch ("first", varargin) > 0;
      optlast = strmatch ("last", varargin) > 0;
      optrows = strmatch ("rows", varargin) > 0;
      if (optfirst && optlast)
        error ("unique: cannot specify both \"last\" and \"first\"");
      elseif (optfirst + optlast + optrows != nargin-1)
        error ("unique: invalid option");
      endif
    else
      error ("unique: options must be strings");
    endif

    if (optrows && iscell (x))
      warning ("unique: 'rows' is ignored for cell arrays");
      optrows = false;
    endif

  else
    optfirst = 0;
    optrows = 0;
  endif

  if (optrows)
    n = size (x, 1);
    dim = 1;
  else
    n = numel (x);
    dim = (size (x, 1) == 1) + 1;
  endif

  y = x;
  if (n < 1)
    if (! optrows && isempty (x) && any (size (x)))
      if (iscell (y))
        y = cell (0, 1);
      else
        y = zeros (0, 1, class (y));
      endif
    endif
    i = j = [];
    return;
  elseif (n < 2)
    i = j = 1;
    return;
  endif

  if (optrows)
    [y, i] = sortrows (y);
    match = all (y(1:n-1,:) == y(2:n,:), 2);
    idx = find (match);
    y(idx,:) = [];
  else
    if (size (y, 1) != 1)
      y = y(:);
    endif
    [y, i] = sort (y);
    if (iscell (y))
      match = strcmp (y(1:n-1), y(2:n));
    else
      match = (y(1:n-1) == y(2:n));
    endif
    idx = find (match);
    y(idx) = [];
  endif

  if (nargout >= 3)
    j = i;
    if (dim == 1)
      j(i) = cumsum ([1; !match]);
    else
      j(i) = cumsum ([1, !match]);
    endif
  endif

  if (optfirst)
    i(idx+1) = [];
  else
    i(idx) = [];
  endif


endfunction

%!assert(unique([1 1 2; 1 2 1; 1 1 2]),[1;2])
%!assert(unique([1 1 2; 1 0 1; 1 1 2],'rows'),[1 0 1; 1 1 2])
%!assert(unique([]),[])
%!assert(unique([1]),[1])
%!assert(unique([1 2]),[1 2])
%!assert(unique([1;2]),[1;2])
%!assert(unique([1,NaN,Inf,NaN,Inf]),[1,Inf,NaN,NaN])
%!assert(unique({'Foo','Bar','Foo'}),{'Bar','Foo'})
%!assert(unique({'Foo','Bar','FooBar'}'),{'Bar','Foo','FooBar'}')
%!assert(unique(zeros(1,0)), zeros(0,1))
%!assert(unique(zeros(1,0), 'rows'), zeros(1,0))
%!assert(unique(cell(1,0)), cell(0,1))
%!assert(unique({}), {})
%!assert(unique([1,2,2,3,2,4], 'rows'), [1,2,2,3,2,4])
%!assert(unique([1,2,2,3,2,4]), [1,2,3,4])
%!assert(unique([1,2,2,3,2,4]', 'rows'), [1,2,3,4]')
%!assert(unique(single([1,2,2,3,2,4]), 'rows'), single([1,2,2,3,2,4]))
%!assert(unique(single([1,2,2,3,2,4])), single([1,2,3,4]))
%!assert(unique(single([1,2,2,3,2,4]'), 'rows'), single([1,2,3,4]'))
%!assert(unique(uint8([1,2,2,3,2,4]), 'rows'), uint8([1,2,2,3,2,4]))
%!assert(unique(uint8([1,2,2,3,2,4])), uint8([1,2,3,4]))
%!assert(unique(uint8([1,2,2,3,2,4]'), 'rows'), uint8([1,2,3,4]'))
%!test
%! [a,i,j] = unique([1,1,2,3,3,3,4]);
%! assert(a,[1,2,3,4])
%! assert(i,[2,3,6,7])
%! assert(j,[1,1,2,3,3,3,4])
%!
%!test
%! [a,i,j] = unique([1,1,2,3,3,3,4]','first');
%! assert(a,[1,2,3,4]')
%! assert(i,[1,3,4,7]')
%! assert(j,[1,1,2,3,3,3,4]')
%!
%!test
%! [a,i,j] = unique({'z'; 'z'; 'z'});
%! assert(a,{'z'})
%! assert(i,[3]')
%! assert(j,[1,1,1]')
%!
%!test
%! A=[1,2,3;1,2,3];
%! [a,i,j] = unique(A,'rows');
%! assert(a,[1,2,3])
%! assert(A(i,:),a)
%! assert(a(j,:),A)
