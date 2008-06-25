## Copyright (C) 2000, 2001, 2005, 2006, 2007 Paul Kienzle
## Copyright (C) 2008 Jaroslav Hajek
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
##
## Return the unique elements of @var{x}, sorted in ascending order.
## If @var{x} is a row vector, return a row vector, but if @var{x}
## is a column vector or a matrix return a column vector.
##
## @deftypefnx {Function File} {} unique (@var{A}, 'rows')
##
## Return the unique rows of @var{A}, sorted in ascending order.
##
## @deftypefnx {Function File} {[@var{y}, @var{i}, @var{j}] =} unique (@var{x})
##
## Return index vectors @var{i} and @var{j} such that @code{x(i)==y} and
## @code{y(j)==x}.
## 
## Additionally, one of 'first' or 'last' can be given as an argument.
## 'last' (default) specifies that the highest possible indices are returned
## in @var{i}, while 'first' means the lowest.
## @seealso{union, intersect, setdiff, setxor, ismember}
## @end deftypefn

function [y, i, j] = unique (x, varargin)

  if (nargin < 1)
    print_usage ();
  endif

  ## parse options
  if (iscellstr (varargin))
    optfirst = strmatch ('first', varargin) > 0;
    optlast = strmatch ('last', varargin) > 0;
    optrows = strmatch ('rows', varargin) > 0 && size (x, 2) > 1;
    if (optfirst && optlast)
      error ("unique: cannot specify both 'last' and 'first'.");
    elseif (optfirst + optlast + optrows != nargin-1)
      error ("unique: invalid option.");
    endif
    optlast = ! optfirst;
  else
    error ("unique: options must be strings");
  endif

  if (iscell (x))
    if (optrows)
      warning ("unique: 'rows' is ignored for cell arrays");
      optrows = false;
    endif
  endif

  if (optrows)
    n = size (x, 1);
  else
    n = numel (x);
  endif

  y = x;
  if (n < 1)
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

  ## I don't know why anyone would need reverse indices, but it
  ## was an interesting challenge.  I welcome cleaner solutions.
  if (nargout >= 3)
    j = i;
    j(i) = cumsum (prepad (! match, n, 1));
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
%!assert(unique({'Foo','Bar','FooBar'}),{'Bar','Foo','FooBar'})

%!test
%! [a,i,j] = unique([1,1,2,3,3,3,4]);
%! assert(a,[1,2,3,4])
%! assert(i,[2,3,6,7])
%! assert(j,[1,1,2,3,3,3,4])
%!
%!test
%! [a,i,j] = unique([1,1,2,3,3,3,4],'first');
%! assert(a,[1,2,3,4])
%! assert(i,[1,3,4,7])
%! assert(j,[1,1,2,3,3,3,4])
