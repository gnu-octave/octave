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
## @deftypefn  {Function File} {} unique (@var{x})
## @deftypefnx {Function File} {} unique (@var{x}, "rows")
## @deftypefnx {Function File} {} unique (@dots{}, "first")
## @deftypefnx {Function File} {} unique (@dots{}, "last")
## @deftypefnx {Function File} {[@var{y}, @var{i}, @var{j}] =} unique (@dots{})
## Return the unique elements of @var{x}, sorted in ascending order.
## If the input @var{x} is a vector then the output is also a vector with the
## same orientation (row or column) as the input.  For a matrix input the
## output is always a column vector.  @var{x} may also be a cell array of
## strings.
##
## If the optional argument @code{"rows"} is supplied, return the unique
## rows of @var{x}, sorted in ascending order.
##
## If requested, return index vectors @var{i} and @var{j} such that
## @code{x(i)==y} and @code{y(j)==x}.
##
## Additionally, if @var{i} is a requested output then one of @code{"first"} or
## @code{"last"} may be given as an input.  If @code{"last"} is specified,
## return the highest possible indices in @var{i}, otherwise, if @code{"first"}
## is specified, return the lowest.  The default is @code{"last"}.
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
      optfirst = strmatch ("first", varargin, "exact") > 0;
      optlast = strmatch ("last", varargin, "exact") > 0;
      optrows = strmatch ("rows", varargin, "exact") > 0;
      if (optfirst && optlast)
        error ('unique: cannot specify both "last" and "first"');
      elseif (optfirst + optlast + optrows != nargin-1)
        error ("unique: invalid option");
      endif
    else
      error ("unique: options must be strings");
    endif

    if (optrows && iscell (x))
      warning ('unique: "rows" is ignored for cell arrays');
      optrows = false;
    endif
  else
    optfirst = false;
    optrows = false;
  endif

  ## FIXME -- the operations
  ##
  ##   match = (y(1:n-1) == y(2:n));
  ##   y(idx) = [];
  ##
  ## are very slow on sparse matrices.  Until they are fixed to be as
  ## fast as for full matrices, operate on the nonzero elements of the
  ## sparse array as long as we are not operating on rows.

  if (issparse (x) && ! optrows && nargout <= 1)
    if (nnz (x) < numel (x))
      y = unique ([0; (full (nonzeros (x)))], varargin{:});
    else
      ## Corner case where sparse matrix is actually full
      y = unique (full (x), varargin{:});
    endif
    return;
  endif

  if (optrows)
    n = rows (x);
    dim = 1;
  else
    n = numel (x);
    dim = (rows (x) == 1) + 1;
  endif

  y = x;
  ## Special cases 0 and 1
  if (n == 0)
    if (! optrows && isempty (x) && any (size (x)))
      if (iscell (y))
        y = cell (0, 1);
      else
        y = zeros (0, 1, class (y));
      endif
    endif
    i = j = [];
    return;
  elseif (n == 1)
    i = j = 1;
    return;
  endif

  if (optrows)
    if (nargout > 1)
      [y, i] = sortrows (y);
    else
      y = sortrows (y);
    endif
    match = all (y(1:n-1,:) == y(2:n,:), 2);
    idx = find (match);
    y(idx,:) = [];
  else
    if (! isvector (y))
      y = y(:);
    endif
    if (nargout > 1)
      [y, i] = sort (y);
    else
      y = sort (y);
    endif
    if (iscell (y))
      match = strcmp (y(1:n-1), y(2:n));
    else
      match = (y(1:n-1) == y(2:n));
    endif
    idx = find (match);
    y(idx) = [];
  endif

  if (isargout (3))
    j = i;
    if (dim == 1)
      j(i) = cumsum ([1; !match]);
    else
      j(i) = cumsum ([1, !match]);
    endif
  endif

  if (isargout (2))
    if (optfirst)
      i(idx+1) = [];
    else
      i(idx) = [];
    endif
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
%!assert(unique(sparse([2,0;2,0])), [0,2]')
%!assert(unique(sparse([1,2;2,3])), [1,2,3]')
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
