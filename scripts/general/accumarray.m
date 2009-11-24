## Copyright (C) 2007, 2008, 2009 David Bateman
## Copyright (C) 2009 VZLU Prague
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
## @deftypefn {Function File} {} accumarray (@var{subs}, @var{vals}, @var{sz}, @var{func}, @var{fillval}, @var{issparse})
## @deftypefnx {Function File} {} accumarray (@var{csubs}, @var{vals}, @dots{})
##
## Create an array by accumulating the elements of a vector into the
## positions defined by their subscripts.  The subscripts are defined by
## the rows of the matrix @var{subs} and the values by @var{vals}.  Each row
## of @var{subs} corresponds to one of the values in @var{vals}.
##
## The size of the matrix will be determined by the subscripts themselves.
## However, if @var{sz} is defined it determines the matrix size.  The length
## of @var{sz} must correspond to the number of columns in @var{subs}.
##
## The default action of @code{accumarray} is to sum the elements with the
## same subscripts.  This behavior can be modified by defining the @var{func}
## function.  This should be a function or function handle that accepts a 
## column vector and returns a scalar.  The result of the function should not
## depend on the order of the subscripts.
##
## The elements of the returned array that have no subscripts associated with
## them are set to zero.  Defining @var{fillval} to some other value allows
## these values to be defined.
##
## By default @code{accumarray} returns a full matrix.  If @var{issparse} is
## logically true, then a sparse matrix is returned instead.
##
## An example of the use of @code{accumarray} is:
##
## @example
## @group
## accumarray ([1,1,1;2,1,2;2,3,2;2,1,2;2,3,2], 101:105)
## @result{} ans(:,:,1) = [101, 0, 0; 0, 0, 0]
##    ans(:,:,2) = [0, 0, 0; 206, 0, 208]
## @end group
## @end example
## @end deftypefn

function A = accumarray (subs, val, sz, func, fillval, isspar)  

  if (nargin < 2 || nargin > 6)
    print_usage ();
  endif

  if (iscell (subs))
    subs = cell2mat (cellfun (@(x) x(:), subs, "UniformOutput", false));
  endif
  ndims = size (subs, 2);

  if (nargin < 5 || isempty (fillval))
    fillval = 0;
  endif

  if (nargin < 6 || isempty (isspar))
    isspar = false;
  endif

  if (isspar && ndims > 2)
    error ("accumarray: sparse matrices limited to 2 dimensions");
  endif

  if (nargin < 4 || isempty (func))
    func = @sum;
    ## This is the fast summation case. Unlike the general case,
    ## this case will be handled using an O(N) algorithm.

    if (isspar && fillval == 0)
      ## The "sparse" function can handle this case.

      if ((nargin < 3 || isempty (sz)))
        A = sparse (subs(:,1), subs(:,2), val);
      elseif (length (sz) == 2)
        A = sparse (subs(:,1), subs(:,2), val, sz(1), sz(2));
      else
        error ("accumarray: dimensions mismatch")
      endif
    else
      ## This case is handled by an internal function.

      if (ndims > 1)
        if ((nargin < 3 || isempty (sz)))
          sz = max (subs);
        elseif (ndims != length (sz))
          error ("accumarray: dimensions mismatch")
        elseif (any (max (subs) > sz))
          error ("accumarray: index out of range")
        endif

        ## Convert multidimensional subscripts.
        subs = sub2ind (sz, num2cell (subs, 1){:});
      elseif (nargin < 3)
        ## In case of linear indexing, the fast built-in accumulator
        ## will determine the extent for us.
        sz = [];
      endif

      ## Call the built-in accumulator.
      if (isempty (sz))
        A = __accumarray_sum__ (subs, val);
      else
        A = __accumarray_sum__ (subs, val, prod (sz));
        ## set proper shape.
        A = reshape (A, sz);
      endif

      ## we fill in nonzero fill value.
      if (fillval != 0)
        mask = true (size (A));
        mask(subs) = false;
        A(mask) = fillval;
      endif
    endif

    return
  endif

  if (nargin < 3 || isempty (sz))
    sz = max (subs);
    if (isscalar(sz))
      sz = [sz, 1];
    endif
  elseif (length (sz) != ndims
	  && (ndims != 1 || length (sz) != 2 || sz(2) != 1))
    error ("accumarray: inconsistent dimensions");
  endif
  
  [subs, idx] = sortrows (subs);

  if (isscalar (val))
    val = repmat (size (idx));
  else
    val = val(idx);
  endif
  cidx = find ([true; (sum (abs (diff (subs)), 2) != 0)]);
  idx = cell (1, ndims);
  for i = 1:ndims
    idx{i} = subs (cidx, i);
  endfor
  x = cellfun (func, mat2cell (val(:), diff ([cidx; length(val) + 1])));
  if (isspar && fillval == 0)
    A = sparse (idx{1}, idx{2}, x, sz(1), sz(2));
  else
    if (iscell (x))
      ## Why did matlab choose to reverse the order of the elements
      x = cellfun (@(x) flipud (x(:)), x, "UniformOutput", false);
      A = cell (sz);
    elseif (fillval == 0)
      A = zeros (sz, class (x));
    else 
      A = fillval .* ones (sz);
    endif
    A(sub2ind (sz, idx{:})) = x;
  endif
endfunction

%!error (accumarray (1:5))
%!error (accumarray ([1,2,3],1:2))
%!assert (accumarray ([1;2;4;2;4],101:105), [101;206;0;208])
%!assert (accumarray ([1,1,1;2,1,2;2,3,2;2,1,2;2,3,2],101:105),cat(3, [101,0,0;0,0,0],[0,0,0;206,0,208]))
%!assert (accumarray ([1,1,1;2,1,2;2,3,2;2,1,2;2,3,2],101:105,[],@(x)sin(sum(x))),sin(cat(3, [101,0,0;0,0,0],[0,0,0;206,0,208])))
%!assert (accumarray ({[1 3 3 2 3 1 2 2 3 3 1 2],[3 4 2 1 4 3 4 2 2 4 3 4],[1 1 2 2 1 1 2 1 1 1 2 2]},101:112),cat(3,[0,0,207,0;0,108,0,0;0,109,0,317],[0,0,111,0;104,0,0,219;0,103,0,0]))
%!assert (accumarray ([1,1;2,1;2,3;2,1;2,3],101:105,[2,4],@max,NaN),[101,NaN,NaN,NaN;104,NaN,105,NaN])
%!assert (accumarray ([1 1; 2 1; 2 3; 2 1; 2 3],101:105,[2 4],@prod,0,true),sparse([1,2,2],[1,1,3],[101,10608,10815],2,4))
%!assert (accumarray ([1 1; 2 1; 2 3; 2 1; 2 3],1,[2,4]), [1,0,0,0;2,0,2,0])
%!assert (accumarray ([1 1; 2 1; 2 3; 2 1; 2 3],101:105,[2,4],@(x)length(x)>1),[false,false,false,false;true,false,true,false])
%!test
%! A = accumarray ([1 1; 2 1; 2 3; 2 1; 2 3],101:105,[2,4],@(x){x});
%! assert (A{2},[104;102])
