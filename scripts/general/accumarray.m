## Copyright (C) 2007, 2008, 2009 David Bateman
## Copyright (C) 2009, 2010 VZLU Prague
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
## @deftypefn  {Function File} {} accumarray (@var{subs}, @var{vals}, @var{sz}, @var{func}, @var{fillval}, @var{issparse})
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
##
## The complexity in the non-sparse case is generally O(M+N), where N is the
## number of
## subscripts and M is the maximum subscript (linearized in multi-dimensional
## case).
## If @var{func} is one of @code{@@sum} (default), @code{@@max}, @code{@@min}
## or @code{@@(x) @{x@}}, an optimized code path is used. 
## Note that for general reduction function the interpreter overhead can play a
## major part and it may be more efficient to do multiple accumarray calls and
## compute the results in a vectorized manner.
## @end deftypefn

function A = accumarray (subs, val, sz = [], func = [], fillval = [], isspar = [])  

  if (nargin < 2 || nargin > 6)
    print_usage ();
  endif

  if (iscell (subs))
    subs = cellfun (@(x) x(:), subs, "UniformOutput", false);
    ndims = numel (subs);
    if (ndims == 1)
      subs = subs{1};
    endif
  else
    ndims = columns (subs);
  endif

  if (isempty (fillval))
    fillval = 0;
  endif

  if (isempty (isspar))
    isspar = false;
  endif

  if (isspar)

    ## Sparse case. Avoid linearizing the subscripts, because it could overflow.

    if (fillval != 0)
      error ("accumarray: fillval must be zero in the sparse case");
    endif

    ## Ensure subscripts are a two-column matrix.
    if (iscell (subs))
      subs = [subs{:}];
    endif

    ## Validate dimensions.
    if (ndims == 1)
      subs(:,2) = 1;
    elseif (ndims != 2)
      error ("accumarray: in the sparse case, needs 1 or 2 subscripts");
    endif

    if (isnumeric (val) || islogical (val))
      vals = double (val);
    else
      error ("accumarray: in the sparse case, values must be numeric or logical");
    endif

    if (! (isempty (func) || func == @sum))

      ## Reduce values. This is not needed if we're about to sum them, because
      ## "sparse" can do that.
      
      ## Sort indices.
      [subs, idx] = sortrows (subs);
      n = rows (subs);
      ## Identify runs.
      jdx = find (any (diff (subs, 1, 1), 2));
      jdx = [jdx; n];

      val = cellfun (func, mat2cell (val(:)(idx), diff ([0; jdx])));
      subs = subs(jdx, :);
      mode = "unique";
    else
      mode = "sum";
    endif

    ## Form the sparse matrix.
    if (isempty (sz))
      A = sparse (subs(:,1), subs(:,2), val, mode);
    elseif (length (sz) == 2)
      A = sparse (subs(:,1), subs(:,2), val, sz(1), sz(2), mode);
    else
      error ("accumarray: dimensions mismatch")
    endif

  else

    ## Linearize subscripts.
    if (ndims > 1)
      if (isempty (sz))
        if (iscell (subs))
          sz = cellfun (@max, subs);
        else
          sz = max (subs, [], 1);
        endif
      elseif (ndims != length (sz))
        error ("accumarray: dimensions mismatch")
      endif

      ## Convert multidimensional subscripts.
      if (ismatrix (subs))
        subs = num2cell (subs, 1);
      endif
      subs = sub2ind (sz, subs{:}); # creates index cache
    elseif (! isempty (sz) && length (sz) < 2)
      error ("accumarray: needs at least 2 dimensions");
    elseif (! isindex (subs)) # creates index cache
      error ("accumarray: indices must be positive integers");
    endif


    ## Some built-in reductions handled efficiently.

    if (isempty (func) || func == @sum)
      ## Fast summation.
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
    elseif (func == @max)
      ## Fast maximization.

      if (isinteger (val))
        zero = intmin (class (val));
      elseif (islogical (val))
        zero = false;
      elseif (fillval == 0 && all (val(:) >= 0))
        ## This is a common case - fillval is zero, all numbers nonegative.
        zero = 0;
      else
        zero = NaN; # Neutral value.
      endif

      if (isempty (sz))
        A = __accumarray_max__ (subs, val, zero);
      else
        A = __accumarray_max__ (subs, val, zero, prod (sz));
        A = reshape (A, sz);
      endif

      if (fillval != zero && ! (isnan (fillval) || isnan (zero)))
        mask = true (size (A));
        mask(subs) = false;
        A(mask) = fillval;
      endif
    elseif (func == @min)
      ## Fast minimization.

      if (isinteger (val))
        zero = intmax (class (val));
      elseif (islogical (val))
        zero = true;
      else
        zero = NaN; # Neutral value.
      endif

      if (isempty (sz))
        A = __accumarray_min__ (subs, val, zero);
      else
        A = __accumarray_min__ (subs, val, zero, prod (sz));
        A = reshape (A, sz);
      endif

      if (fillval != zero && ! (isnan (fillval) || isnan (zero)))
        mask = true (size (A));
        mask(subs) = false;
        A(mask) = fillval;
      endif
    else

      ## The general case. Reduce values. 
      n = rows (subs);
      if (numel (val) == 1)
        val = val(ones (1, n), 1);
      else
        val = val(:);
      endif
      
      ## Sort indices.
      [subs, idx] = sort (subs);
      ## Identify runs.
      jdx = find (subs(1:n-1) != subs(2:n));
      jdx = [jdx; n];
      val = mat2cell (val(idx), diff ([0; jdx]));
      ## Optimize the case when function is @(x) {x}, i.e. we just want to
      ## collect the values to cells.
      persistent simple_cell_str = func2str (@(x) {x});
      if (! strcmp (func2str (func), simple_cell_str))
        val = cellfun (func, val);
      endif
      subs = subs(jdx);

      ## Construct matrix of fillvals.
      if (iscell (val))
        A = cell (sz);
      elseif (fillval == 0)
        A = zeros (sz, class (val));
      else
        A = repmat (fillval, sz);
      endif

      ## Set the reduced values.
      A(subs) = val;
    endif
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
%! assert (A{2},[102;104])
%!test
%! subs = ceil (rand (2000, 3)*10);
%! val = rand (2000, 1);
%! assert (accumarray (subs, val, [], @max), accumarray (subs, val, [], @(x) max (x)));
%!test
%! subs = ceil (rand (2000, 1)*100);
%! val = rand (2000, 1);
%! assert (accumarray (subs, val, [100, 1], @min, NaN), accumarray (subs, val, [100, 1], @(x) min (x), NaN));
%!test
%! subs = ceil (rand (2000, 2)*30);
%! subsc = num2cell (subs, 1);
%! val = rand (2000, 1);
%! assert (accumarray (subsc, val, [], [], 0, true), accumarray (subs, val, [], [], 0, true));
%!test
%! subs = ceil (rand (2000, 3)*10);
%! subsc = num2cell (subs, 1);
%! val = rand (2000, 1);
%! assert (accumarray (subsc, val, [], @max), accumarray (subs, val, [], @max));


