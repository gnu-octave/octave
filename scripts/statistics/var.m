########################################################################
##
## Copyright (C) 1995-2021 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
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
##
########################################################################

## -*- texinfo -*-
## @deftypefn  {} {} var (@var{x})
## @deftypefnx {} {} var (@var{x}, @var{w})
## @deftypefnx {} {} var (@var{x}, @var{w}, @var{dim})
## @deftypefnx {} {} var (@var{x}, @var{w}, @qcode{"ALL"})
## Compute the variance of the elements of the vector @var{x}.
##
## The variance is defined as
## @tex
## $$
## {\rm var} (x) = \sigma^2 = {\sum_{i=1}^N (x_i - \bar{x})^2 \over N - 1}
## $$
## where $\bar{x}$ is the mean value of @var{x} and $N$ is the number of
## elements of @var{x}.
##
## @end tex
## @ifnottex
##
## @example
## @group
## var (@var{x}) = 1/(N-1) SUM_i (@var{x}(i) - mean(@var{x}))^2
## @end group
## @end example
##
## @noindent
## where @math{N} is the length of the @var{x} vector.
##
## @end ifnottex
## If @var{x} is an array, compute the variance for each column and return
## them in a row vector (or for an n-D array, the result is returned as
## an array of dimension 1 x n x m x @dots{}).
##
## The optional argument @var{w} determines the weighting scheme to use.  Valid
## values are
##
## @table @asis
## @item 0 [default]:
## Normalize with @math{N-1}.  This provides the square root of the best
## unbiased estimator of the variance.
##
## @item 1:
## Normalize with @math{N}, this provides the square root of the second moment
## around the mean
##
## @item a vector:
## Compute the weighted variance with nonnegative scalar weights.  The length of
## @var{w} must be equal to the size of @var{x} along dimension @var{dim}.
## @end table
##
## If @math{N} is equal to 1 the value of @var{W} is ignored and
## normalization by @math{N} is used.
##
## The optional variable @var{dim} forces @code{var} to operate over the
## specified dimension.  @var{dim} can either be a scalar dimension or a vector
## of non-repeating dimensions over which to operate.  Dimensions must be
## positive integers, and the variance is calculated over the array slice
## defined by @var{dim}.
##
## Specifying dimension @qcode{"ALL"} will force @code{var} to operate on all
## elements of @var{x}, and is equivalent to @code{var (@var{x}(:))}.
##
## When @var{dim} is a vector or @qcode{"ALL"}, @var{w} must be either 0 or 1.
## @seealso{cov, std, skewness, kurtosis, moment}
## @end deftypefn

function retval = var (x, w = 0, dim)

  if (nargin < 1)
    print_usage ();
  elseif (nargin < 3)
    dim = [];
  endif

  if (! (isnumeric (x) || islogical (x)))
    error ("var: X must be a numeric vector or matrix");
  endif

  nd = ndims (x);
  sz = size (x);
  emptydimflag = false;

  if (isempty (dim))
    emptydimflag = true;  ## Compatibliity hack for empty x, ndims==2
    ## Find the first non-singleton dimension.
   (dim = find (sz != 1, 1)) || (dim = 1);

  else
    if (! (isscalar (dim) && dim == fix (dim) && dim > 0))
      if (isvector (dim) &&
          isnumeric (dim) &&
          all (dim > 0) &&
          all (rem (dim, 1) == 0))
        if (dim != unique (dim, "stable"))
          error (["var: vector DIM must contain non-repeating positive"...
                  "integers"]);
        endif
        ## Check W
        if (! isscalar (w))
          error ("var: W must be either 0 or 1 when DIM is a vector");
        endif

        ## Reshape X to compute the variance over an array slice
        if (iscolumn (dim))
          dim = transpose (dim);
        endif

        collapsed_dims = dim;
        dim = dim(end);

        ## Permute X to cluster the dimensions to collapse
        highest_dim = max ([nd, collapsed_dims]);
        perm_start = perm_end = [1:highest_dim];
        perm_start(dim:end) = [];
        perm_start(ismember (perm_start, collapsed_dims)) = [];
        perm_end(1:dim) = [];
        perm_end(ismember (perm_end, collapsed_dims)) = [];
        perm = [perm_start, collapsed_dims, perm_end];

        x = permute (x, perm);

        ## Collapse the given dimensions
        newshape = ones (1, highest_dim);
        newshape(1:nd) = sz;
        newshape(collapsed_dims(1:(end - 1))) = 1;
        newshape(dim) = prod (sz(collapsed_dims));

        ## New X with collapsed dimensions
        x = reshape (x, newshape);
      elseif (ischar (dim) &&
              strcmp (tolower (dim), "all"))
        ## Check W
        if (! isscalar (w))
          error ("var: W must be either 0 or 1 when using 'ALL' as dimension");
        endif

        ## "ALL" equals to collapsing all elements to a single vector
        x = x(:);
        dim = 1;
        sz = size (x);
      else
        error ("var: DIM must be a positive integer scalar, vector, or 'all'");
      endif
    endif
  endif

  n = size (x, dim);
  if (isempty (w))
    w = 0;
  elseif (! isvector (w) ||
          ! isnumeric (w) ||
          (isvector (w) && any (w < 0)) ||
          (isscalar (w) && ((w != 0 && w != 1) && (n != 1))))
    error ("var: W must be 0, 1, or a vector of positive integers");
  endif

  if (isempty (x))
    if (emptydimflag && isequal (sz, [0 0]))
      retval = NaN;
    else
      output_size = sz;
      output_size(dim) = 1;
      retval = NaN(output_size);
    endif
  else
    if (n == 1)
      if (! isscalar (w))
        error (["var: the length of W must be equal to the size of X "...
                  "in the dimension along which variance is calculated"])
      else
        if (isa (x, "single"))
          retval = zeros (sz, "single");
        else
          retval = zeros (sz);
        endif
      endif
    else
      if (isscalar (w))
        retval = sumsq (center (x, dim), dim) / (n - 1 + w);
      else
        ## Weighted variance
        if (length (w) != n)
          error (["var: the length of W must be equal to the size of X "...
                  "in the dimension along which variance is calculated"]);
        else
          if ((dim == 1 && rows (w) == 1) ||
              (dim == 2 && columns (w) == 1))
            w = transpose (w);
          elseif (dim > 2)
            newdims = [(ones (1, (dim - 1))), (length (w))];
            w = reshape (w, newdims);
          endif
          den = sum (w);
          mu = sum (w .* x, dim) ./ sum (w);
          retval = sum (w .* ((x - mu) .^ 2), dim) / den;
        endif
      endif
    endif
  endif

endfunction

%!assert (var (13), 0)
%!assert (var (single (13)), single (0))
%!assert (var ([1,2,3]), 1)
%!assert (var ([1,2,3], 1), 2/3, eps)
%!assert (var ([1,2,3], [], 1), [0,0,0])
%!assert (var ([1,2,3], [], 3), [0,0,0])
%!assert (var (5, 99), 0)
%!assert (var (5, 99, 1), 0)
%!assert (var (5, 99, 2), 0)
%!assert (var ([1:7], [1:7]), 3)
%!assert (var ([eye(3)], [1:3]), [5/36, 2/9, 1/4], eps)
%!assert (var (ones (2,2,2), [1:2], 3), [(zeros (2,2))])
%!assert (var ([1 2; 3 4], 0, 'all'), var ([1:4]))
%!assert (var (reshape ([1:8], 2, 2, 2), 0, [1 3]), [17/3 17/3], eps)

##Test empty inputs
%!assert (var ([]), NaN)
%!assert (var ([],[],1), NaN(1,0))
%!assert (var ([],[],2), NaN(0,1))
%!assert (var ([],[],3), [])
%!assert (var (ones (0,1)), NaN)
%!assert (var (ones (1,0)), NaN)
%!assert (var (ones (1,0), [], 1), NaN(1,0))
%!assert (var (ones (1,0), [], 2), NaN)
%!assert (var (ones (1,0), [], 3), NaN(1,0))
%!assert (var (ones (0,1)), NaN)
%!assert (var (ones (0,1), [], 1), NaN)
%!assert (var (ones (0,1), [], 2), NaN(0,1))
%!assert (var (ones (0,1), [], 3), NaN(0,1))
%!assert (var (ones (1,3,0,2)), NaN(1,1,0,2))
%!assert (var (ones (1,3,0,2), [], 1), NaN(1,3,0,2))
%!assert (var (ones (1,3,0,2), [], 2), NaN(1,1,0,2))
%!assert (var (ones (1,3,0,2), [], 3), NaN(1,3,1,2))
%!assert (var (ones (1,3,0,2), [], 4), NaN(1,3,0))

## Test input validation
%!error <Invalid call> var ()
%!error <X must be a numeric> var (['A'; 'B'])
%!error <W must be 0> var ([1 2 3], 2)
%!error <W must be .* a vector of positive integers> var ([1 2], [-1 0])
%!error <W must be .* a vector of positive integers> var ([1 2], eye (2))
%!error <W must be either 0 or 1> var (ones (2, 2), [1 2], [1 2])
%!error <W must be either 0 or 1> var ([1 2], [1 2], 'all')
%!error <the length of W must be> var ([1 2], [1 2 3])
%!error <the length of W must be> var (1, [1 2])
%!error <the length of W must be> var ([1 2], [1 2], 1)
%!error <DIM must be a positive integer> var (1, [], ones (2,2))
%!error <DIM must be a positive integer> var (1, [], 1.5)
%!error <DIM must be a positive integer> var (1, [], 0)
