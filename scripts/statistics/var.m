########################################################################
##
## Copyright (C) 1995-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{v} =} var (@var{x})
## @deftypefnx {} {@var{v} =} var (@var{x}, @var{w})
## @deftypefnx {} {@var{v} =} var (@var{x}, @var{w}, @var{dim})
## @deftypefnx {} {@var{v} =} var (@var{x}, @var{w}, @qcode{"ALL"})
## @deftypefnx {} {[@var{v}, @var{m}] =} var (@dots{})
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
## If @var{x} is an array, compute the variance for each column and return them
## in a row vector (or for an n-D array, the result is returned as an array of
## dimension 1 x n x m x @dots{}).
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
## Normalize with @math{N}@.  This provides the square root of the second
## moment around the mean.
##
## @item a vector:
## Compute the weighted variance with non-negative scalar weights.  The length
## of @var{w} must equal the size of @var{x} along dimension @var{dim}.
## @end table
##
## If @math{N} is equal to 1 the value of @var{W} is ignored and normalization
## by @math{N} is used.
##
## The optional variable @var{dim} forces @code{var} to operate over the
## specified dimension(s).  @var{dim} can either be a scalar dimension or a
## vector of non-repeating dimensions.  Dimensions must be positive integers,
## and the variance is calculated over the array slice defined by @var{dim}.
##
## Specifying dimension @qcode{"all"} will force @code{var} to operate on all
## elements of @var{x}, and is equivalent to @code{var (@var{x}(:))}.
##
## When @var{dim} is a vector or @qcode{"all"}, @var{w} must be either 0 or 1.
##
## The optional second output variable @var{mu} contains the mean or weighted
## mean used to calculate @var{v}, and will be the same size as @var{v}.
## @seealso{cov, std, skewness, kurtosis, moment}
## @end deftypefn

function [v, mu] = var (x, w = 0, dim = [])

  if (nargin < 1)
    print_usage ();
  endif

  if (! (isnumeric (x) || islogical (x)))
    error ("var: X must be a numeric vector or matrix");
  endif

  if (isempty (w))
    w = 0;
  endif

  nd = ndims (x);
  sz = size (x);
  emptydimflag = false;

  if (isempty (dim))
    emptydimflag = true;  # Compatibility hack for empty x, ndims==2

    ## Find the first non-singleton dimension.
    (dim = find (sz != 1, 1)) || (dim = 1);

  else
    if (isscalar (dim))
      if (dim < 1 || dim != fix (dim))
        error ("var: DIM must be a positive integer scalar, vector, or 'all'");
      endif
    elseif (isnumeric (dim))
      if (! isvector (dim) && all (dim > 0) && all (rem (dim, 1) == 0))
        error ("var: DIM must be a positive integer scalar, vector, or 'all'");
      endif
      if (dim != unique (dim, "stable"))
        error ("var: vector DIM must contain non-repeating positive integers");
      endif
      if (! isscalar (w))
        error ("var: W must be either 0 or 1 when DIM is a vector");
      endif

      ## Reshape X to compute the variance over an array slice
      if (iscolumn (dim))
        dim = dim.';
      endif

      collapsed_dims = dim;
      dim = dim(end);

      ## Permute X to cluster the dimensions to collapse
      max_dim = max ([nd, collapsed_dims]);
      perm_start = perm_end = [1:max_dim];
      perm_start(dim:end) = [];
      perm_start(ismember (perm_start, collapsed_dims)) = [];
      perm_end(1:dim) = [];
      perm_end(ismember (perm_end, collapsed_dims)) = [];
      perm = [perm_start, collapsed_dims, perm_end];

      x = permute (x, perm);

      ## Collapse the given dimensions
      newshape = ones (1, max_dim);
      newshape(1:nd) = sz;
      newshape(collapsed_dims(1:(end-1))) = 1;
      newshape(dim) = prod (sz(collapsed_dims));

      ## New X with collapsed dimensions
      x = reshape (x, newshape);

    elseif (ischar (dim) && strcmpi (dim, "all"))
      if (! isscalar (w))
        error ("var: W must be either 0 or 1 when using 'all' as dimension");
      endif

      ## "all" equates to collapsing all elements to a single vector
      x = x(:);
      dim = 1;
      sz = size (x);
    else
      error ("var: DIM must be a positive integer scalar, vector, or 'all'");
    endif
  endif

  n = size (x, dim);
  if (! isvector (w) || ! isnumeric (w)
      || (isvector (w) && any (w < 0)) ||
          (isscalar (w) && ((w != 0 && w != 1) && (n != 1))))
    error ("var: W must be 0, 1, or a vector of positive integers");
  endif

  if (isempty (x))
    ## Empty matrix special case
    if (emptydimflag && nd == 2 && all (sz == [0, 0]))
      v = NaN;
      mu = NaN;
    else
      sz(dim) = 1;
      v = NaN (sz);
      mu = NaN (sz);
    endif
  elseif (n == 1)
    ## Scalar special case
    if (! isscalar (w))
      error (["var: the length of W must be equal to the size of X " ...
              "in the dimension along which variance is calculated"]);
    endif
    if (isa (x, "single"))
      v = zeros (sz, "single");
      mu = x;
    else
      v = zeros (sz);
      mu = x;
    endif
    v(isnan (x) | isinf (x)) = NaN;
  else
    ## Regular algorithm
    if (isscalar (w))
      v = sumsq (center (x, dim), dim) / (n - 1 + w);
      if (nargout == 2)
        mu = mean (x, dim);
      endif
    else
      ## Weighted variance
      if (numel (w) != n)
        error (["var: the length of W must be equal to the size of X " ...
                "in the dimension along which variance is calculated"]);
      endif
      if ((dim == 1 && isrow (w)) || (dim == 2 && iscolumn (w)))
        w = w.';
      elseif (dim > 2)
        newdims = [ones(1, dim - 1), numel(w)];
        w = reshape (w, newdims);
      endif
      den = sum (w);

      ## FIXME: Use bsxfun, rather than broadcasting, until broadcasting
      ##        supports diagonal and sparse matrices (Bugs #41441, #35787).
      mu = sum (bsxfun (@times, w , x), dim) ./ den;
      v = sum (bsxfun (@times, w, ...
                            bsxfun (@minus, x, mu) .^ 2), dim) ./ den;
      ## mu = sum (w .* x, dim) ./ den; # automatic broadcasting
      ## v = sum (w .* ((x - mu) .^ 2), dim) ./ den;
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
%!assert (var ([1 2 3;1 2 3], [], [1 2]), 0.8, eps)

## Test empty inputs
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

## Test second output
%!test <*62395>
%! [~, m] = var (13);
%! assert (m, 13);
%! [~, m] = var (single(13));
%! assert (m, single(13));
%! [~, m] = var ([1, 2, 3; 3 2 1], []);
%! assert (m, [2 2 2]);
%! [~, m] = var ([1, 2, 3; 3 2 1], [], 1);
%! assert (m, [2 2 2]);
%! [~, m] = var ([1, 2, 3; 3 2 1], [], 2);
%! assert (m, [2 2]');
%! [~, m] = var ([1, 2, 3; 3 2 1], [], 3);
%! assert (m, [1 2 3; 3 2 1]);

## 2nd output, weighted inputs, vector dims
%!test <*62395>
%! [~, m] = var(5,99);
%! assert (m, 5);
%! [~, m] = var ([1:7], [1:7]);
%! assert (m, 5);
%! [~, m] = var ([eye(3)], [1:3]);
%! assert (m, [1/6, 1/3, 0.5], eps);
%! [~, m] = var (ones (2,2,2), [1:2], 3);
%! assert (m, ones (2,2));
%! [~, m] = var ([1 2; 3 4], 0, 'all');
%! assert (m, 2.5, eps);
%! [~, m] = var (reshape ([1:8], 2, 2, 2), 0, [1 3]);
%! assert (m, [3.5, 5.5], eps);

## 2nd output, empty inputs
%!test <*62395>
%! [~, m] = var ([]);
%! assert (m, NaN);
%! [~, m] = var ([],[],1);
%! assert (m, NaN(1,0));
%! [~, m] = var ([],[],2);
%! assert (m, NaN(0,1));
%! [~, m] = var ([],[],3);
%! assert (m, []);
%! [~, m] = var (ones (1,3,0,2));
%! assert (m, NaN(1,1,0,2));

## Test Inf and NaN inputs
%!test <*63203>
%! [v, m] = var (Inf);
%! assert (v, NaN);
%! assert (m, Inf);
%!test <*63203>
%! [v, m] = var (NaN);
%! assert (v, NaN);
%! assert (m, NaN);
%!test <*63203>
%! [v, m] = var ([1, Inf, 3]);
%! assert (v, NaN);
%! assert (m, Inf);
%!test <*63203>
%! [v, m] = var ([1, Inf, 3]');
%! assert (v, NaN);
%! assert (m, Inf);
%!test <*63203>
%! [v, m] = var ([1, NaN, 3]);
%! assert (v, NaN);
%! assert (m, NaN);
%!test <*63203>
%! [v, m] = var ([1, NaN, 3]');
%! assert (v, NaN);
%! assert (m, NaN);
%!test <*63203>
%! [v, m] = var ([1, Inf, 3], [], 1);
%! assert (v, [0, NaN, 0]);
%! assert (m, [1, Inf, 3]);
%!test <*63203>
%! [v, m] = var ([1, Inf, 3], [], 2);
%! assert (v, NaN);
%! assert (m, Inf);
%!test <*63203>
%! [v, m] = var ([1, Inf, 3], [], 3);
%! assert (v, [0, NaN, 0]);
%! assert (m, [1, Inf, 3]);
%!test <*63203>
%! [v, m] = var ([1, NaN, 3], [], 1);
%! assert (v, [0, NaN, 0]);
%! assert (m, [1, NaN, 3]);
%!test <*63203>
%! [v, m] = var ([1, NaN, 3], [], 2);
%! assert (v, NaN);
%! assert (m, NaN);
%!test <*63203>
%! [v, m] = var ([1, NaN, 3], [], 3);
%! assert (v, [0, NaN, 0]);
%! assert (m, [1, NaN, 3]);
%!test <*63203>
%! [v, m] = var ([1, 2, 3; 3, Inf, 5]);
%! assert (v, [2, NaN, 2]);
%! assert (m, [2, Inf, 4]);
%!test <*63203>
%! [v, m] = var ([1, Inf, 3; 3, Inf, 5]);
%! assert (v, [2, NaN, 2]);
%! assert (m, [2, Inf, 4]);
%!test <*63203>
%! [v, m] = var ([1, 2, 3; 3, NaN, 5]);
%! assert (v, [2, NaN, 2]);
%! assert (m, [2, NaN, 4]);
%!test <*63203>
%! [v, m] = var ([1, NaN, 3; 3, NaN, 5]);
%! assert (v, [2, NaN, 2]);
%! assert (m, [2, NaN, 4]);
%!test <*63203>
%! [v, m] = var ([Inf, 2, NaN]);
%! assert (v, NaN);
%! assert (m, NaN);
%!test <*63203>
%! [v, m] = var ([Inf, 2, NaN]');
%! assert (v, NaN);
%! assert (m, NaN);
%!test <*63203>
%! [v, m] = var ([NaN, 2, Inf]);
%! assert (v, NaN);
%! assert (m, NaN);
%!test <*63203>
%! [v, m] = var ([NaN, 2, Inf]');
%! assert (v, NaN);
%! assert (m, NaN);
%!test <*63203>
%! [v, m] = var ([Inf, 2, NaN], [], 1);
%! assert (v, [NaN, 0, NaN]);
%! assert (m, [Inf, 2, NaN]);
%!test <*63203>
%! [v, m] = var ([Inf, 2, NaN], [], 2);
%! assert (v, NaN);
%! assert (m, NaN);
%!test <*63203>
%! [v, m] = var ([NaN, 2, Inf], [], 1);
%! assert (v, [NaN, 0, NaN]);
%! assert (m, [NaN, 2, Inf]);
%!test <*63203>
%! [v, m] = var ([NaN, 2, Inf], [], 2);
%! assert (v, NaN);
%! assert (m, NaN);
%!test <*63203>
%! [v, m] = var ([1, 3, NaN; 3, 5, Inf]);
%! assert (v, [2, 2, NaN]);
%! assert (m, [2, 4, NaN]);
%!test <*63203>
%! [v, m] = var ([1, 3, Inf; 3, 5, NaN]);
%! assert (v, [2, 2, NaN]);
%! assert (m, [2, 4, NaN]);

## Test sparse/diagonal inputs
%!test <*63291>
%! [v, m] = var (2 * eye (2));
%! assert (v, [2, 2]);
%! assert (m, [1, 1]);
%!test <*63291>
%! [v, m] = var (4 * eye (2), [1, 3]);
%! assert (v, [3, 3]);
%! assert (m, [1, 3]);
%!test <*63291>
%! [v, m] = var (sparse (2 * eye (2)));
%! assert (full (v), [2, 2]);
%! assert (full (m), [1, 1]);
%!test <*63291>
%! [v, m] = var (sparse (4 * eye (2)), [1, 3]);
%! assert (full (v), [3, 3]);
%! assert (full (m), [1, 3]);

%!test <63291>
%! [v, m] = var (sparse (eye (2)));
%! assert (issparse (v));
%! assert (issparse (m));
%!test <63291>
%! [v, m] = var (sparse (eye (2)), [1, 3]);
%! assert (issparse (v));
%! assert (issparse (m));

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
