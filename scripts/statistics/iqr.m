########################################################################
##
## Copyright (C) 1995-2025 The Octave Project Developers
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
## @deftypefn  {} {@var{r} =} iqr (@var{x})
## @deftypefnx {} {@var{r} =} iqr (@var{x}, @var{dim})
## @deftypefnx {} {@var{r} =} iqr (@var{x}, @var{vecdim})
## @deftypefnx {} {@var{r} =} iqr (@var{x}, @qcode{"all"})
## @deftypefnx {} {[@var{r}, @var{q}] =} iqr (@dots{})
## Compute the interquartile range of @var{x}.
##
## The interquartile range is defined as the difference between the 75th and
## 25th percentile values of @var{x} calculated using
##
## @example
## quantile (x, [0.25 , 0.75])
## @end example
##
## If @var{x} is a vector, then @code{iqr (@var{x})} returns the interquartile
## range of the elements in @var{x}.
##
## If @var{x} is a matrix, then @code{iqr (@var{x})} returns a row vector with
## each element containing the interquartile range of the corresponding column
## in @var{x}.
##
## If @var{x} is an array, then @code{iqr (@var{x})} computes the interquartile
## range along the first non-singleton dimension of @var{x}.
##
## The optional input @var{dim} specifies the dimension to operate on and must
## be a positive integer.  Specifying any singleton dimension of @var{x},
## including any dimension exceeding @code{ndims (@var{x})}, will return
## @code{zeros (size (@var{x}))}.
##
## Specifying multiple dimensions with input @var{vecdim}, a vector of
## non-repeating dimensions, will operate along the array slice defined by
## @var{vecdim}.  If @var{vecdim} indexes all dimensions of @var{x}, then it is
## equivalent to the option @qcode{"all"}.  Any dimension in @var{vecdim}
## greater than @code{ndims (@var{x})} is ignored.
##
## Specifying the dimension as @qcode{"all"} will cause @code{iqr} to operate
## on all elements of @var{x}, and is equivalent to @code{iqr (@var{x}(:))}.
##
## The optional output @var{q} contains the quantiles for the 25th and 75th
## percentile of the data.
##
## Usage Note: As a measure of dispersion, the interquartile range is less
## affected by outliers than either @code{range} or @code{std}.  The
## interquartile range of a scalar is necessarily @code{0}.
##
## @seealso{bounds, mad, range, std, prctile, quantile}
## @end deftypefn

## TODO:  When Probability Distribution Objects are implemented, enable
##        handling for those object types.

function [r, q] = iqr (x, dim = [])

  if (nargin < 1)
    print_usage ();
  endif

  vecdim_flag = false;
  nd = ndims (x);
  sz = size (x);
  empty_x = isempty (x);

  ## FIXME: Matlab only works on real floating point inputs (single, double).
  ##        Should octave operate on integer or logical arrays?
  if (! (isnumeric (x) || islogical (x)))
    error ("iqr: X must be numeric or logical");
  endif

  if (isempty (dim))
    ## Find first non-singleton dimension.
    (dim = find (sz != 1, 1)) || (dim = 1);

    ## Return immediately for an empty matrix
    if (empty_x)
      if (nd == 2 && max (sz) <= 1)
        ## Matlab compatibility for 0x0, 1x0, 0x1 matrices
        sz = [1, 1];
      else
        ## Function operates on DIM and reduces it to 1.
        sz(dim) = 1;
      endif
      r = NaN (sz);
      q = cat (dim, r, r);
      return;
    endif

  else

    ## Check for numeric DIM/VECDIM argument
    if (isnumeric (dim) && isvector (dim) && isindex (dim))

      ## Check for proper VECDIM (more than 1 dim, no repeats)
      nel_vecdim = numel (dim);
      if (nel_vecdim > 1 && all (diff (sort (dim))))

        if (empty_x)
          ## Return immediately for an empty matrix
          dim = dim(dim <= nd);  # exclude dims greater than ndims
          sz(dim) = 1;           # opearate on VECDIMS and reduce to 1
          r = NaN (sz);
          ## Matlab compatibility: change only first VECDIM of q to 2.
          sz(min (dim)) = 2;
          q = NaN (sz);
          return;
        endif

        ## Detect trivial case of DIM being all dimensions (same as "all").
        max_dim = max (nd, max (dim));
        if (nel_vecdim == nd && max_dim == nd)
          x = x(:);
          sz = size (x);
          dim = 1;
        else
          ## Algorithm: Move dimensions for operation to the front, keeping the
          ## order of the remaining dimensions.  Reshape the moved dims into a
          ## single dimension (row).  Calculate as normal with iqr on dim1 of
          ## X, then reshape to correct dimensions.

          vecdim_flag = true;  # reshape results at end

          dim = dim(:).';  # Force row vector

          ## Permutation vector with DIM at front
          perm = [1:max_dim];
          perm(dim) = [];
          perm = [dim, perm];

          ## Reshape X to put dims to process at front.
          x = permute (x, perm);
          newsize = size (x);

          ## Preserve trailing singletons when dim > ndims (x).
          newsize = [newsize, ones(1, max_dim - numel (newsize))];

          newshape = [prod(newsize(1:nel_vecdim)), ...
                      ones(1, (nel_vecdim-1)), ...
                      newsize((nel_vecdim+1):end)];

          ## Size must always have 2 dimensions.
          if (isscalar (newshape))
            newshape = [newshape, 1];
          endif

          ## Collapse dimensions to be processsed into single column.
          x = reshape (x, newshape);

          ## Operate on rows.
          dim = 1;
        endif

      elseif (! isscalar (dim))
        error ("iqr: VECDIM must contain non-repeating positive integers");

      elseif (empty_x)
        ## Return immediately for an empty matrix
        if (dim <= nd)
          sz(dim) = 1;
        endif
        r = NaN (sz);
        q = cat (dim, r, r);
        return;
      endif

    elseif (strcmpi (dim, "all"))

      ## Return immediately for an empty matrix
      if (empty_x)
        r = NaN;
        q = [NaN; NaN];
        return;
      endif

      ## "all" simplifies to collapsing all elements to single vector.
      x = x(:);
      sz = size (x);
      dim = 1;

    else
      error ("iqr: DIM must be a positive integer scalar, vector, or 'all'");
    endif

  endif

  if ((dim > nd || sz(dim) == 1) && all (isfinite (x)))
    ## Shortcut, no calculation required for 1-element dimensions
    r = zeros (sz);
    q = cat (dim, x, x);
  elseif (dim == 1 && iscolumn (x))
    ## Work around quantile() where vector output orientation follows p input,
    ## rather than x input.
    q = quantile (x, [0.25; 0.75], 1);
    r = abs (diff (q, [], 1));
  else
    q = quantile (x, [0.25, 0.75], dim);
    r = abs (diff (q, [], dim));
  endif

  if (vecdim_flag)
    r = ipermute (r, perm);
    q = ipermute (q, perm);
  endif

endfunction


## Basic tests
%!assert (iqr (17), 0)
%!assert (iqr (17, 1), 0)
%!assert (iqr (17, 4), 0)
%!assert (iqr (1:3), 1.5)
%!assert (iqr (1:4), 2)
%!assert (iqr (1:5), 2.5)
%!assert (iqr (1:10), 5)
%!assert (iqr ((1:10).'), 5)
%!assert (iqr (1:10, 2), 5)
%!assert (iqr (1:10, 1), zeros (1, 10))
%!assert (iqr (1:10, 3), zeros (1, 10))
%!assert (iqr ([1:5; 2:6], [1, 2]), 3)
%!assert (iqr ([1:5; 2:6], "all"), 3)

%!test
%! x = reshape (1:6, [1, 2, 3]);
%! assert (iqr (x), ones (1, 1, 3));
%! assert (iqr (x, 1), zeros (1, 2, 3));
%! assert (iqr (x, 2), ones (1, 1, 3));
%! assert (iqr (x, 3), [3, 3]);

## N-D arrays
%!test
%! x = magic (4); x = cat (3,x, 2*x, 3*x); x = cat (4, x, 2*x);
%! y = cat (3, 8*[1, 1, 1, 1], 16*[1, 1, 1, 1], 24*[1, 1, 1, 1]);
%! assert (iqr (x), cat (4, y, 2*y));
%! assert (iqr (x, 1), cat (4, y, 2*y));
%! y = cat (3, 4*[3, 1, 1, 3].', 8*[3, 1, 1, 3].', 12*[3, 1, 1, 3].');
%! assert (iqr (x, 2), cat (4, y, 2*y));
%! y = [24, 3, 4.5, 19.5; 7.5, 16.5, 15, 12; 13.5, 10.5, 9, 18; 6, 21, 22.5, 1.5];
%! assert (iqr (x, 3), cat (4, y, 2*y));
%! y = [16, 2, 3, 13; 5, 11, 10, 8; 9, 7, 6, 12; 4, 14, 15, 1];
%! assert (iqr (x, 4), cat (3, y, 2*y, 3*y));
%! assert (iqr (x, 5), zeros (size (x)));

## vector dimensions
%!assert (iqr (17, [1, 8]), 0)
%!assert (iqr ([[1, 2, 5]; [2, 5, 6]], [1, 2]), 3)
%!assert (iqr (cat (3, [1, 2, 5; 2, 5, 6], [1, 2, 5; 2, 5, 6]), [1, 2]), cat(3, 3, 3))
%!assert (iqr (cat (3, [1, 2, 5; 2, 5, 6], [1, 2, 5; 2, 5, 6]), [1, 2]'), cat(3, 3, 3))
%!test
%! x = magic (4); x = cat (3, x, 2*x, 3*x); x = cat (4, x, 2*x);
%! y = cat (3, 8, 16, 24);
%! assert (iqr (x, [1, 2]), cat (4, y, 2*y));
%! y = [14, 18.5, 17.5, 19.5];
%! assert (iqr (x, [1, 3]), cat (4, y, 2*y));
%! y = [10.5, 12.5, 11.5, 15];
%! assert (iqr (x, [1, 4]), cat (3, y, 2*y, 3*y));
%! assert (iqr (x, [1, 5]), iqr (x, 1));
%! y = [24, 13, 12, 25.5]';
%! assert (iqr (x, [2, 3]), cat (4, y, 2*y));
%! y = [17.5, 9, 8, 18.5]';
%! assert (iqr (x, [2, 4]), cat (3, y, 2*y, 3*y));
%! assert (iqr (x, [3, 4]), [32, 4, 6, 26; 10, 22, 20, 16; 18, 14, 12, 24; 8, 28, 30, 2]);
%! assert (iqr (x, [3, 4]), iqr (x, [4, 3]));
%! assert (iqr (x, [1, 2, 3]), cat (4, 17.5, 35));
%! assert (iqr (x, [2, 3, 4]), [29.5, 19.5, 23, 31]');
%! assert (iqr (x, [1, 3, 4]), [22, 28, 22, 30.5]);
%! assert (iqr (x, [1, 2, 4]), cat (3, 11, 22, 33));
%! assert (iqr (x, [1, 2, 5]), iqr (x, [1, 2]));
%! assert (iqr (x, [5, 6]), zeros (size (x)));

## Inf, NaN
%!assert (iqr (Inf), NaN)
%!assert (iqr (-Inf), NaN)
%!assert (iqr (NaN), NaN)
%!assert (iqr (NaN), NaN)
%!assert (iqr ([1, 2, Inf], 1), [0, 0, NaN])
%!assert (iqr ([1, 2, Inf], 2), Inf)
%!assert (iqr ([1, 2, -Inf], 1), [0, 0, NaN])
%!assert (iqr ([1, 2, -Inf], 2), Inf)
%!assert (iqr ([1, 2, 3, NaN], 1), [0, 0, 0, NaN])
%!assert (iqr ([1, 2, 3, NaN], 2), 1.5)
%!assert (iqr ([1, NaN, 2, 3], 2), 1.5)
%!assert (iqr (NaN (2), 1), [NaN, NaN])
%!assert (iqr (NaN (2), 2), [NaN; NaN])
%!assert (iqr (NaN (2), 3), NaN (2))
%!assert (iqr ([[1, 2, 5], [2, NaN, 6]], "all"), 3.5)

## Tests for q output
%!assert ([~,q] = iqr ([1:100]), [25.5, 75.5])
%!assert ([~,q] = iqr ([1:100]'), [25.5; 75.5])
%!assert ([~,q] = iqr (repmat ([1:100]', [1,2])), repmat ([25.5; 75.5], [1,2]))
%!assert ([~,q] = iqr (repmat ([1:100], [2,1]), 2), repmat ([25.5, 75.5], [2,1]))
%!assert ([~,q] = iqr (repmat ([1:100], [2,1]), [1 2]), [25.5; 75.5])
%!assert ([~,q] = iqr (repmat ([1:100], [2,1]), 'all'), [25.5; 75.5])

## Empty inputs
%!assert <*65531> (iqr ([]), NaN)
%!assert <*65531> (iqr (ones (0, 1)), NaN)
%!assert <*65531> (iqr (ones (0, 1), 1), NaN)
%!assert <*65531> (iqr (ones (0, 1), 2), NaN (0, 1))
%!assert <*65531> (iqr (ones (0, 1), 3), NaN (0, 1))
%!assert <*65531> (iqr (ones (1, 0)), NaN)
%!assert <*65531> (iqr (ones (1, 0), 1), NaN (1, 0))
%!assert <*65531> (iqr (ones (1, 0), 2), NaN)
%!assert <*65531> (iqr (ones (1, 0), 3), NaN (1, 0))
%!assert <*65531> (iqr (ones (1, 0), 9), NaN (1, 0))
%!assert <*65531> (iqr (ones (1, 1, 0)), NaN)
%!assert <*65531> (iqr (ones (0, 0, 1, 0)), NaN (1, 0, 1, 0))
%!assert <*65531> (iqr (ones (1, 1, 1, 0)), NaN)
%!assert <*65531> (iqr (ones (1, 1, 1, 0), 1), NaN (1, 1, 1, 0))
%!assert <*65531> (iqr (ones (0, 0, 1, 0), 1), NaN (1, 0, 1, 0))
%!assert <*65531> (iqr (ones (0, 0, 1, 0), 2), NaN (0, 1, 1, 0))
%!assert <*65531> (iqr (ones (0, 0, 1, 0), 3), NaN (0, 0, 1, 0))
%!assert <*65531> (iqr (ones (0, 0, 1, 0), 4), NaN (0, 0, 1, 1))
%!assert <*65531> (iqr (ones (0, 0, 1, 0), 9), NaN (0, 0, 1, 0))
%!assert <*65531> (iqr (ones (0, 0, 1, 0), [1, 2]), NaN (1, 1, 1, 0))
%!assert <*65531> (iqr (ones (0, 0, 1, 0), [1, 4]), NaN (1, 0, 1, 1))
%!assert <*65531> (iqr (ones (0, 0, 1, 0), [1, 9]), NaN (1, 0, 1, 0))
%!assert <*65531> (iqr ([], "all"), NaN)
%!assert <*65531> (iqr (ones (0, 1), "all"), NaN)
%!assert <*65531> (iqr (ones (1, 0), "all"), NaN)
%!assert <*65531> (iqr (ones (1, 1, 0), "all"), NaN)
%!assert <*65531> (iqr (ones (0, 0, 1, 0), 'all'), NaN)
%!assert <*65531> ([~,q] = iqr ([]), NaN (2, 1))
%!assert <*65531> ([~,q] = iqr (ones (0, 1)), NaN (2, 1))
%!assert <*65531> ([~,q] = iqr (ones (0, 1), 1), NaN (2, 1))
%!assert <*65531> ([~,q] = iqr (ones (0, 1), 2), NaN (0, 2))
%!assert <*65531> ([~,q] = iqr (ones (0, 1), 3), NaN (0, 1, 2))
%!assert <*65531> ([~,q] = iqr (ones (1, 0)), NaN (1, 2))
%!assert <*65531> ([~,q] = iqr (ones (1, 0), 1), NaN (2, 0))
%!assert <*65531> ([~,q] = iqr (ones (1, 0), 2), NaN (1, 2))
%!assert <*65531> ([~,q] = iqr (ones (1, 0), 3), NaN (1, 0, 2))
%!assert <*65531> ([~,q] = iqr (ones (1, 0), 9), NaN (1,0,1,1,1,1,1,1,2))
%!assert <*65531> ([~,q] = iqr (ones (1, 1, 0)), NaN (1, 1, 2))
%!assert <*65531> ([~,q] = iqr (ones (0, 0, 1, 0)), NaN (2, 0, 1, 0))
%!assert <*65531> ([~,q] = iqr (ones (1, 1, 1, 0)), NaN (1, 1, 1, 2))
%!assert <*65531> ([~,q] = iqr (ones (1, 1, 1, 0), 1), NaN (2, 1, 1, 0))
%!assert <*65531> ([~,q] = iqr (ones (0, 0, 1, 0), 1), NaN (2, 0, 1, 0))
%!assert <*65531> ([~,q] = iqr (ones (0, 0, 1, 0), 2), NaN (0, 2, 1, 0))
%!assert <*65531> ([~,q] = iqr (ones (0, 0, 1, 0), 3), NaN (0, 0, 2, 0))
%!assert <*65531> ([~,q] = iqr (ones (0, 0, 1, 0), 4), NaN (0, 0, 1, 2))
%!assert <*65531> ([~,q] = iqr (ones (0, 0, 1, 0), 9), NaN (0,0,1,0,1,1,1,1,2))
%!assert <*65531> ([~,q] = iqr (ones (0, 0, 1, 0), [1, 2]), NaN (2, 1, 1, 0))
%!assert <*65531> ([~,q] = iqr (ones (0, 0, 1, 0), [1, 4]), NaN (2, 0, 1, 1))
%!assert <*65531> ([~,q] = iqr (ones (0, 0, 1, 0), [1, 9]), NaN (2, 0, 1, 0))
%!assert <*65531> ([~,q] = iqr ([], "all"), NaN (2, 1))
%!assert <*65531> ([~,q] = iqr (ones (0, 1), "all"), NaN (2, 1))
%!assert <*65531> ([~,q] = iqr (ones (1, 0), "all"), NaN (2, 1))
%!assert <*65531> ([~,q] = iqr (ones (1, 1, 0), "all"), NaN (2, 1))
%!assert <*65531> ([~,q] = iqr (ones (0, 0, 1, 0), 'all'), NaN (2, 1))

## input validation
%!error <Invalid call> iqr ()
%!error <X must be numeric or logical> iqr (['A'; 'B'])
%!error <X must be numeric or logical> iqr ({1, 2})
%!error <VECDIM must contain non-repeating> iqr ([1, 2, 3], [1, 2, 1])
%!error <DIM must be a positive integer> iqr (1, 'A')
%!error <DIM must be a positive integer> iqr (1, 0)
%!error <DIM must be a positive integer> iqr (1, -2)
%!error <DIM must be a positive integer> iqr (1, 1.4)
%!error <DIM must be .* positive integer> iqr (1, [1, -2])
%!error <DIM must be .* positive integer> iqr (1, [1, 1.4])
%!error <DIM must be .* positive integer> iqr ([1, 2, 3], NaN)
%!error <DIM must be .* positive integer> iqr ([1, 2, 3], [2, NaN])
%!error <DIM must be .* positive integer> iqr ([1, 2, 3], Inf)
%!error <DIM must be .* positive integer> iqr ([1, 2, 3], [2, Inf])
%!error <DIM must be .* scalar, vector> iqr (1, [1, 2; 3, 4])
