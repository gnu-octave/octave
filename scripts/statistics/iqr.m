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
## @deftypefn  {} {@var{Z} =} iqr (@var{x})
## @deftypefnx {} {@var{Z} =} iqr (@var{x}, @var{dim})
## @deftypefnx {} {@var{Z} =} iqr (@var{x}, @qcode{"ALL"})
## Return the interquartile range of @var{x}, defined as the distance between
## the 25th and 75th percentile values of @var{x} calculated using:
##    quantile (x, [0.25 0.75])
##
## If @var{x} is a vector, @code{iqr (@var{x})} will operate on the data in
## @var{x}.
##
## If @var{x} is a matrix, @code{iqr (@var{x})} will operate independently on
## each column in @var{x} returning a row vector @var{Z}.
##
## If @var{x} is a n-dimensional array, @code{iqr (@var{x})} will operate
## independently on the first non-singleton dimension in @var{x}, returning an
## array @var{Z} the same shape as @var{x} with the non-singleton dimenion
## reduced to 1.
##
## The optional variable @var{dim} can be used to force @code{iqr} to operate
## over the specified dimension.  @var{dim} can either be a scalar dimension or
## a vector of non-repeating dimensions over which to operate.  In either case
## @var{dim} must be positive integers.  A vector @var{dim} concatenates all
## specified dimensions for independent operation by @code{iqr}.
##
## Specifying dimension @qcode{"ALL"} will force @code{iqr} to operate
## on all elements of @var{x}, and is equivalent to @code{iqr (@var{x}(:))}.
## Similarly, specifying a vector dimension including all non-singleton
## dimensions of @var{x} is equivalent to @code{iqr (@var{x}, @qcode{"ALL"})}.
##
## If @var{x} is a scalar, or only singleton dimensions are specified for
## @var{dim}, the output will be @code{zeros (size (@var{x}))}.
##
## As a measure of dispersion, the interquartile range is less affected by
## outliers than either @code{range} or @code{std}.
##
## @seealso{bounds, mad, range, std, prctile, quantile}
## @end deftypefn

## TODO:  When Probability Distribution Objects are implemented, enable
##        handling for those object types.

function z = iqr (x, dim)

  ## input checks
  if (nargin < 1)
    print_usage ();
  elseif (nargin < 2)
    dim = [];
  endif

  if (! (isnumeric (x) || islogical (x)))
    error ("iqr: X must be a numeric vector or matrix");
  endif

  vecdim_flag = false;
  nd = ndims (x);
  sz = size (x);

  if (isempty (dim))
    ## Find first non-singleton dimension.
    if (max (sz) == 1)
      dim = 2;
    else
      dim = find ((sz > 1), 1);
    endif
  else

    if (isvector (dim) && isnumeric (dim)
        && all (dim > 0) && all (rem (dim, 1) == 0))

      if (((num_vecdims = numel (dim)) > 1) && all (diff (sort (dim))))
        ## DIM must be 1-D and non repeating.

        ## Detect trivial case of DIM being all dimensions (same as "all").
        highest_dim = (max (nd, max (dim)));
        if ((num_vecdims == nd) && (highest_dim == nd))
          x = x(:);
          sz = size (x);
          dim = 1;
        else
          ## Move dimensions for operation to the front, keeping the order of
          ## the remaining dimensions.
          ## Reshape those into a single dimension.
          ## Process as normal for a dim1 iqr on X, reshape when done.

          vecdim_flag = true;  ## flag for final reshape

          if (iscolumn (dim))
            dim = dim.';
          endif

          ## Permutation vector with DIM at front
          perm = [1:highest_dim];
          perm(dim) = [];
          perm = [dim, perm];

          ## Reshape X to put dims to process at front.
          x = permute (x, perm);
          sz_x_new = size (x);

          ## Preserve trailing singletons when dim > ndims (x).
          sz_x_new = [sz_x_new, ones(1, highest_dim - numel (sz_x_new))];

          newshape = [prod(sz_x_new(1:num_vecdims)), ...
                      ones(1, (num_vecdims-1)), ...
                      sz_x_new((num_vecdims+1):end)];

          if (numel (newshape) == 1)
            newshape = [newshape, 1];
          endif

          ## Collapse dimensions to be processses into single column.
          x = reshape (x, newshape);

          ## Operate column-wise.
          dim = 1;
        endif

      elseif (! isscalar (dim))
        error ("iqr: vector DIM must contain non-repeating positive integers");
      endif

    elseif (strcmp (tolower (dim), "all"))
      ## "ALL" simplifies to collapsing all elements to single vector
      x = x(:);
      dim = 1;
      sz = size (x);

    else
      error ("iqr: DIM must be a positive integer scalar, vector, or 'all'");
    endif

  endif

  if (((dim > nd) || (sz(dim) == 1)) && all (isfinite (x)))
    ## shortcut easy zeros
    z = zeros (sz);
  elseif (iscolumn (x) && (dim == 1))
    ## detect col vector with quantile/diff dim requirement mismatch
    z = abs (diff (quantile (x, [0.25, 0.75], 1), [], 2));
  else
    z = abs (diff (quantile (x, [0.25, 0.75], dim), [], dim));
  endif

  if (vecdim_flag)
    z = ipermute (z, perm);
  endif

endfunction


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
%!assert (iqr ([1:5; 2:6], "all"), 3)

%!test
%! x = reshape (1:6, [1 2 3]);
%! assert (iqr (x), ones (1, 1, 3));
%! assert (iqr (x, 1), zeros (1, 2, 3));
%! assert (iqr (x, 2), ones (1, 1, 3));
%! assert (iqr (x, 3), [3 3]);

## n-D arrays
%!test
%! x = magic (4); x = cat (3,x, 2*x, 3*x); x = cat (4, x, 2*x);
%! y = cat (3, 8*[1 1 1 1], 16*[1 1 1 1], 24*[1 1 1 1]);
%! assert (iqr (x), cat (4, y, 2*y));
%! assert (iqr (x, 1), cat (4, y, 2*y));
%! y = cat (3, 4*[3 1 1 3].', 8*[3 1 1 3].', 12*[3 1 1 3].');
%! assert (iqr (x, 2), cat (4, y, 2*y));
%! y = [24 3 4.5 19.5; 7.5 16.5 15 12; 13.5 10.5 9, 18; 6 21 22.5 1.5];
%! assert (iqr (x, 3), cat (4, y, 2*y));
%! y = [16 2 3 13; 5 11 10 8; 9 7 6 12; 4 14 15 1];
%! assert (iqr (x, 4), cat (3, y, 2*y, 3*y));
%! assert (iqr (x, 5), zeros (size (x)));

## vector dimensions
%!assert (iqr (17, [1 8]), 0)
%!assert (iqr ([[1 2 5]; [2 5 6]], [1 2]), 3)
%!assert (iqr (cat (3, [1 2 5; 2 5 6], [1 2 5; 2 5 6]), [1 2]), cat(3, 3, 3))
%!assert (iqr (cat (3, [1 2 5; 2 5 6], [1 2 5; 2 5 6]), [1 2]'), cat(3, 3, 3))
%!test
%! x = magic (4); x = cat (3, x, 2*x, 3*x); x = cat (4, x, 2*x);
%! y = cat (3, 8, 16, 24);
%! assert (iqr (x, [1 2]), cat (4, y, 2*y));
%! y = [14, 18.5, 17.5 19.5];
%! assert (iqr (x, [1 3]), cat (4, y, 2*y));
%! y = [10.5 12.5 11.5 15.0000];
%! assert (iqr (x, [1 4]), cat (3, y, 2*y, 3*y));
%! assert (iqr (x, [1 5]), iqr (x, 1));
%! y = [24 13 12 25.5]';
%! assert (iqr (x, [2 3]), cat (4, y, 2*y));
%! y = [17.5, 9, 8, 18.5]';
%! assert (iqr (x, [2 4]), cat (3, y, 2*y, 3*y));
%! assert (iqr (x, [3 4]), [32 4 6 26; 10 22 20 16; 18 14 12 24; 8 28 30 2]);
%! assert (iqr (x, [3 4]), iqr (x, [4 3]));
%! assert (iqr (x, [1 2 3]), cat (4, 17.5, 35));
%! assert (iqr (x, [2 3 4]), [29.5 19.5 23 31]');
%! assert (iqr (x, [1 3 4]), [22 28 22 30.5]);
%! assert (iqr (x, [1 2 4]), cat (3, 11, 22, 33));
%! assert (iqr (x, [1 2 5]), iqr (x, [1 2]));
%! assert (iqr (x, [5 6]), zeros (size (x)));

## Inf, NaN
%!assert (iqr (Inf), NaN)
%!assert (iqr (-Inf), NaN)
%!assert (iqr (NaN), NaN)
%!assert (iqr (NaN), NaN)
%!assert (iqr ([1 2 Inf], 1), [0 0 NaN])
%!assert (iqr ([1 2 Inf], 2), Inf)
%!assert (iqr ([1 2 -Inf], 1), [0 0 NaN])
%!assert (iqr ([1 2 -Inf], 2), Inf)
%!assert (iqr ([1 2 3 NaN], 1), [0 0 0 NaN])
%!assert (iqr ([1 2 3 NaN], 2), 1.5)
%!assert (iqr ([1 NaN 2 3], 2), 1.5)
%!assert (iqr (NaN (2), 1), [NaN, NaN])
%!assert (iqr (NaN (2), 2), [NaN; NaN])
%!assert (iqr (NaN (2), 3), NaN (2))
%!assert (iqr ([[1 2 5], [2 NaN 6]], "all"), 3.5)

## input validation
%!error iqr ()
%!error iqr (1, 2, 3)
%!error <X .* numeric> iqr (['A'; 'B'])
%!error <DIM .* positive integer> iqr (1, 'A')
%!error <DIM .* positive integer> iqr (1, 0)
%!error <DIM .* positive integer> iqr (1, -2)
%!error <DIM .* positive integer> iqr (1, 1.4)
%!error <DIM .* positive integer> iqr (1, [1 -2])
%!error <DIM .* positive integer> iqr (1, [1 1.4])
%!error <DIM .* positive integer> iqr ([1 2 3], NaN)
%!error <DIM .* positive integer> iqr ([1 2 3], [2 NaN])
%!error <DIM .* positive integer> iqr ([1 2 3], Inf)
%!error <DIM .* positive integer> iqr ([1 2 3], [2 Inf])
%!error <vector DIM .* non-repeating> iqr ([1 2 3], [1 2 1])
%!error <DIM .* vector> iqr (1, [1 2; 3 4])
