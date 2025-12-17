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
## @deftypefn  {} {@var{stats} =} statistics (@var{x})
## @deftypefnx {} {@var{stats} =} statistics (@var{x}, @var{dim})
## @deftypefnx {} {@var{stats} =} statistics (@var{x}, @var{vecdim})
## @deftypefnx {} {@var{stats} =} statistics (@var{x}, "all")
## @deftypefnx {} {@var{stats} =} statistics (@dots{}, @var{nanflag})
## Return a vector with statistics parameters over the input data @var{x}.
##
## @code{statistics (@var{x}} operates along the first non-singleton dimension
## of @var{x} and calculates the following statistical parameters:
## @enumerate
## @item minimum
## @item first quartile
## @item median
## @item third quartile
## @item maximum
## @item mean
## @item standard deviation
## @item skewness
## @item kurtosis
## @end enumerate
##
## If @var{x} is a row vector, then @code{statistics (@var{x})} returns a row
## vector with the aforementioned statistical parameters.  If @var{x} is a
## column vector, then it returns a column vector.
##
## If @var{x} is a matrix, then @code{statistics (@var{x})} returns a matrix
## such that each column contains the statistical parameters calculated over the
## corresponding column of @var{x}.
##
## If @var{x} is an array, then @code{statistics (@var{x})} computes the
## statistical parameters along the first non-singleton dimension of @var{x}.
##
## The data in @var{x} must be numeric and by default any NaN values are ignored
## from the computations of statistical parameters except for the mean and the
## standard deviation.  Set the optional argument @var{nanflag} to
## @qcode{"omitnan"} to exclude the NaN values from the calculation of the mean
## and standard deviation parameters.  Setting @var{nanflag} to
## @qcode{"includenan"} is ignored and it is equivalent to calling the
## @code{statistics} function without the @var{nanflag} argument.
##
## The size of @var{stats} is equal to the size of @var{x} except for the
## operating dimension, which equals to 9 (i.e. the number of statistical
## parameters returned).
##
## The optional input @var{dim} specifies the dimension to operate on and must
## be a positive integer.  Specifying any singleton dimension of @var{x},
## including any dimension exceeding @code{ndims (@var{x})}, will return
## @code{@var{x}}.
##
## Specifying multiple dimensions with input @var{vecdim}, a vector of
## non-repeating dimensions, will operate along the array slice defined by
## @var{vecdim}.  If @var{vecdim} indexes all dimensions of @var{x}, then it is
## equivalent to the option @qcode{"all"}.  Any dimension in @var{vecdim}
## greater than @code{ndims (@var{x})} is ignored.
##
## Specifying the dimension as @qcode{"all"} will cause @code{statistics} to
## operate on all elements of @var{x}, and is equivalent to
## @code{statistics (@var{x}(:))}.
## @seealso{min, max, median, mean, std, skewness, kurtosis}
## @end deftypefn

function stats = statistics (x, varargin)

  if (nargin < 1)
    print_usage ();
  endif

  if (! (isnumeric (x)))
    error ("statistics: X must be a numeric array");
  endif

  sz = size (x);

  if (nargin < 2)
    ## Find the first non-singleton dimension.
    (dim = find (sz != 1, 1)) || (dim = 1);
    nanflag = "includenan";
  else
    if (any (strcmpi (varargin{1}, {"omitnan", "includenan"})))
      (dim = find (sz != 1, 1)) || (dim = 1);
      nanflag = varargin{1};
    else
      dim = varargin{1};
      if (nargin > 2)
        nanflag = varargin{2};
        if (! any (strcmpi (nanflag, {"omitnan", "includenan"})))
          error ("statistics: NANFLAG must be either 'omitnan' or 'includenan'");
        endif
      else
        nanflag = "includenan";
      endif
    endif
  endif

  ## Handle "all" option here and test for minimum sample size
  if (strcmpi (dim, "all"))
    dim = 1;
    x = x(:);
    if (numel (x) < 2)
      error ("statistics: dimension of X is too small (<2)");
    endif
  endif

  ## Remove exceeding dimensions
  nd = ndims (x);
  ed = dim > nd;
  if (all (ed))
    stats = x;
    return;
  elseif (any (ed))
    dim(ed) = [];
  endif

  ## Test operating dim for minimum sample size
  if (prod (sz(dim)) < 2)
    error ("statistics: dimension of X is too small (<2)");
  endif

  ## Fix orientation for vector input
  if (dim(1) == 2)
    p = [0.25, 0.5, 0.75];
  else
    p = [0.25; 0.5; 0.75];
  endif

  min_val = min (x, [], dim);
  emp_inv = quantile (x, p, dim, 7);
  max_val = max (x, [], dim);
  meanval = mean (x, dim, nanflag);
  std_val = std (x, [], dim, nanflag);

  stats = cat (dim(1), min_val, emp_inv, max_val, meanval, std_val, ...
               skewness (x, [], dim), kurtosis (x, [], dim));

endfunction


%!test
%! x = rand (7, 5);
%! s = statistics (x);
%! assert (min (x), s(1,:), eps);
%! assert (median (x), s(3,:), eps);
%! assert (max (x), s(5,:), eps);
%! assert (mean (x), s(6,:), eps);
%! assert (std (x), s(7,:), eps);
%! assert (skewness (x), s(8,:), eps);
%! assert (kurtosis (x), s(9,:), eps);

%!test
%! x = rand (7, 5);
%! s = statistics (x, 2);
%! assert (min (x, [], 2), s(:,1), eps);
%! assert (median (x, 2), s(:,3), eps);
%! assert (max (x, [], 2), s(:,5), eps);
%! assert (mean (x, 2), s(:,6), eps);
%! assert (std (x, [], 2), s(:,7), eps);
%! assert (skewness (x, [], 2), s(:,8), eps);
%! assert (kurtosis (x, [], 2), s(:,9), eps);

## Test exceeding dimensions
%!test
%! x = rand (7, 5);
%! assert (statistics (x, 3), x);
%! assert (statistics (x, [3, 4]), x);
%! assert (statistics (x, [2, 4]), statistics (x, 2));

## Test vecdim and "all"
%!test
%! x = rand (7, 5, 3);
%! s = statistics (x, [2, 3]);
%! assert (min (x, [], [2, 3]), s(:,1), eps);
%! assert (median (x, [2, 3]), s(:,3), eps);
%! assert (max (x, [], [2, 3]), s(:,5), eps);
%! assert (mean (x, [2, 3]), s(:,6), eps);
%! assert (std (x, [], [2, 3]), s(:,7), eps);
%! assert (skewness (x, [], [2, 3]), s(:,8), eps);
%! assert (kurtosis (x, [], [2, 3]), s(:,9), eps);
%! assert (statistics (x, [1, 2, 3]), statistics (x, "all"));

## Test "omitnan" option
%! x = rand (7, 5);
%! x(3,1) = NaN;
%! s = statistics (x, 1, "omitnan");
%! assert (statistics (x, "omitnan"), s);
%! assert (min (x, [], 1), s(1,:), eps);
%! assert (median (x, 1, "omitnan"), s(3,:), eps);
%! assert (max (x, [], 1), s(5,:), eps);
%! assert (mean (x, 1, "omitnan"), s(6,:), eps);
%! assert (std (x, [], 1, "omitnan"), s(7,:), eps);
%! assert (skewness (x, [], 1), s(8,:), eps);
%! assert (kurtosis (x, [], 1), s(9,:), eps);

## Test input validation
%!error <Invalid call> statistics ()
%!error <statistics: X must be a numeric array> statistics (['A'; 'B'])
%!error <statistics: X must be a numeric array> statistics ([true; false])
%!error <statistics: X must be a numeric array> statistics ({1, 2, 3})
%!error <statistics: dimension of X is too small \(<2\)> statistics (1, "all")
%!error <statistics: dimension of X is too small \(<2\)> statistics (1)
