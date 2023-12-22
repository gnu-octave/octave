########################################################################
##
## Copyright (C) 2017-2024 The Octave Project Developers
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
## @deftypefn  {} {@var{m} =} mad (@var{x})
## @deftypefnx {} {@var{m} =} mad (@var{x}, @var{opt})
## @deftypefnx {} {@var{m} =} mad (@var{x}, @var{opt}, @var{dim})
## @deftypefnx {} {@var{m} =} mad (@var{x}, @var{opt}, @var{vecdim})
## @deftypefnx {} {@var{m} =} mad (@var{x}, @var{opt}, "all")
## Compute the mean or median absolute deviation (MAD) of the elements of
## @var{x}.
##
## The mean absolute deviation is defined as
##
## @example
## @var{mad} = mean (abs (@var{x} - mean (@var{x})))
## @end example
##
## The median absolute deviation is defined as
##
## @example
## @var{mad} = median (abs (@var{x} - median (@var{x})))
## @end example
##
## If @var{x} is a vector, compute @code{mad} for each element in @var{x}.  If
## @var{x} is an array the calculation is performed over the first
## non-singleton dimension.
##
## @code{mad} excludes NaN values from calculation similar to using the
## @code{omitnan} option in @code{var}, @code{mean}, and @code{median}.
##
## The optional argument @var{opt} determines whether mean or median absolute
## deviation is calculated.  The default is 0 which corresponds to mean
## absolute deviation; a value of 1 corresponds to median absolute deviation.
## Passing an empty input [] defaults to mean absolute deviation
## (@var{opt} = 0).
##
## The optional argument @var{dim} forces @code{mad} to operate along the
## specified dimension.  Specifying any singleton dimension in @var{x},
## including any dimension exceeding @code{ndims (@var{x})}, will result in
## an output of 0.
##
## Specifying the dimension as @var{vecdim}, a vector of non-repeating
## dimensions, will return the @code{mad} over the array slice defined by
## @var{vecdim}.  If @var{vecdim} indexes all dimensions of @var{x}, then it is
## equivalent to the option @qcode{"all"}.  Any dimension included in
## @var{vecdim} greater than @code{ndims (@var{x})} is ignored.
##
## Specifying the dimension as @qcode{"all"} will force @code{mad} to operate
## on all elements of @var{x}, and is equivalent to @code{mad (@var{x}(:))}.
##
## As a measure of dispersion, @code{mad} is less affected by outliers than
## @code{std}.
## @seealso{bounds, range, iqr, std, mean, median}
## @end deftypefn

function m = mad (x, opt = 0, dim)

  if (nargin < 1)
    print_usage ();
  endif

  if (! (isnumeric (x) || islogical (x)))
    error ("mad: X must be a numeric vector or matrix");
  endif

  if (isempty (opt))
    opt = 0;
  elseif (! isscalar (opt) || (opt != 0 && opt != 1))
    error ("mad: OPT must be 0 or 1");
  endif

  if (nargin < 3)
    ## Dim not provided

    ## First check for special empty case.
    if (isempty (x) && ndims (x) == 2 && size (x) == [0, 0])
      if (isa (x, "single"))
        m = NaN ("single");
      else
        m = NaN;
      endif
      return;
    endif

    ## Then find the first non-singleton dimension.
    (dim = find (size (x) != 1, 1)) || (dim = 1);
  endif

  if (opt == 0)
    fcn = @mean;
  else
    fcn = @median;
  endif

  m = fcn (abs (x - fcn (x, dim, "omitnan")), dim, "omitnan");

endfunction


%!assert (mad (123), 0)
%!assert (mad (Inf), NaN)
%!assert (mad ([3, Inf]),Inf)
%!assert (mad ([0 0 1 2 100]), 31.76)
%!assert (mad (single ([0 0 1 2 100])), single (31.76))
%!assert (mad ([0 0 1 2 100]'), 31.76)
%!assert (mad ([0 0 1 2 100], 1), 1)
%!assert (mad (single ([0 0 1 2 100]), 1), single (1))
%!assert (mad ([0 0 1 2 100]', 1), 1)
%!assert (mad (magic (4)), [4, 4, 4, 4])
%!assert (mad (magic (4), [], 2), [6; 2; 2; 6])
%!assert (mad (magic (4), 1), [2.5, 3.5, 3.5, 2.5])
%!assert (mad (magic (4), 1, 2), [5.5; 1.5; 1.5; 5.5])
%!assert (mad (magic (4), 0, 3), zeros (4))
%!assert (mad (magic (4), 1, 3), zeros (4))
%!assert (mad (cat (3, magic (4), magic (4))), 4 * ones (1, 4, 2))

## Test all and vecdim options
%!assert (mad (magic (4), 0, "all"), 4)
%!assert (mad (magic (4), 1, "all"), 4)
%!assert (mad (magic (4), 0, [1 2]), 4)
%!assert (mad (magic (4), 0, [1 3]), mad (magic(4), 0, 1))
%!assert (mad (magic (4), 0, [1 2 3]), 4)
%!assert (mad (magic (4), 1, [1 2]), 4)
%!assert (mad (magic (4), 1, [1 3]), mad (magic(4), 1, 1))
%!assert (mad (magic (4), 1, [1 2 3]), 4)
%!assert (mad (magic (4), 0, [3 4 99]), zeros (4))
%!assert (mad (magic (4), 1, [3 4 99]), zeros (4))

## Verify ignoring NaN values unless all NaN
%!assert (mad (NaN), NaN)
%!assert (mad (NaN (2)), NaN(1,2))
%!assert (mad ([1,2;3,NaN]), [1, 0])
%!assert (mad ([1,2;3,NaN], [], 1), [1, 0])
%!assert (mad ([1,2;3,NaN], [], 2), [0.5; 0], eps)
%!assert (mad ([1,NaN;3,NaN], [], 1), [1, NaN])
%!assert (mad ([1,NaN;3,NaN], [], 2), [0; 0])

## Verify compatible empty handling
%!assert (mad ([]), NaN)
%!assert (mad ([], 0, 1), NaN (1,0))
%!assert (mad ([], 0, 2), NaN (0,1))
%!assert (mad ([], 0, 3), NaN (0,0))
%!assert (mad (single ([])), NaN ('single'))
%!assert (mad (ones (0,1)), NaN)
%!assert (mad (ones (0,1), 0, 1), NaN (1,1))
%!assert (mad (ones (0,1), 0, 2), NaN (0,1))
%!assert (mad (ones (0,1), 0, 3), NaN (0,1))
%!assert (mad (ones (1,0)), NaN)
%!assert (mad (ones (1,0), 0, 1), NaN (1,0))
%!assert (mad (ones (1,0), 0, 2), NaN (1,1))
%!assert (mad (ones (1,0), 0, 3), NaN (1,0))
%!assert (mad (ones (0,0,0)), NaN (1,0,0))
%!assert (mad (ones (1,0,0)), NaN (1,1,0))
%!assert (mad (ones (0,1,0)), NaN (1,1,0))
%!assert (mad (ones (0,0,0)), NaN (1,0,0))
%!assert (mad (ones (0,1,0), 0, 1), NaN (1,1,0))
%!assert (mad (ones (0,1,0), 0, 2), NaN (0,1,0))
%!assert (mad (ones (0,1,0), 0, 3), NaN (0,1,1))
%!assert (mad (ones (0,1,0), 0, 4), NaN (0,1,0))
%!assert (mad (ones (0,2,1,0)), ones (1,2,1,0))
%!assert (mad (ones (2,0,1,0)), ones (1,0,1,0))

## Test input case insensitivity
%!assert (mad ([1 2 3], 0, "aLL"), 2/3, eps)
%!assert (mad ([1 2 3], 1, "aLL"), 1)

## Test input validation
%!error <Invalid call> mad ()
%!error <X must be a numeric> mad (['A'; 'B'])
%!error <OPT must be 0 or 1> mad (1, 2)
