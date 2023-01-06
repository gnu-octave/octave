########################################################################
##
## Copyright (C) 2017-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{y} =} mad (@var{x})
## @deftypefnx {} {@var{y} =} mad (@var{x}, @var{opt})
## @deftypefnx {} {@var{y} =} mad (@var{x}, @var{opt}, @var{dim})
## Compute the mean or median absolute deviation of the elements of @var{x}.
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
## If @var{x} is a matrix, compute @code{mad} for each column and return
## results in a row vector.  For a multi-dimensional array, the calculation is
## done over the first non-singleton dimension.
##
## The optional argument @var{opt} determines whether mean or median absolute
## deviation is calculated.  The default is 0 which corresponds to mean
## absolute deviation; A value of 1 corresponds to median absolute deviation.
##
## If the optional argument @var{dim} is given, operate along this dimension.
##
## As a measure of dispersion, @code{mad} is less affected by outliers than
## @code{std}.
## @seealso{bounds, range, iqr, std, mean, median}
## @end deftypefn

function retval = mad (x, opt = 0, dim)

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

  sz = size (x);
  if (nargin < 3)
    ## Find the first non-singleton dimension.
    (dim = find (sz > 1, 1)) || (dim = 1);
  else
    if (! (isscalar (dim) && dim == fix (dim) && dim > 0))
      error ("mad: DIM must be an integer and a valid dimension");
    endif
  endif

  if (opt == 0)
    fcn = @mean;
  else
    fcn = @median;
  endif

  retval = fcn (abs (x - fcn (x, dim)), dim);

endfunction


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

## Test input validation
%!error <Invalid call> mad ()
%!error <X must be a numeric> mad (['A'; 'B'])
%!error <OPT must be 0 or 1> mad (1, 2)
%!error <DIM must be an integer> mad (1, [], ones (2,2))
%!error <DIM must be an integer> mad (1, [], 1.5)
%!error <DIM must be .* a valid dimension> mad (1, [], 0)
