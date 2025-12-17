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
## @deftypefn  {} {@var{y} =} center (@var{x})
## @deftypefnx {} {@var{y} =} center (@var{x}, @var{dim})
## @deftypefnx {} {@var{y} =} center (@var{x}, @var{vecdim})
## @deftypefnx {} {@var{y} =} center (@var{x}, "all")
## @deftypefnx {} {@var{y} =} center (@dots{}, @var{nanflag})
## Center data by subtracting its mean.
##
## If @var{x} is a vector, then @code{center (@var{x})} computes the centered
## data by subtracting the mean of @var{x} from each element of @var{x}.
##
## If @var{x} is a matrix, then @code{center (@var{x})} returns a row vector
## with each element containing the centered data for each column of @var{x}.
##
## If @var{x} is an array, then @code{center (@var{x})} centers the data alonng
## the first non-singleton dimension of @var{x}.
##
## The data in @var{x} must be numeric.  The size of @var{y} is equal to the
## size of @var{x}.
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
## Specifying the dimension as @qcode{"all"} will cause @code{center} to
## compute the center of all elements of @var{x}, and is equivalent to
## @code{center (@var{x}(:))}.
##
## The optional variable @var{nanflag} specifies whether to include or exclude
## NaN values from the calculation of the mean using any of the previously
## specified input argument combinations.  The default value for @var{nanflag}
## is @qcode{"includenan"} which keeps NaN values in the calculation.  To
## exclude NaN values set the value of @var{nanflag} to @qcode{"omitnan"}.  Any
## NaN value along the operating dimensions will result in all corresponding
## element in @var{y} being NaN.
##
## Programming Note: @code{center} has obvious application for normalizing
## statistical data.  It is also useful for improving the precision of general
## numerical calculations.  Whenever there is a large value that is common
## to a batch of data, the mean can be subtracted off, the calculation
## performed, and then the mean added back to obtain the final answer.
## @seealso{zscore}
## @end deftypefn

function y = center (x, varargin)

  if (nargin < 1 || nargin > 3)
    print_usage ();
  endif

  if (! (isnumeric (x)))
    error ("center: X must be a numeric array");
  endif

  if (nargin < 2)
    ## Find the first non-singleton dimension.
    (dim = find (size (x) != 1, 1)) || (dim = 1);
    nanflag = "includenan";
  else
    if (any (strcmpi (varargin{1}, {"omitnan", "includenan"})))
      (dim = find (size (x) != 1, 1)) || (dim = 1);
      nanflag = varargin{1};
    else
      dim = varargin{1};
      if (nargin > 2)
        nanflag = varargin{2};
        if (! any (strcmpi (nanflag, {"omitnan", "includenan"})))
          error ("center: NANFLAG must be either 'omitnan' or 'includenan'");
        endif
      else
        nanflag = "includenan";
      endif
    endif
  endif

  ## Expand diagonal to full matrix before computation, since broadcasting
  ## is not supported for diagonal matrices (Bug #35787).
  if (! issparse (x) && isdiag (x))
    x = full (x);
  endif

  y = x - mean (x, dim, nanflag);

endfunction


%!assert (center ([1,2,3]), [-1,0,1])
%!assert (center (single ([1,2,3])), single ([-1,0,1]))
%!assert (center (int8 ([1,2,3])), int8([-1,0,1]))
%!assert (center (ones (3,2,0,2)), zeros (3,2,0,2))
%!assert (center (ones (3,2,0,2, "single")), zeros (3,2,0,2, "single"))
%!assert (center (magic (3)), [3,-4,1;-2,0,2;-1,4,-3])
%!assert (center ([1 2 3; 6 5 4], 2), [-1 0 1; 1 0 -1])
%!assert (center (1, 3), 0)

%!assert (center (repmat ([1, 2, 3], 5, 1)), zeros (5, 3))
%!assert (center (repmat ([1, 2, 3], 5, 1), 2), repmat ([-1, 0, 1], 5, 1))

## Test input validation
%!error <Invalid call> center ()
%!error <Invalid call> center (1, 2, 3, 4)
%!error <center: X must be a numeric array> center (['A'; 'B'])
%!error <center: X must be a numeric array> center ([true; false])
%!error <center: X must be a numeric array> center ({1, 2})
