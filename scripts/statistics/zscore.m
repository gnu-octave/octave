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
## @deftypefn  {} {@var{z} =} zscore (@var{x})
## @deftypefnx {} {@var{z} =} zscore (@var{x}, @var{opt})
## @deftypefnx {} {@var{z} =} zscore (@var{x}, @var{opt}, @var{dim})
## @deftypefnx {} {@var{z} =} zscore (@var{x}, @var{opt}, @var{vecdim})
## @deftypefnx {} {@var{z} =} zscore (@var{x}, @var{opt}, "all")
## @deftypefnx {} {@var{z} =} zscore (@dots{}, @var{nanflag})
## @deftypefnx {} {[@var{z}, @var{mu}, @var{sigma}] =} zscore (@dots{})
## Compute the Z score of @var{x}.
##
## If @var{x} is a vector, subtract its mean and divide by its standard
## deviation.  If the standard deviation is zero, divide by 1 instead.
##
## The optional parameter @var{opt} determines the normalization to use when
## computing the standard deviation and has the same definition as the
## corresponding parameter for @code{std}.
##
## If @var{x} is a matrix, calculate along the first non-singleton dimension.
## If the third optional argument @var{dim} is given, operate along this
## dimension.
##
## Specifying the dimensions as @var{vecdim}, a vector of non-repeating
## dimensions will return the mean over the array slice defined by
## @var{vecdim}.  If @var{vecdim} indexes all dimensions of @var{x}, then it is
## equivalent to the option @qcode{"all"}.  Any dimension in @var{vecdim}
## greater than @code{ndims (@var{x})} is ignored.
##
## Specifying the dimension as @qcode{"all"} will force @code{zscore} to
## operate on all elements of @var{x}, and is equivalent to
## @code{reshape (zscore (@var{x}(:)), size (x))}.
##
## The optional variable @var{nanflag} specifies whether to include or exclude
## @code{NaN} values from the calculation using any of the previously specified
## input argument combinations.  The default value for @var{nanflag} is
## @qcode{"includenan"} which keeps @code{NaN} values in the calculation.  To
## exclude @code{NaN} values, set the value of @var{nanflag} to
## @qcode{"omitnan"}.  The output will still contain @code{NaN} values at the
## same locations as in @var{x}.
##
## The optional outputs @var{mu} and @var{sigma} contain the mean and standard
## deviation.
##
## @seealso{mean, std, center}
## @end deftypefn

function [z, mu, sigma] = zscore (x, opt = 0, varargin)

  if (nargin < 1)
    print_usage ();
  endif

  if (! (isnumeric (x) || islogical (x)))
    error ("zscore: X must be a numeric vector or matrix");
  endif

  if (isempty (opt))
    opt = 0;
  elseif (! isscalar (opt) || (opt != 0 && opt != 1))
    error ("zscore: normalization OPT must be empty, 0, or 1");
  endif

  sz = size (x);
  if (any (sz == 0))
    z = mu = sigma = x;
  else

    if (isinteger (x))
      x = double (x);
    endif

    mu = mean (x, varargin{:});
    sigma = std (x, opt, varargin{:});
    s = sigma;
    s(s==0) = 1;
    z = (x - mu) ./ s;
  endif

endfunction


%!assert (zscore ([1,2,3]), [-1,0,1])
%!assert (zscore (single ([1,2,3])), single ([-1,0,1]))
%!assert (zscore (int8 ([1,2,3])), [-1,0,1])
%!assert (zscore (ones (3,2,2,2)), zeros (3,2,2,2))
%!assert (zscore ([2,0,-2;0,2,0;-2,-2,2]), [1,0,-1;0,1,0;-1,-1,1])
%!assert <*54531> (zscore ([1,2,3], [], 2), [-1,0,1])
%!assert (size (zscore (ones (3,2,0,2))), [3,2,0,2])
%!assert (ndims (zscore (ones (3,2,0,1))), 3)
%!test
%! [z, mu, sigma] = zscore (ones (3,2,0,1));
%! assert (z, ones (3,2,0,1));
%! assert (size (mu), [3,2,0]);
%! assert (isempty (sigma), true);
%!assert (zscore (repmat (1:3, [3,3,3]), [], 1), zeros (3,9,3))
%!assert (zscore (repmat (1:3, [3,3,3]), [], [1,3]), zeros (3,9,3))
%!assert (zscore (repmat (1:3, [3,3,3]), [], [1,2]), ...
%! repmat ([-1,0,1] * 1.2019, [3,3,3]), 1e-4)
%!assert (zscore (repmat (1:3, [3,3,3]), [], [2,3]), ...
%! repmat ([-1,0,1] * 1.2019, [3,3,3]), 1e-4)
%!assert (zscore (repmat (1:3, [3,3,3]), [], 2), ...
%! repmat ([-1,0,1] * 1.1547, [3,3,3]), 1e-4)
%!assert (zscore ([1,2,3;4,5,6;7,8,9], [], 1), [-1,-1,-1;0,0,0;1,1,1])
%!assert (zscore ([1,2,3;4,5,6;7,8,9], [], 2), [-1,-1,-1;0,0,0;1,1,1]')
%!assert (zscore ([1,2,3;4,5,6;7,8,9], [], 'all'), ...
%! [-1.4606,-1.0954,-0.7303;-0.3651,0,0.3651;0.7303,1.0954,1.4606], 1e-4)
%!assert (zscore ([1,2,NaN;4,5,6;7,8,9]), [-1,-1,NaN;0,0,NaN;1,1,NaN])
%!assert (zscore ([1,2,NaN;4,5,6;7,8,9], [], 'omitnan'), ...
%! [-1,-1,NaN;0,0,-0.7071;1,1,0.7071], 1e-4)
%!test
%! x = [1,2,NaN;4,5,6;7,8,9];
%! assert (zscore (x, [], 'all', 'omitnan'), ...
%! reshape (zscore (x(:), [], 'omitnan'), size (x)))


## Test input validation
%!error <Invalid call> zscore ()
%!error zscore (1, 2, 3)
%!error <X must be a numeric> zscore (['A'; 'B'])
%!error <OPT must be empty, 0, or 1> zscore (1, ones (2,2))
%!error <OPT must be empty, 0, or 1> zscore (1, 1.5)
%!error <DIM must be .* scalar or vector> zscore (1, [], ones (2,2))
%!error <DIM must be .* integer> zscore (1, [], 1.5)
%!error <DIM must be .* positive> zscore (1, [], 0)
