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
## @deftypefn  {} {@var{m} =} moment (@var{x}, @var{p})
## @deftypefnx {} {@var{m} =} moment (@var{x}, @var{p}, @var{dim})
## @deftypefnx {} {@var{m} =} moment (@var{x}, @var{p}, @var{vecdim})
## @deftypefnx {} {@var{m} =} moment (@var{x}, @var{p}, "all")
## @deftypefnx {} {@var{m} =} moment (@var{x}, @var{p}, @dots{}, @var{type})
## Compute the @var{p}-th central moment of the input data @var{x}.
##
## The @var{p}-th central moment of @var{x} is defined as:
## @tex
## $$
## {\sum_{i=1}^N (x_i - \bar{x})^p \over N}
## $$
## where $\bar{x}$ is the mean value of @var{x} and $N$ is the number of elements of @var{x}.
##
## @end tex
## @ifnottex
##
## @example
## @group
## 1/N SUM_i (@var{x}(i) - mean(@var{x}))^@var{p}
## @end group
## @end example
##
## @noindent
## where @math{N} is the length of the @var{x} vector.
##
## @end ifnottex
##
## If @var{x} is a vector, then @code{moment (@var{x})} computes the @var{p}-th
## central moment of the data in @var{x}.
##
## If @var{x} is a matrix, then @code{moment (@var{x})} returns a vector with
## element containing the @var{p}-th central moment of the corresponding column
## in @var{x}.
##
## If @var{x} is an array, then @code{moment (@var{x})} computes the @var{p}-th
## central moment along the first non-singleton dimension of @var{x}.
##
## The data in @var{x} must be a non-empty numeric array and any NaN values
## along the operating dimension will return NaN for central moment.  The size
## of @var{m} is equal to the size of @var{x} except for the operating
## dimension, which becomes 1.
##
## The optional input @var{dim} specifies the dimension to operate on and must
## be a positive integer.  Specifying any singleton dimension of @var{x},
## including any dimension exceeding @code{ndims (@var{x})}, will return
## @var{x}.
##
## Specifying multiple dimensions with input @var{vecdim}, a vector of
## non-repeating dimensions, will operate along the array slice defined by
## @var{vecdim}.  If @var{vecdim} indexes all dimensions of @var{x}, then it is
## equivalent to the option @qcode{"all"}.  Any dimension in @var{vecdim}
## greater than @code{ndims (@var{x})} is ignored.  If all dimensions in
## @var{vecdim} are greater than @code{ndims (@var{x})}, then @code{moment}
## will return @var{x}.
##
## Specifying the dimension as @qcode{"all"} will cause @code{moment} to operate
## on all elements of @var{x}, and is equivalent to @code{moment (@var{x}(:))}.
##
## The optional fourth input argument, @var{type}, is a string specifying the
## type of moment to be computed.
## Valid options are:
##
## @table @asis
## @item @qcode{"c"}
##   Central Moment (default).
##
## @item  @qcode{"a"}
## @itemx @qcode{"ac"}
##   Absolute Central Moment.  The moment about the mean ignoring sign
## defined as
## @tex
## $$
## {\sum_{i=1}^N {\left| x_i - \bar{x} \right|}^p \over N}
## $$
## @end tex
## @ifnottex
##
## @example
## @group
## 1/N SUM_i (abs (@var{x}(i) - mean(@var{x})))^@var{p}
## @end group
## @end example
##
## @end ifnottex
##
## @item @qcode{"r"}
##   Raw Moment.  The moment about zero defined as
## @tex
## $$
## {\rm moment} (x) = { \sum_{i=1}^N {x_i}^p \over N }
## $$
## @end tex
## @ifnottex
##
## @example
## @group
## moment (@var{x}) = 1/N SUM_i @var{x}(i)^@var{p}
## @end group
## @end example
##
## @end ifnottex
##
## @item @nospell{@qcode{"ar"}}
##   Absolute Raw Moment.  The moment about zero ignoring sign defined as
## @tex
## $$
## {\sum_{i=1}^N {\left| x_i \right|}^p \over N}
## $$
## @end tex
## @ifnottex
##
## @example
## @group
## 1/N SUM_i ( abs (@var{x}(i)) )^@var{p}
## @end group
## @end example
##
## @end ifnottex
## @end table
## @seealso{var, skewness, kurtosis}
## @end deftypefn

## Can easily be made to work for continuous distributions (using quad)
## as well, but how does the general case work?

function m = moment (x, p, dim = [], type = 'c')

  if (nargin < 2)
    print_usage ();
  endif

  if (! isnumeric (x) || isempty (x))
    error ("moment: X must be a non-empty numeric array");
  endif

  if (! (isnumeric (p) && isscalar (p)))
    error ("moment: P must be a numeric scalar");
  endif

  sz = size (x);
  if (isempty (dim))
    ## Find the first non-singleton dimension.
    (dim = find (sz > 1, 1)) || (dim = 1);
  elseif (strcmp (dim, "all"))
    ## Handle "all" option so we can get sample size
    x = x(:);
    dim = 1;
  endif

  if (! (ischar (type) && isvector (type)))
    error ("moment: TYPE must be a character vector");
  endif

  n = prod (size (x, dim));

  if (! any (type == "r"))
    x = center (x, dim);
  endif
  if (any (type == "a"))
    x = abs (x);
  endif

  m = sum (x .^ p, dim) / n;

endfunction


%!shared x, xx
%! x = rand (10);
%! xx = randi (10, 4, 4, 4);
%!assert (moment (x, 1), mean (center (x)), eps)
%!assert (moment (x, 2), meansq (center (x)), eps)
%!assert (moment (x, 1, 2), mean (center (x, 2), 2), eps)
%!assert (moment (x, 1, [], "a"), mean (abs (center (x))), eps)
%!assert (moment (x, 1, [], "r"), mean (x), eps)
%!assert (moment (x, 1, [], "ar"), mean (abs (x)), eps)
%!assert (moment (x, 1, 1, "a"), mean (abs (center (x)), 1), eps)
%!assert (moment (x, 1, 1, "r"), mean (x, 1), eps)
%!assert (moment (x, 1, 1, "ar"), mean (abs (x), 1), eps)
%!assert (moment (x, 1, 2, "a"), mean (abs (center (x, 2)), 2), eps)
%!assert (moment (x, 1, 2, "r"), mean (x, 2), eps)
%!assert (moment (x, 1, 2, "ar"), mean (abs (x), 2), eps)

%!assert (moment (single ([1 2 3]), 1, [], "r"), single (2))

## Test vecdim and "all"
%!assert (moment (x, 1, [1, 2]), mean (center (x, [1, 2]), [1, 2]), eps)
%!assert (moment (x, 2, [1, 2]), meansq (center (x, [1, 2]), [1, 2]), eps)
%!assert (moment (x, 1, [1, 3]), mean (center (x, [1, 3]), [1, 3]), eps)
%!assert (moment (x, 2, [1, 3]), meansq (center (x, [1, 3]), [1, 3]), eps)
%!assert (moment (x, 1, [2, 3]), mean (center (x, [2, 3]), [2, 3]), eps)
%!assert (moment (x, 2, [2, 3]), meansq (center (x, [2, 3]), [2, 3]), eps)
%!assert (moment (x, 1, "all"), mean (center (x, "all"), "all"), eps)
%!assert (moment (x, 2, "all"), meansq (center (x, "all"), "all"), eps)

%!assert (moment (1, 2, 4), 0)

## Test input validation
%!error <Invalid call> moment ()
%!error <Invalid call> moment (1)
%!error <moment: X must be a non-empty numeric array> moment (['A'; 'B'], 2)
%!error <moment: X must be a non-empty numeric array> moment (ones (2,0,3), 2)
%!error <moment: X must be a non-empty numeric array> moment ([], 1)
%!error <moment: P must be a numeric scalar> moment (1, ones (2,2))
%!error <moment: P must be a numeric scalar> moment (1, true)
%!error <moment: P must be a numeric scalar> moment (1, "str")
%!error <moment: TYPE must be a character vector> moment (1, 2, 3, 4)
