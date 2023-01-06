########################################################################
##
## Copyright (C) 2018-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{y} =} movvar (@var{x}, @var{wlen})
## @deftypefnx {} {@var{y} =} movvar (@var{x}, [@var{nb}, @var{na}])
## @deftypefnx {} {@var{y} =} movvar (@dots{}, @var{opt})
## @deftypefnx {} {@var{y} =} movvar (@dots{}, @var{opt}, @var{dim})
## @deftypefnx {} {@var{y} =} movvar (@dots{}, "@var{nancond}")
## @deftypefnx {} {@var{y} =} movvar (@dots{}, @var{property}, @var{value})
## Calculate the moving variance over a sliding window of length @var{wlen} on
## data @var{x}.
##
## If @var{wlen} is a scalar, the function @code{var} is applied to a
## moving window of length @var{wlen}.  When @var{wlen} is an odd number the
## window is symmetric and includes @w{@code{(@var{wlen} - 1) / 2}} elements on
## either side of the central element.  For example, when calculating the
## output at index 5 with a window length of 3, @code{movvar} uses data
## elements @w{@code{[4, 5, 6]}}.  If @var{wlen} is an even number, the window
## is asymmetric and has @w{@code{@var{wlen}/2}} elements to the left of the
## central element and @w{@code{@var{wlen}/2 - 1}} elements to the right of the
## central element.  For example, when calculating the output at index 5 with a
## window length of 4, @code{movvar} uses data elements
## @w{@code{[3, 4, 5, 6]}}.
##
## If @var{wlen} is an array with two elements @w{@code{[@var{nb}, @var{na}]}},
## the function is applied to a moving window @code{-@var{nb}:@var{na}}.  This
## window includes @var{nb} number of elements @emph{before} the current
## element and @var{na} number of elements @emph{after} the current element.
## The current element is always included.  For example, given
## @w{@code{@var{wlen} = [3, 0]}}, the data used to calculate index 5 is
## @w{@code{[2, 3, 4, 5]}}.
##
## The optional argument @var{opt} determines the type of normalization to use.
## Valid values are
##
## @table @asis
## @item 0:
##   normalize with @math{N-1}, provides the best unbiased estimator of the
## variance [default]
##
## @item 1:
##   normalizes with @math{N}, this provides the second moment around the mean
## @end table
##
## If the optional argument @var{dim} is given, operate along this dimension.
## The normalization argument @var{opt} must be given before the dimension.
##
## The optional string argument @qcode{"@var{nancond}"} controls whether
## @code{NaN} and @code{NA} values should be included (@qcode{"includenan"}),
## or excluded (@qcode{"omitnan"}), from the data passed to @code{var}.  The
## default is @qcode{"includenan"}.  Caution: the @qcode{"omitnan"} option is
## not yet implemented.
##
## The calculation can be controlled by specifying @var{property}/@var{value}
## pairs.  Valid properties are
##
## @table @asis
##
## @item @qcode{"Endpoints"}
##
## This property controls how results are calculated at the boundaries
## (@w{endpoints}) of the window.  Possible values are:
##
## @table @asis
## @item @qcode{"shrink"}  (default)
## The window is truncated at the beginning and end of the array to exclude
## elements for which there is no source data.  For example, with a window of
## length 3, @code{@var{y}(1) = var (@var{x}(1:2))}, and
## @code{@var{y}(end) = var (@var{x}(end-1:end))}.
##
## @item @qcode{"discard"}
## Any @var{y} values that use a window extending beyond the original
## data array are deleted.  For example, with a 10-element data vector and a
## window of length 3, the output will contain only 8 elements.  The first
## element would require calculating the function over indices
## @w{@code{[0, 1, 2]}} and is therefore discarded.  The last element would
## require calculating the function over indices @w{@code{[9, 10, 11]}} and is
## therefore discarded.
##
## @item @qcode{"fill"}
## Any window elements outside the data array are replaced by @code{NaN}.  For
## example, with a window of length 3,
## @code{@var{y}(1) = var ([NaN, @var{x}(1:2)])}, and
## @code{@var{y}(end) = var ([@var{x}(end-1:end), NaN])}.
## This option usually results in @var{y} having @code{NaN} values at the
## boundaries, although it is influenced by how @code{var} handles @code{NaN},
## and also by the property @qcode{"nancond"}.
##
## @item @var{user_value}
## Any window elements outside the data array are replaced by the specified
## value @var{user_value} which must be a numeric scalar.  For example, with a
## window of length 3,
## @code{@var{y}(1) = var ([@var{user_value}, @var{x}(1:2)])}, and
## @code{@var{y}(end) = var ([@var{x}(end-1:end), @var{user_value}])}.
## A common choice for @var{user_value} is 0.
##
## @item @qcode{"same"}
## Any window elements outside the data array are replaced by the value of
## @var{x} at the boundary.  For example, with a window of length 3,
## @code{@var{y}(1) = var ([@var{x}(1), @var{x}(1:2)])}, and
## @code{@var{y}(end) = var ([@var{x}(end-1:end), @var{x}(end)])}.
##
## @item @qcode{"periodic"}
## The window is wrapped so that any missing data elements are taken from
## the other side of the data.  For example, with a window of length 3,
## @code{@var{y}(1) = var ([@var{x}(end), @var{x}(1:2)])}, and
## @code{@var{y}(end) = var ([@var{x}(end-1:end), @var{x}(1)])}.
##
## @end table
##
## @item @qcode{"SamplePoints"}
## Caution: This option is not yet implemented.
##
## @end table
##
## Programming Note: This function is a wrapper which calls @code{movfun}.
## For additional options and documentation, @pxref{XREFmovfun,,@code{movfun}}.
##
## @seealso{movfun, movslice, movmad, movmax, movmean, movmedian, movmin,
## movprod, movstd, movsum}
## @end deftypefn

function y = movvar (x, wlen, varargin)

  if (nargin < 2)
    print_usage ();
  endif

  ## Process "opt" normalization argument
  if (nargin > 2 && isnumeric (varargin{1}))
    if (! varargin{1})
      fcn = @var;
    else
      fcn = @(x) var (x, 1);
    endif
    varargin(1) = [];
  else
    fcn = @var;
  endif

  y = movfun (fcn, x, wlen, __parse_movargs__ ("movvar", varargin{:}){:});

endfunction


## FIXME: Need functional BIST tests
## test for bug #55241
%!assert ([0.5; ones(8,1); 0.5], movvar ((1:10).', 3))

%!test <*56765>
%! x = 1:10;
%! y = movvar (x, 4);
%! y0 = movvar (x, 4, 0);
%! assert (y, y0);
%! y1 = movvar (x, 4, 1);
%! assert (y1(1:3), [1/4, 2/3, 5/4]);

## Test input validation
%!error <Invalid call> movvar ()
%!error <Invalid call> movvar (1)
