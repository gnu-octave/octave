########################################################################
##
## Copyright (C) 2018-2024 The Octave Project Developers
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
## @deftypefn  {} {@var{y} =} movmin (@var{x}, @var{wlen})
## @deftypefnx {} {@var{y} =} movmin (@var{x}, [@var{nb}, @var{na}])
## @deftypefnx {} {@var{y} =} movmin (@dots{}, @var{dim})
## @deftypefnx {} {@var{y} =} movmin (@dots{}, "@var{nancond}")
## @deftypefnx {} {@var{y} =} movmin (@dots{}, @var{property}, @var{value})
## Calculate the moving minimum over a sliding window of length @var{wlen} on
## data @var{x}.
##
## The moving window length input @var{wlen} can either be a numeric scalar
## or a 2-element numeric array. The elements included in the moving window
## depend on both the size and value of @var{wlen} as follows:
##
## For integer-valued @var{wlen}:
## @itemize
## @item
## For odd, integer-valued, scalar @var{wlen} the window is symmetric and includes
## @w{@code{(@var{wlen} - 1) / 2}} elements on either side of the central
## element.  For example, when calculating the output at index 5 with a
## window length of 3, @code{movmin} uses data elements @w{@code{[4, 5, 6]}}.
## @item
## For even, integer-valued, scalar @var{wlen} the window is asymmetric and has
## @w{@code{@var{wlen}/2}} elements to the left of the central element and
## @w{@code{@var{wlen}/2 - 1}} elements to the right of the central
## element.  For example, when calculating the output at index 5 with a
## window length of 4, @code{movmin} uses data elements
## @w{@code{[3, 4, 5, 6]}}.
## @item
## For integer-valued vector @var{wlen} of the form
## @w{@qcode{[@var{nb}, @var{na}]}} where @var{nb} and @var{na} are integer
## valued the window includes @var{nb} elements to the left of the central
## element and @var{na} elements to the right of the central element.  For
## example, given @w{@code{@var{wlen} = [3, 0]}}, the data used to calculate
## index 5 is @w{@code{[2, 3, 4, 5]}}.
## @end itemize
##
## For non-integer-valued scalar @var{wlen}:
## @itemize
## @item
## Non-integer-valued scalar @var{wlen} will be converted to
## two-element vector form with
## @w{@code{@var{nb} = @var{na} = fix (@var{wlen} / 2)}}, and then processed
## as stated above for integer-valued vectors.  For example, when
## calculating the output at index 5 with @w{@code{@var{wlen} = 2.5}},
## @code{movmin} uses data elements @w{@code{[3, 4, 5, 6, 7]}}.
## @item
## Non-integer-valued vector @var{wlen} will be  truncated to interger values
## with @w{@code{@var{wlen} = fix (@var{wlen}}}, and then processed as
## stated above for integer-valued vectors. For example, when
## calculating the output at index 5 with @w{@code{@var{wlen} = [1.2, 2.3]}},
## @code{movmin} uses data elements @w{@code{[4, 5, 6, 7]}}.
## @end itemize
##
## If the optional argument @var{dim} is given, operate along this dimension.
##
## The optional string argument @qcode{"@var{nancond}"} controls whether
## @code{NaN} and @code{NA} values should be included (@qcode{"includenan"}),
## or excluded (@qcode{"omitnan"}), from the data passed to @code{min}.  The
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
## length 3, @code{@var{y}(1) = min (@var{x}(1:2))}, and
## @code{@var{y}(end) = min (@var{x}(end-1:end))}.
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
## @code{@var{y}(1) = min ([NaN, @var{x}(1:2)])}, and
## @code{@var{y}(end) = min ([@var{x}(end-1:end), NaN])}.
## This option usually results in @var{y} having @code{NaN} values at the
## boundaries, although it is influenced by how @code{min} handles @code{NaN},
## and also by the property @qcode{"nancond"}.
##
## @item @var{user_value}
## Any window elements outside the data array are replaced by the specified
## value @var{user_value} which must be a numeric scalar.  For example, with a
## window of length 3,
## @code{@var{y}(1) = min ([@var{user_value}, @var{x}(1:2)])}, and
## @code{@var{y}(end) = min ([@var{x}(end-1:end), @var{user_value}])}.
## A common choice for @var{user_value} is 0.
##
## @item @qcode{"same"}
## Any window elements outside the data array are replaced by the value of
## @var{x} at the boundary.  For example, with a window of length 3,
## @code{@var{y}(1) = min ([@var{x}(1), @var{x}(1:2)])}, and
## @code{@var{y}(end) = min ([@var{x}(end-1:end), @var{x}(end)])}.
##
## @item @qcode{"periodic"}
## The window is wrapped so that any missing data elements are taken from
## the other side of the data.  For example, with a window of length 3,
## @code{@var{y}(1) = min ([@var{x}(end), @var{x}(1:2)])}, and
## @code{@var{y}(end) = min ([@var{x}(end-1:end), @var{x}(1)])}.
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
## @seealso{movfun, movslice, movmad, movmax, movmean, movmedian, movprod,
## movstd, movsum, movvar}
## @end deftypefn

function y = movmin (x, wlen, varargin)

  if (nargin < 2)
    print_usage ();
  endif

  y = movfun (@min, x, wlen, "Endpoints", Inf,
              __parse_movargs__ ("movmin", varargin{:}){:});

endfunction


%!assert (movmin (1:5, 3), [1, 1:4])
%!assert (movmin (1:5, [1, 1]), [1, 1:4])
%!assert (movmin (1:5, 3, 2), [1, 1:4])

%!assert (movmin (magic (4), 3), [5, 2, 3, 8; 5, 2, 3, 8; ...
%!                                4, 7, 6, 1; 4, 7, 6, 1])
%!assert (movmin (magic (4), 3, 1), [5, 2, 3, 8; 5, 2, 3, 8; ...
%!                                4, 7, 6, 1; 4, 7, 6, 1])
%!assert (movmin (magic (4), 3, 2), [2, 2, 2, 3; 5, 5, 8, 8; ...
%!                                   7, 6, 6, 6; 4, 4, 1, 1])

%!assert <*55241> (movmin ((1:10).', 3), [1; (1:9).'])

## Test input validation
%!error <Invalid call> movmin ()
%!error <Invalid call> movmin (1)
