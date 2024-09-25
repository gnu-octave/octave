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
## @deftypefn  {} {@var{y} =} movsum (@var{x}, @var{wlen})
## @deftypefnx {} {@var{y} =} movsum (@var{x}, [@var{nb}, @var{na}])
## @deftypefnx {} {@var{y} =} movsum (@dots{}, @var{dim})
## @deftypefnx {} {@var{y} =} movsum (@dots{}, "@var{nancond}")
## @deftypefnx {} {@var{y} =} movsum (@dots{}, @var{property}, @var{value})
## Calculate the moving sum over a sliding window of length @var{wlen} on
## data @var{x}.
##
## The moving window length input @var{wlen} can either be a numeric scalar
## or a 2-element numeric array @w{@qcode{[@var{nb}, @var{na}]}}. The elements
## included in the moving window depend on the size and value of @var{wlen}
## as well as whether the @qcode{"SamplePoints"} option has been specified.
## For full details of element inclusion,
## @pxref{XREFmovslice,,@code{movslice}}.
##
## If the optional argument @var{dim} is given, operate along this dimension.
##
## The optional string argument @qcode{"@var{nancond}"} controls how @code{NaN}
## and @code{NA} values affect the output of @qcode{"movsum"}. The value
## @qcode{"includenan"} (default) causes @code{NaN} and @code{NA} values to be
## included in the moving window, and any window slice containing @code{NaN} or
## @code{NA} values will return @code{NaN} for that element.  The value
## @qcode{"omitnan"} causes @qcode{"movsum"} to ignore any @code{NaN}
## or @code{NA} values resulting in fewer elements being used to calculate the
## sum for that window slice.  If @qcode{"omitnan"} is specified and a window
## slice contains all @code{NaN} or @code{NA} values, @qcode{"movsum"} returns
## 0 for that element.  The values @qcode{"includemissing"} and
## @qcode{"omitmissing"} may be used synonymously with @qcode{"includenan"} and
## @qcode{"omitnan"}, respectively.
##
## The calculation can be controlled by specifying @var{property}/@var{value}
## pairs.  Valid properties are @qcode{"Endpoints"} and
## @qcode{"SamplePoints"}.  For full descriptions of these properties and
## valid options, @pxref{XREFmovfun,,@code{movfun}}.
##
## Programming Note: This function is a wrapper which calls @code{movfun}.
## For full documentation of inputs and options,
## @pxref{XREFmovfun,,@code{movfun}}.
##
## @seealso{sum, movfun, movslice, movmad, movmax, movmean, movmedian, movmin,
## movprod, movstd, movvar}
## @end deftypefn

function y = movsum (x, wlen, varargin)

  if (nargin < 2)
    print_usage ();
  endif

  y = movfun (@sum, x, wlen, "nanval", 0, "Endpoints", 0,
              __parse_movargs__ ("movsum", varargin{:}){:});

endfunction


%!assert (movsum (1:5, 3), [3, 6, 9, 12, 9])
%!assert (movsum (1:5, [1, 1]), [3, 6, 9, 12, 9])
%!assert (movsum (1:5, 3, 2), [3, 6, 9, 12, 9])
%!assert <*65928> (movsum (1:5, 3, 1), 1:5)
%!assert <*65928> (movsum (1:5, 3, 3), 1:5)

%!assert (movsum (magic (4), 3), [21, 13, 13, 21; 30, 20, 19, 33; ...
%!                                18, 32, 31, 21; 13, 21, 21, 13])
%!assert (movsum (magic (4), 3, 1), [21, 13, 13, 21; 30, 20, 19, 33; ...
%!                                18, 32, 31, 21; 13, 21, 21, 13])
%!assert (movsum (magic (4), 3, 2), [18, 21, 18, 16; 16, 26, 29, 18; ...
%!                                   16, 22, 25, 18; 18, 33, 30, 16])
%!assert <*65928> (movsum (magic (4), 3, 3), magic (4))

%!assert <*55241> (movsum ((1:10).', 3), [(3:3:27).'; 19])

%!assert <66156> (movsum ([1:4, NaN(1,3), 8:10], 3), movsum ([1:4, NaN(1,3), 8:10], 3, "includenan"))
%!assert <66156> (movsum ([1:4, NaN(1,3), 8:10], 3, "includenan"), [3:3:9, NaN(1,5), 27, 19])
%!assert <66156> (movsum ([1:4, NaN(1,3), 8:10], 3, "omitnan"), [3:3:9, 7, 4, 0, 8, 17, 27, 19])

## Test input validation
%!error <Invalid call> movsum ()
%!error <Invalid call> movsum (1)
