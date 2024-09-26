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
## @deftypefn  {} {@var{y} =} movmax (@var{x}, @var{wlen})
## @deftypefnx {} {@var{y} =} movmax (@var{x}, [@var{nb}, @var{na}])
## @deftypefnx {} {@var{y} =} movmax (@dots{}, @var{dim})
## @deftypefnx {} {@var{y} =} movmax (@dots{}, "@var{nancond}")
## @deftypefnx {} {@var{y} =} movmax (@dots{}, @var{property}, @var{value})
## Calculate the moving maximum over a sliding window of length @var{wlen} on
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
## and @code{NA} values affect the output of @qcode{"movmax"}. The value
## @qcode{"includenan"} causes @code{NaN} and @code{NA} values to be
## included in the moving window, and any window slice containing @code{NaN} or
## @code{NA} values will return @code{NaN} for that element.  The value
## @qcode{"omitnan"} (default) causes @qcode{"movmax"} to ignore any @code{NaN}
## or @code{NA} values resulting in fewer elements being used to calculate the
## maximum for that window slice.  If @qcode{"omitnan"} is specified and a
## window slice contains all @code{NaN} or @code{NA} values, @qcode{"movmax"}
## returns @code{NaN} for that element.  The values @qcode{"includemissing"} and
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
## @seealso{max, movfun, movslice, movmad, movmean, movmedian, movmin,
## movprod, movstd, movsum, movvar}
## @end deftypefn

function y = movmax (x, wlen, varargin)

  if (nargin < 2)
    print_usage ();
  endif

  if (any (strcmpi (varargin, "samplepoints")))
    ## Avoid error mixing certain Endpoints & SamplePoints combinations.
    y = movfun (@max, x, wlen, "nancond", "omitnan",
                __parse_movargs__ ("movmax", varargin{:}){:});
  else
    y = movfun (@max, x, wlen, "nancond", "omitnan", "Endpoints", -Inf,
                __parse_movargs__ ("movmax", varargin{:}){:});
  endif
endfunction


%!assert (movmax (1:5, 3), [2:5, 5])
%!assert (movmax (1:5, [1, 1]), [2:5, 5])
%!assert (movmax (1:5, 3, 2), [2:5, 5])
%!assert <*65928> (movmax (1:5, 3, 1), 1:5)
%!assert <*65928> (movmax (1:5, 3, 3), 1:5)

%!assert (movmax (magic (4), 3), [16, 11, 10, 13; 16, 11, 10, 13; ...
%!                                9, 14, 15, 12; 9, 14, 15, 12])
%!assert (movmax (magic (4), 3, 1), [16, 11, 10, 13; 16, 11, 10, 13; ...
%!                                   9, 14, 15, 12; 9, 14, 15, 12])
%!assert (movmax (magic (4), 3, 2), [16, 16, 13, 13; 11, 11, 11, 10; ...
%!                                   9, 9, 12, 12; 14, 15, 15, 15])
%!assert <*65928> (movmax (magic (4), 3, 3), magic (4))

%!assert <*55241> (movmax ((1:10).', 3), [(2:10).'; 10])

%!assert <66156> (movmax ([1:4, NaN(1,3), 8:10], 3), movmax ([1:4, NaN(1,3), 8:10], 3, "omitnan"))
%!assert <66156> (movmax ([1:4, NaN(1,3), 8:10], 3, "includenan"), [2:4, NaN(1,5), 10, 10])
%!assert <66156> (movmax ([1:4, NaN(1,3), 8:10], 3, "omitnan"), [2:4, 4, 4, NaN, 8, 9, 10, 10])

%!assert <*66025> (movmax (1:5, 3, "samplepoints", [1:4, 6]), [2, 3, 4, 4, 5])

## Test input validation
%!error <Invalid call> movmax ()
%!error <Invalid call> movmax (1)
