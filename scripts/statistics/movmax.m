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
## The optional string argument @qcode{"@var{nancond}"} controls whether
## @code{NaN} and @code{NA} values should be included (@qcode{"includenan"}),
## or excluded (@qcode{"omitnan"}), from the data passed to @code{mad}.  The
## default is @qcode{"includenan"}.  Caution: the @qcode{"omitnan"} option is
## not yet implemented.
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

  y = movfun (@max, x, wlen, "Endpoints", -Inf,
              __parse_movargs__ ("movmax", varargin{:}){:});

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

## Test input validation
%!error <Invalid call> movmax ()
%!error <Invalid call> movmax (1)
