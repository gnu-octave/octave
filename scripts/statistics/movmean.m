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
## @deftypefn  {} {@var{y} =} movmean (@var{x}, @var{wlen})
## @deftypefnx {} {@var{y} =} movmean (@var{x}, [@var{nb}, @var{na}])
## @deftypefnx {} {@var{y} =} movmean (@dots{}, @var{dim})
## @deftypefnx {} {@var{y} =} movmean (@dots{}, "@var{nancond}")
## @deftypefnx {} {@var{y} =} movmean (@dots{}, @var{property}, @var{value})
## Calculate the moving average over a sliding window of length @var{wlen} on
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
## @seealso{mean, movfun, movslice, movmad, movmax, movmedian, movmin,
## movprod, movstd, movsum, movvar}
## @end deftypefn

function y = movmean (x, wlen, varargin)

  if (nargin < 2)
    print_usage ();
  endif

  y = movfun (@mean, x, wlen, __parse_movargs__ ("movmean", varargin{:}){:});

endfunction


%!assert (movmean (1:5, 3), [1.5, 2, 3, 4, 4.5], eps)
%!assert (movmean (1:5, [1, 1]), [1.5, 2, 3, 4, 4.5], eps)
%!assert (movmean (1:5, 3, 2), [1.5, 2, 3, 4, 4.5], eps)
%!assert <*65928> (movmean (1:5, 3, 1), 1:5)
%!assert <*65928> (movmean (1:5, 3, 3), 1:5)

%!assert (movmean (magic (4), 3), ...
%!                 [10.5, 6.5, 6.5, 10.5; 10, 20/3, 19/3, 11; ...
%!                  6, 32/3, 31/3, 7; 6.5, 10.5, 10.5, 6.5], eps)
%!assert (movmean (magic (4), 3, 1), ...
%!                 [10.5, 6.5, 6.5, 10.5; 10, 20/3, 19/3, 11; ...
%!                  6, 32/3, 31/3, 7; 6.5, 10.5, 10.5, 6.5], eps)
%!assert (movmean (magic (4), 3, 2), ...
%!                 [9, 7, 6, 8; 8, 26/3, 29/3, 9; ...
%!                  8, 22/3, 25/3, 9; 9, 11, 10, 8], eps)
%!assert <*65928> (movmean (magic (4), 3, 3), magic (4))

%!assert <*55241> (movmean ((1:10).', 3), [1.5; (2:9).'; 9.5])

## Test input validation
%!error <Invalid call> movmean ()
%!error <Invalid call> movmean (1)
