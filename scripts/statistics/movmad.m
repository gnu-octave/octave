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
## @deftypefn  {} {@var{y} =} movmad (@var{x}, @var{wlen})
## @deftypefnx {} {@var{y} =} movmad (@var{x}, [@var{nb}, @var{na}])
## @deftypefnx {} {@var{y} =} movmad (@dots{}, @var{dim})
## @deftypefnx {} {@var{y} =} movmad (@dots{}, "@var{nancond}")
## @deftypefnx {} {@var{y} =} movmad (@dots{}, @var{property}, @var{value})
## Calculate the moving mean absolute deviation over a sliding window of length
## @var{wlen} on data @var{x}.
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
## @seealso{mad, movfun, movslice, movmax, movmean, movmedian, movmin,
## movprod, movstd, movsum, movvar}
## @end deftypefn

function y = movmad (x, wlen, varargin)

  if (nargin < 2)
    print_usage ();
  endif

  y = movfun (@mad, x, wlen, __parse_movargs__ ("movmad", varargin{:}){:});

endfunction


%!assert (movmad (1:5, 3), [1/2, 2/3, 2/3, 2/3, 1/2], eps)
%!assert (movmad (1:5, [1, 1]), [1/2, 2/3, 2/3, 2/3, 1/2], eps)
%!assert (movmad (1:5, 3, 2), [1/2, 2/3, 2/3, 2/3, 1/2], eps)
%!assert <*65928> (movmad (1:5, 3, 1), zeros (1, 5))
%!assert <*65928> (movmad (1:5, 3, 3), zeros (1, 5))

%!assert (movmad (magic (4), 3), ...
%!       [5.5, 4.5, 3.5, 2.5; 4, 28/9, 22/9, 2; ...
%!        2, 22/9, 28/9, 4; 2.5, 3.5, 4.5, 5.5], 3*eps)
%!assert (movmad (magic (4), 3, 1), ...
%!       [5.5, 4.5, 3.5, 2.5; 4, 28/9, 22/9, 2; ...
%!        2, 22/9, 28/9, 4; 2.5, 3.5, 4.5, 5.5], 3*eps)
%!assert (movmad (magic (4), 3, 2), ...
%!       [7, 6, 14/3, 5; 3, 22/9, 10/9, 1; ...
%!        1, 10/9, 22/9, 3; 5, 14/3, 6, 7], 3*eps)
%!assert <*65928> (movmad (magic (4), 3, 3), zeros (4, 4))

%!assert <*55241> (movmad ((1:10).', 3), [0.5; repmat(2/3, 8, 1); 0.5], eps)

## Test input validation
%!error <Invalid call> movmad ()
%!error <Invalid call> movmad (1)
