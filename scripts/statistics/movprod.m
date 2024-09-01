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
## @deftypefn  {} {@var{y} =} movprod (@var{x}, @var{wlen})
## @deftypefnx {} {@var{y} =} movprod (@var{x}, [@var{nb}, @var{na}])
## @deftypefnx {} {@var{y} =} movprod (@dots{}, @var{dim})
## @deftypefnx {} {@var{y} =} movprod (@dots{}, "@var{nancond}")
## @deftypefnx {} {@var{y} =} movprod (@dots{}, @var{property}, @var{value})
## Calculate the moving product over a sliding window of length @var{wlen} on
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
## @seealso{prod, movfun, movslice, movmad, movmax, movmean, movmedian,
## movmin, movstd, movsum, movvar}
## @end deftypefn

function y = movprod (x, wlen, varargin)

  if (nargin < 2)
    print_usage ();
  endif

  y = movfun (@prod, x, wlen, "Endpoints", 1,
              __parse_movargs__ ("movprod", varargin{:}){:});

endfunction


%!assert (movprod (1:5, 3), [2, 6, 24, 60, 20])
%!assert (movprod (1:5, [1, 1]), [2, 6, 24, 60, 20])
%!assert (movprod (1:5, 3, 2), [2, 6, 24, 60, 20])
%!assert <*65928> (movprod (1:5, 3, 1), 1:5)
%!assert <*65928> (movprod (1:5, 3, 3), 1:5)

%!assert (movprod (magic (4), 3), [80, 22, 30, 104; 720, 154, 180, 1248; ...
%!                                180, 1078, 900, 96; 36, 98, 90, 12])
%!assert (movprod (magic (4), 3, 1), [80, 22, 30, 104; 720, 154, 180, 1248; ...
%!                                180, 1078, 900, 96; 36, 98, 90, 12])
%!assert (movprod (magic (4), 3, 2), [32, 96, 78, 39; 55, 550, 880, 80; ...
%!                                   63, 378, 504, 72; 56, 840, 210, 15])
%!assert <*65928> (movprod (magic (4), 3, 3), magic (4))

%!assert <*55241> (movprod ((1:10).', 3), ...
%!                 [2; 6; 24; 60; 120; 210; 336; 504; 720; 90])

## Test input validation
%!error <Invalid call> movprod ()
%!error <Invalid call> movprod (1)
