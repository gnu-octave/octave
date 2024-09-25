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
## @deftypefn  {} {@var{y} =} movvar (@var{x}, @var{wlen})
## @deftypefnx {} {@var{y} =} movvar (@var{x}, [@var{nb}, @var{na}])
## @deftypefnx {} {@var{y} =} movvar (@dots{}, @var{opt})
## @deftypefnx {} {@var{y} =} movvar (@dots{}, @var{opt}, @var{dim})
## @deftypefnx {} {@var{y} =} movvar (@dots{}, "@var{nancond}")
## @deftypefnx {} {@var{y} =} movvar (@dots{}, @var{property}, @var{value})
## Calculate the moving variance over a sliding window of length @var{wlen} on
## data @var{x}.
##
##
## The moving window length input @var{wlen} can either be a numeric scalar
## or a 2-element numeric array @w{@qcode{[@var{nb}, @var{na}]}}. The elements
## included in the moving window depend on the size and value of @var{wlen}
## as well as whether the @qcode{"SamplePoints"} option has been specified.
## For full details of element inclusion,
## @pxref{XREFmovslice,,@code{movslice}}.
##
## The optional argument @var{opt} determines the type of normalization to use.
## Valid values are:
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
## To use the default value for @var{opt} you may pass an empty input
## argument [].
##
## The optional string argument @qcode{"@var{nancond}"} controls how @code{NaN}
## and @code{NA} values affect the output of @qcode{"movvar"}. The value
## @qcode{"includenan"} (default) causes @code{NaN} and @code{NA} values to be
## included in the moving window, and any window slice containing @code{NaN} or
## @code{NA} values will return @code{NaN} for that element.  The value
## @qcode{"omitnan"} causes @qcode{"movvar"} to ignore any @code{NaN}
## or @code{NA} values resulting in fewer elements being used to calculate the
## variance for that window slice.  If @qcode{"omitnan"} is specified and a
## window slice contains all @code{NaN} or @code{NA} values, @qcode{"movvar"}
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
## @seealso{var, movfun, movslice, movmad, movmax, movmean, movmedian, movmin,
## movprod, movstd, movsum}
## @end deftypefn

function y = movvar (x, wlen, varargin)

  if (nargin < 2)
    print_usage ();
  endif

  ## Process "opt" normalization argument
  if (nargin > 2 && isnumeric (varargin{1}) && ! isempty (varargin{1}))
    if (isempty (varargin{1}))
      fcn = @var;
    elseif (! isscalar (varargin{1}))
      error ("movvar: OPT must be 0 or 1");
    elseif (! varargin{1})
      fcn = @var;
    elseif (varargin{1} == 1)
      fcn = @(x) var (x, 1);
    else
      error ("movvar: OPT must be 0 or 1");
    endif
    varargin(1) = [];
  else
    fcn = @var;
  endif

  y = movfun (fcn, x, wlen, __parse_movargs__ ("movvar", varargin{:}){:});

endfunction


%!assert (movvar (1:5, 3), [0.5, 1, 1, 1, 0.5], eps)
%!assert (movvar (1:5, [1, 1]), [0.5, 1, 1, 1, 0.5], eps)
%!assert (movvar (1:5, 3, 0), [0.5, 1, 1, 1, 0.5], eps)
%!assert <*66021> (movvar (1:5, 3, []), [0.5, 1, 1, 1, 0.5], eps)
%!assert (movvar (1:5, 3, 1), [0.25, 2/3, 2/3, 2/3, 0.25], eps)
%!assert <*65928> (movvar (1:5, 3, 0, 1), zeros (1, 5))
%!assert (movvar (1:5, 3, 0, 2), [0.5, 1, 1, 1, 0.5], eps)
%!assert (movvar (1:5, 3, 1, 2), [0.25, 2/3, 2/3, 2/3, 0.25], eps)
%!assert <*66021> (movvar (1:5, 3, [], 2), [0.5, 1, 1, 1, 0.5], eps)
%!assert <*65928> (movvar (1:5, 3, 0, 3), zeros (1, 5))

%!assert (movvar (magic (4), 3, 0), [60.5, 40.5, 24.5, 12.5; 31, 61/3, 37/3, 7;...
%!                                   7, 37/3, 61/3, 31; 12.5, 24.5, 40.5, 60.5], 20*eps)
%!assert (movvar (magic (4), 3, 0, 1), [60.5, 40.5, 24.5, 12.5; 31, 61/3, 37/3, 7;...
%!                                   7, 37/3, 61/3, 31; 12.5, 24.5, 40.5, 60.5], 20*eps)
%!assert (movvar (magic (4), 3, 0, 2), [98, 61, 37, 50; 18, 31/3, 7/3, 2; ...
%!                                   2, 7/3, 31/3, 18; 50, 37, 61, 98], 20*eps)
%!assert <*65928> (movvar (magic (4), 3, 0, 3), zeros (4, 4))

%!assert <*55241> (movvar ((1:10).', 3), [0.5; ones(8,1); 0.5], eps)

%!test <*56765>
%! x = 1:10;
%! y = movvar (x, 4);
%! y0 = movvar (x, 4, 0);
%! assert (y, y0);
%! y1 = movvar (x, 4, 1);
%! assert (y1(1:3), [1/4, 2/3, 5/4]);

%!assert <66156> (movvar ([1:4, NaN(1,3), 8:10], 3), movvar ([1:4, NaN(1,3), 8:10], 3, "includenan"))
%!assert <66156> (movvar ([1:4, NaN(1,3), 8:10], 3, "includenan"), [0.5, 1, 1, NaN(1,5), 1, 0.5], eps)
%!assert <66156> (movvar ([1:4, NaN(1,3), 8:10], 3, "omitnan"), [0.5, 1, 1, 0.5, 0, NaN,0, 0.5, 1, 0.5], eps)


## Test input validation
%!error <Invalid call> movvar ()
%!error <Invalid call> movvar (1)
%!error <OPT must be 0 or 1> movvar (1:10, 3, -1)
%!error <OPT must be 0 or 1> movvar (1:10, 3, 1.4)
%!error <OPT must be 0 or 1> movvar (1:10, 3, [1, 1])
%!error <OPT must be 0 or 1> movvar (1:10, 3, [0, 0])
%!error <OPT must be 0 or 1> movvar (1:10, 3, [0, 1])
%!error <OPT must be 0 or 1> movvar (1:10, 3, [1, 0])
%!error <OPT must be 0 or 1> movvar (1:10, 3, [0, 3])
%!error <OPT must be 0 or 1> movvar (1:10, 3, [1, 3])
%!error <OPT must be 0 or 1> movvar (1:10, 3, [3, 0])
%!error <OPT must be 0 or 1> movvar (1:10, 3, [3, 1])

