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
## @deftypefn  {} {@var{y} =} movstd (@var{x}, @var{wlen})
## @deftypefnx {} {@var{y} =} movstd (@var{x}, [@var{nb}, @var{na}])
## @deftypefnx {} {@var{y} =} movstd (@dots{}, @var{opt})
## @deftypefnx {} {@var{y} =} movstd (@dots{}, @var{opt}, @var{dim})
## @deftypefnx {} {@var{y} =} movstd (@dots{}, "@var{nancond}")
## @deftypefnx {} {@var{y} =} movstd (@dots{}, @var{property}, @var{value})
## Calculate the moving standard deviation over a sliding window of length
## @var{wlen} on data @var{x}.
##
## The moving window length input @var{wlen} can either be a numeric scalar
## or a 2-element numeric array @w{@qcode{[@var{nb}, @var{na}]}}. The elements
## included in the moving window depend on the size and value of @var{wlen}
## as well as whether the @qcode{"SamplePoints"} option has been specified.
## For full details of element inclusion,
## @pxref{XREFmovslice,,@code{movslice}}.
##
## The optional argument @var{opt} determines the type of normalization to use.
## Valid values are
##
## @table @asis
## @item 0:
##   normalize with @math{N-1}, provides the square root of the best unbiased
## estimator of the variance [default]
##
## @item 1:
##   normalize with @math{N}, this provides the square root of the second
## moment around the mean
## @end table
##
## If the optional argument @var{dim} is given, operate along this dimension.
## The normalization argument @var{opt} must be given before the dimension.
## To use the default value for @var{opt} you may pass an empty input
## argument [].
##
## The optional string argument @qcode{"@var{nancond}"} controls how @code{NaN}
## and @code{NA} values affect the output of @qcode{"movstd"}. The value
## @qcode{"includenan"} (default) causes @code{NaN} and @code{NA} values to be
## included in the moving window, and any window slice containing @code{NaN} or
## @code{NA} values will return @code{NaN} for that element.  The value
## @qcode{"omitnan"} causes @qcode{"movstd"} to ignore any @code{NaN}
## or @code{NA} values resulting in fewer elements being used to calculate the
## standard deviation for that window slice.  If @qcode{"omitnan"} is specified
## and a window slice contains all @code{NaN} or @code{NA} values,
## @qcode{"movstd"} returns @code{NaN} for that element.  The values
## @qcode{"includemissing"} and @qcode{"omitmissing"} may be used synonymously
## with @qcode{"includenan"} and @qcode{"omitnan"}, respectively.
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
## @seealso{std, movfun, movslice, movmad, movmax, movmean, movmedian, movmin,
## movprod, movsum, movvar}
## @end deftypefn

function y = movstd (x, wlen, varargin)

  if (nargin < 2)
    print_usage ();
  endif

  ## Process "opt" normalization argument
  if (nargin > 2 && isnumeric (varargin{1}) && ! isempty (varargin{1}))
    if (isempty (varargin{1}))
      fcn = @std;
    elseif (! isscalar (varargin{1}))
      error ("movstd: OPT must be 0 or 1");
    elseif (! varargin{1})
      fcn = @std;
    elseif (varargin{1} == 1)
      fcn = @(x) std (x, 1);
    else
      error ("movstd: OPT must be 0 or 1");
    endif
    varargin(1) = [];
  else
    fcn = @std;
  endif

  y = movfun (fcn, x, wlen, __parse_movargs__ ("movstd", varargin{:}){:});

endfunction


%!assert (movstd (1:5, 3), [1/sqrt(2), 1, 1, 1, 1/sqrt(2)], eps)
%!assert (movstd (1:5, [1, 1]), [1/sqrt(2), 1, 1, 1, 1/sqrt(2)], eps)
%!assert (movstd (1:5, 3, 0), [1/sqrt(2), 1, 1, 1, 1/sqrt(2)], eps)
%!assert <*66021> (movstd (1:5, 3, []), [1/sqrt(2), 1, 1, 1, 1/sqrt(2)], eps)
%!assert (movstd (1:5, 3, 1), sqrt ([0.25, 2/3, 2/3, 2/3, 0.25]), eps)
%!assert <*65928> (movstd (1:5, 3, 0, 1), zeros (1, 5))
%!assert (movstd (1:5, 3, 0, 2), [1/sqrt(2), 1, 1, 1, 1/sqrt(2)], eps)
%!assert (movstd (1:5, 3, 1, 2), sqrt ([0.25, 2/3, 2/3, 2/3, 0.25]), eps)
%!assert <*66021> (movstd (1:5, 3, [], 2), [1/sqrt(2), 1, 1, 1, 1/sqrt(2)], eps)
%!assert <*65928> (movstd (1:5, 3, 0, 3), zeros (1, 5))

%!assert (movstd (magic (4), 3, 0), sqrt ([60.5, 40.5, 24.5, 12.5; 31, 61/3, 37/3, 7;...
%!                                   7, 37/3, 61/3, 31; 12.5, 24.5, 40.5, 60.5]), 2*eps)
%!assert (movstd (magic (4), 3, 0, 1), sqrt ([60.5, 40.5, 24.5, 12.5; 31, 61/3, 37/3, 7;...
%!                                   7, 37/3, 61/3, 31; 12.5, 24.5, 40.5, 60.5]), 2*eps)
%!assert (movstd (magic (4), 3, 0, 2), sqrt ([98, 61, 37, 50; 18, 31/3, 7/3, 2; ...
%!                                   2, 7/3, 31/3, 18; 50, 37, 61, 98]), 2*eps)
%!assert <*65928> (movstd (magic (4), 3, 0, 3), zeros (4, 4))

%!assert <*55241> (movstd ((1:10).', 3), [1/sqrt(2); ones(8,1); 1/sqrt(2)], eps)

%!assert <66156> (movstd ([1:4, NaN(1,3), 8:10], 3), movstd ([1:4, NaN(1,3), 8:10], 3, "includenan"))
%!assert <66156> (movstd ([1:4, NaN(1,3), 8:10], 3, "includenan"), [sqrt(2)/2, 1, 1, NaN(1,5), 1, sqrt(2)/2], eps)
%!assert <66156> (movstd ([1:4, NaN(1,3), 8:10], 3, "omitnan"), [sqrt(2)/2, 1, 1, sqrt(2)/2, 0, NaN,0, sqrt(2)/2, 1, sqrt(2)/2], eps)

%!test <*56765>
%! x = 1:10;
%! y = movstd (x, 4);
%! y0 = movstd (x, 4, 0);
%! assert (y, y0);
%! y1 = movstd (x, 4, 1);
%! assert (y1(1:3), sqrt ([1/4, 2/3, 5/4]));

## Test input validation
%!error <Invalid call> movstd ()
%!error <Invalid call> movstd (1)
%!error <OPT must be 0 or 1> movstd (1:10, 3, -1)
%!error <OPT must be 0 or 1> movstd (1:10, 3, 1.4)
%!error <OPT must be 0 or 1> movstd (1:10, 3, [1, 1])
%!error <OPT must be 0 or 1> movstd (1:10, 3, [0, 0])
%!error <OPT must be 0 or 1> movstd (1:10, 3, [0, 1])
%!error <OPT must be 0 or 1> movstd (1:10, 3, [1, 0])
%!error <OPT must be 0 or 1> movstd (1:10, 3, [0, 3])
%!error <OPT must be 0 or 1> movstd (1:10, 3, [1, 3])
%!error <OPT must be 0 or 1> movstd (1:10, 3, [3, 0])
%!error <OPT must be 0 or 1> movstd (1:10, 3, [3, 1])

