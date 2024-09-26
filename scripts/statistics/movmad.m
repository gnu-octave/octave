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
## @deftypefnx {} {@var{y} =} movmad (@dots{}, @var{mode})
## @deftypefnx {} {@var{y} =} movmad (@dots{}, @var{nancond})
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
## The optional argument @var{nancond} is a string that controls how
## @code{NaN} and @code{NA} values affect the output of @qcode{"movmad"}. The
## value @qcode{"includenan"} causes @code{NaN} and @code{NA} values to be
## included in the moving window, and any window slice containing @code{NaN}
## or @code{NA} values will return @code{NaN} for that element.  The value
## @qcode{"omitnan"} (default) causes @qcode{"movmad"} to ignore any
## @code{NaN} or @code{NA} values resulting in fewer elements being used to
## calculate the mad for that window slice.  If @qcode{"omitnan"} is specified
## and a window slice contains all @code{NaN} or @code{NA} values,
## @qcode{"movmad"} returns @code{NaN} for that element.  The values
## @qcode{"includemissing"} and @qcode{"omitmissing"} may be used synonymously
## with @qcode{"includenan"} and @qcode{"omitnan"}, respectively.
##
## The optional argument @var{mode} is a string that can take the value
## @qcode{"median"} (default) or @qcode{"mean"} and will control whether
## @qcode{"movmad"} performs median or mean absolute deviation
## calculations on the data.
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
## Compatibility Note: Prior to Octave 10 this function only calculated mean
## absolute deviation.  To achieve code compatibility, the default has been
## changed to median absolute deviation.  The @var{mode} option is now
## provided to enable access to both @qcode{"mad"} calculation methods.  This
## option should not be expected to be functional outside of Octave code.
##
## @seealso{mad, movfun, movslice, movmax, movmean, movmedian, movmin,
## movprod, movstd, movsum, movvar}
## @end deftypefn

function y = movmad (x, wlen, varargin)

  if (nargin < 2)
    print_usage ();
  endif

  ## Check for user specified mean or median absolute deviation.
  ## If multiple, last "mean" or "median" input takes precedence.
  ## Use to send Opt = 1 via anonymous function for median case, use default
  ## opt = 0 for mean case and avoid overhead of anonymous fn call.
  ## strip all "mean" / "median" inputs before sending to movfun.

  mean_check = strcmpi (varargin, "mean");
  med_check = strcmpi (varargin, "median");
  mode_check = mean_check | med_check;
  mode_loc = find (mode_check, 1, "last");

  if (any (mode_check))

    switch varargin{mode_loc}
      case "mean"
        varargin(mode_check) = [];
        y = movfun (@mad , x, wlen,
                     __parse_movargs__ ("movmad", varargin{:}){:});

      case "median"
        varargin(mode_check) = [];
        y = movfun (@(x) mad (x, 1), x, wlen,
                     __parse_movargs__ ("movmad", varargin{:}){:});
    endswitch

  else
    ## Default to median if unspecified.
    y = movfun (@(x) mad (x, 1), x, wlen,
                   __parse_movargs__ ("movmad", varargin{:}){:});
  endif

endfunction

## mean absolute deviation tests
%!assert (movmad (1:5, 3, "mean"), [1/2, 2/3, 2/3, 2/3, 1/2], eps)
%!assert (movmad (1:5, [1, 1], "mean"), [1/2, 2/3, 2/3, 2/3, 1/2], eps)
%!assert (movmad (1:5, 3, 2, "mean"), [1/2, 2/3, 2/3, 2/3, 1/2], eps)
%!assert <*65928> (movmad (1:5, 3, 1, "mean"), zeros (1, 5))
%!assert <*65928> (movmad (1:5, 3, 3, "mean"), zeros (1, 5))

%!assert (movmad (magic (4), 3, "mean"), ...
%!       [5.5, 4.5, 3.5, 2.5; 4, 28/9, 22/9, 2; ...
%!        2, 22/9, 28/9, 4; 2.5, 3.5, 4.5, 5.5], 3*eps)
%!assert (movmad (magic (4), 3, 1, "mean"), ...
%!       [5.5, 4.5, 3.5, 2.5; 4, 28/9, 22/9, 2; ...
%!        2, 22/9, 28/9, 4; 2.5, 3.5, 4.5, 5.5], 3*eps)
%!assert (movmad (magic (4), 3, 2, "mean"), ...
%!       [7, 6, 14/3, 5; 3, 22/9, 10/9, 1; ...
%!        1, 10/9, 22/9, 3; 5, 14/3, 6, 7], 3*eps)
%!assert <*65928> (movmad (magic (4), 3, 3, "mean"), zeros (4, 4))

%!assert <*55241> (movmad ((1:10).', 3, "mean"), [0.5; repmat(2/3, 8, 1); 0.5], eps)

%!assert <*66156> (movmad ([1:4, NaN(1,3), 8:10], 3, "mean"), movmad ([1:4, NaN(1,3), 8:10], 3, "mean", "includenan"))
%!assert <*66156> (movmad ([1:4, NaN(1,3), 8:10], 3, "includenan", "mean"), [1/2, 2/3, 2/3, NaN(1,5), 2/3, 1/2], eps)
%!assert <*66156> (movmad ([1:4, NaN(1,3), 8:10], 3, "omitnan", "mean"), [1/2, 2/3, 2/3, 1/2, 0, NaN, 0, 1/2, 2/3 1/2], eps)

## median absolute deviation tests
%!assert (movmad (1:5, 3, "median"), [1/2, 1, 1, 1, 1/2], eps)
%!assert (movmad (1:5, 3), [1/2, 1, 1, 1, 1/2], eps)
%!assert (movmad (1:5, [1, 1]), [1/2, 1, 1, 1, 1/2], eps)
%!assert (movmad (1:5, 3, 2), [1/2, 1, 1, 1, 1/2], eps)
%!assert <*65928> (movmad (1:5, 3, 1), zeros (1, 5))
%!assert <*65928> (movmad (1:5, 3, 3), zeros (1, 5))

%!assert (movmad (magic (4), 3), ...
%!       [5.5, 4.5, 3.5, 2.5; 4, 4, 3, 1; ...
%!        1, 3, 4, 4; 2.5, 3.5, 4.5, 5.5], eps)
%!assert (movmad (magic (4), 3, 1), ...
%!       [5.5, 4.5, 3.5, 2.5; 4, 4, 3, 1; ...
%!        1, 3, 4, 4; 2.5, 3.5, 4.5, 5.5], eps)
%!assert (movmad (magic (4), 3, 2), ...
%!       [7, 1, 1, 5; 3, 1, 1, 1; ...
%!        1, 1, 1, 3; 5, 1, 1, 7])
%!assert <*65928> (movmad (magic (4), 3, 3), zeros (4, 4))

%!assert <*55241> (movmad ((1:10).', 3), [0.5; ones(8,1); 0.5], eps)

%!assert <*66156> (movmad ([1:4, NaN(1,3), 8:10], 3), movmad ([1:4, NaN(1,3), 8:10], 3, "includenan"))
%!assert <*66156> (movmad ([1:4, NaN(1,3), 8:10], 3, "includenan"), [1/2, 1, 1, NaN(1,5), 1, 1/2], eps)
%!assert <*66156> (movmad ([1:4, NaN(1,3), 8:10], 3, "omitnan"), [1/2, 1, 1, 1/2, 0, NaN, 0, 1/2, 1 1/2], eps)

%!assert <*66256> (movmad (1:5, 3, "median", "mean"), (movmad (1:5, 3, "mean")))
%!assert <*66256> (movmad (1:5, 3, "median", 1, "mean", "includenan"), movmad (1:5, 3, 1, "mean", "includenan"))

## Test input validation
%!error <Invalid call> movmad ()
%!error <Invalid call> movmad (1)
