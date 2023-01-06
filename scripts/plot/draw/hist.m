########################################################################
##
## Copyright (C) 1994-2023 The Octave Project Developers
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
## @deftypefn  {} {} hist (@var{y})
## @deftypefnx {} {} hist (@var{y}, @var{nbins})
## @deftypefnx {} {} hist (@var{y}, @var{x})
## @deftypefnx {} {} hist (@var{y}, @var{x}, @var{norm})
## @deftypefnx {} {} hist (@dots{}, @var{prop}, @var{val}, @dots{})
## @deftypefnx {} {} hist (@var{hax}, @dots{})
## @deftypefnx {} {[@var{nn}, @var{xx}] =} hist (@dots{})
## Produce histogram counts or plots.
##
## With one vector input argument, @var{y}, plot a histogram of the values
## with 10 bins.  The range of the histogram bins is determined by the
## range of the data (difference between maximum and minimum value in @var{y}).
## Extreme values are lumped into the first and last bins.  If @var{y} is a
## matrix then plot a histogram where each bin contains one bar per input
## column of @var{y}.
##
## If the optional second argument is a scalar, @var{nbins}, it defines the
## number of bins.
##
## If the optional second argument is a vector, @var{x}, it defines the centers
## of the bins.  The width of the bins is determined from the adjacent values
## in the vector.  The total number of bins is @code{numel (@var{x})}.
##
## If a third argument @var{norm} is provided, the histogram is normalized.
## In case @var{norm} is a positive scalar, the resulting bars are normalized
## to @var{norm}.  If @var{norm} is a vector of positive scalars of length
## @code{columns (@var{y})}, then the resulting bar of @code{@var{y}(:,i)} is
## normalized to @code{@var{norm}(i)}.
##
## @example
## @group
## [nn, xx] = hist (rand (10, 3), 5, [1 2 3]);
## sum (nn, 1)
## @result{} ans =
##       1   2   3
## @end group
## @end example
##
## The histogram's appearance may be modified by specifying property/value
## pairs to the underlying patch object.  For example, the face and edge color
## may be modified:
##
## @example
## @group
## hist (randn (1, 100), 25, "facecolor", "r", "edgecolor", "b");
## @end group
## @end example
##
## @noindent
## The full list of patch properties is documented at @ref{Patch Properties}.
## property.  If not specified, the default colors for the histogram are taken
## from the @qcode{"Colormap"} property of the axes or figure.
##
## If the first argument @var{hax} is an axes handle, then plot into this axes,
## rather than the current axes returned by @code{gca}.
##
## If an output is requested then no plot is made.  Instead, return the values
## @var{nn} (numbers of elements) and @var{xx} (bin centers) such that
## @code{bar (@var{xx}, @var{nn})} will plot the histogram.
##
## @seealso{histc, bar, pie, rose}
## @end deftypefn

function [nn, xx] = hist (varargin)

  [hax, varargin, nargin] = __plt_get_axis_arg__ ("hist", varargin{:});

  if (nargin < 1)
    print_usage ();
  endif

  ## Process Y argument
  iarg = 1;
  y = varargin{iarg++};

  if (! isreal (y))
    error ("hist: Y must be real-valued");
  endif

  arg_is_vector = isvector (y);
  if (arg_is_vector)
    y = y(:);
  endif

  yfinite = y(isfinite (y))(:);
  max_val = max (yfinite);
  min_val = min (yfinite);
  ## Do not convert if input is of class single (or if already is double).
  if (! isfloat (y))
    max_val = double (max_val);
    min_val = double (min_val);
  endif

  ## Equidistant entries allow much more efficient algorithms.
  equal_bin_spacing = true;

  ## Process possible second argument
  if (nargin == 1 || ischar (varargin{iarg}))
    n = 10;
    ## Use integer range values and perform division last to preserve
    ## accuracy.
    if (min_val != max_val)
      x = 1:2:2*n;
      x = ((max_val - min_val) * x + 2*n*min_val) / (2*n);
    else
      x = (-floor ((n-1)/2):ceil ((n-1)/2)) + min_val;
    endif
    x = x.';  # Convert to matrix
  else
    ## Parse bin specification argument
    x = varargin{iarg++};
    if (! isreal (x))
      error ("hist: bin specification must be a numeric scalar or vector");
    endif

    ## Convert integer types or a single specification of N bins to double
    if (! isfloat (x) || isscalar (x))
      x = double (x);
    endif

    if (isscalar (x))
      n = fix (x);
      if (n <= 0)
        error ("hist: number of bins NBINS must be positive");
      endif
      ## Use integer range values and perform division last to preserve
      ## accuracy.
      if (min_val != max_val)
        x = 1:2:2*n;
        x = ((max_val - min_val) * x + 2*n*min_val) / (2*n);
      else
        x = (-floor ((n-1)/2):ceil ((n-1)/2)) + min_val;
      endif
      x = x.';  # Convert to matrix
    elseif (isvector (x))
      equal_bin_spacing = strcmp (typeinfo (x), "range");
      if (! equal_bin_spacing)
        diffs = diff (x);
        if (all (diffs == diffs(1)))
          equal_bin_spacing = true;
        endif
      endif
      x = x(:);
      if (! issorted (x))
        warning ("hist: bin values X not sorted on input");
        x = sort (x);
      endif
    else
      error ("hist: bin specification must be a scalar or vector");
    endif
  endif

  ## Check for third argument (normalization)
  norm = false;
  if (nargin > 2 && ! ischar (varargin{iarg}))
    norm = varargin{iarg++};
    if (! isnumeric (norm) || ! all (norm > 0))
      error ("hist: NORM must be a numeric constant > 0");
    endif
    if (! isvector (norm) ...
        || ! (length (norm) == 1 || length (norm) == columns (y)))
      error ("hist: NORM must be scalar or vector of length 'columns (Y)'");
    endif
    norm = norm (:).';  # Ensure vector orientation.
  endif

  ## Perform histogram calculation
  cutoff = (x(1:end-1,:) + x(2:end,:)) / 2;

  n = rows (x);
  y_nc = columns (y);

  if (n < 11 * (1 + (! equal_bin_spacing)))
    ## The following algorithm works fastest for small n.
    nanidx = isnan (y);
    chist = zeros (n+1, y_nc);
    for i = 1:n-1
      chist(i+1,:) = sum (y <= cutoff(i));
    endfor
    chist(n+1,:) = sum (! nanidx);

    freq = diff (chist);
  else
    ## Lookup is more efficient if y is sorted, but sorting costs.
    if (! equal_bin_spacing && n > sqrt (rows (y) * 1e4))
      y = sort (y);
    endif

    nanidx = isnan (y);
    y(nanidx) = 0;
    freq = zeros (n, y_nc);
    if (equal_bin_spacing)
      if (n < 3)
        d = 1;
      else
        d = (x(end) - x(1)) / (length (x) - 1);
      endif
      cutlen = length (cutoff);
      for j = 1:y_nc
        freq(:,j) = accumarray (1 + max (0, min (cutlen, ceil ((double (y(:,j))
                                                         - cutoff(1)) / d))),
                                double (! nanidx(:,j)),
                                [n, 1]);
      endfor
    else
      for j = 1:y_nc
        i = lookup (cutoff, y(:,j));
        i = 1 + i - (cutoff(max (i, 1)) == y(:,j));
        freq(:,j) = accumarray (i, double (! nanidx(:,j)), [n, 1]);
      endfor
    endif
  endif

  if (norm)
    ## Normalize the histogram
    freq .*= norm ./ sum (! nanidx);
  endif

  if (nargout == 0)
    if (isempty (hax))
      hax = gca ();
    endif
    bar (hax, x, freq, "hist", varargin{iarg:end});
  else
    if (arg_is_vector)
      ## Matlab compatibility requires a row vector return
      nn = freq.';
      xx = x.';
    else
      nn = freq;
      xx = x;
    endif
  endif

endfunction


%!test
%! [nn,xx] = hist ([1:4], 3);
%! assert (xx, [1.5,2.5,3.5]);
%! assert (nn, [2,1,1]);
%!test
%! [nn,xx] = hist ([1:4]', 3);
%! assert (xx, [1.5,2.5,3.5]);
%! assert (nn, [2,1,1]);
%!test
%! [nn,xx] = hist ([1 1 1 NaN NaN NaN 2 2 3], [1, 2, 3]);
%! assert (xx, [1,2,3]);
%! assert (nn, [3,2,1]);
%!test
%! [nn,xx] = hist ([1 1 1 NaN NaN NaN 2 2 3], [1, 2, 3], 6);
%! assert (xx, [1,2,3]);
%! assert (nn, [3,2,1]);
%!test  # Multiple columns
%! [nn,xx] = hist ([[1:4]', [1:4]'], 3);
%! assert (xx, [1.5;2.5;3.5]);
%! assert (nn, [[2,1,1]', [2,1,1]']);
%!test
%! for n = [10, 30, 100, 1000]
%!   assert (sum (hist ([1:n], n)), n);
%!   assert (sum (hist ([1:n], [2:n-1])), n);
%!   assert (sum (hist ([1:n], [1:n])), n);
%!   assert (sum (hist ([1:n], 29)), n);
%!   assert (sum (hist ([1:n], 30)), n);
%! endfor
%!assert (hist (1,1), 1)
%!test <*54326> # All values identical
%! [nn,xx] = hist (ones (1,5), 3);
%! assert (nn, [0,5,0]);
%! assert (xx, [0,1,2]);
%!assert (size (hist (randn (750,240), 200)), [200, 240])

## Test normalization
%!assert <*42394> (isempty (hist (rand (10,2), 0:5, 1)), false)
%!assert <*42394> (isempty (hist (rand (10,2), 0:5, [1 1])), false)
%!test <*60783>
%! [nn, xx] = hist (reshape (1:30, 10, 3), 5, 1);
%! assert (sum (nn, 1), [1 1 1]);
%! [nn, xx] = hist (reshape (1:30, 10, 3), 5, [1 2 3]);
%! assert (sum (nn, 1), [1 2 3]);
%! [nn, xx] = hist (reshape (1:30, 10, 3), 5, [1 2 3]');
%! assert (sum (nn, 1), [1 2 3]);

%!test <*47707>
%! y = [1  9  2  2  9  3  8  9  1  7  1  1  3  2  4  4  8  2  1  9  4  1 ...
%!      2  3  1  1  6  5  5  3  9  9  1  1  8  7  7  2  4  1];
%! [n, x] = hist (y, 10);
%! [nn, xx] = hist (uint8 (y), 10);
%! assert (nn, n);
%! assert (xx, x);
%!
%! ## test again with N > 26 because there's a special case for it
%! [n, x] = hist (y, 30);
%! [nn, xx] = hist (uint8 (y), 30);
%! assert (nn, n);
%! assert (xx, x);

## Test logical input
%!test
%! y = [0  1  0  0  1  0  1  1  0  1  0  0  0  0  0  0  1  0];
%! [n, x] = hist (y, 10);
%! [nn, xx] = hist (logical (y), 10);
%! assert (nn, n);
%! assert (xx, x);
%!
%! ## test again with N > 26 because there's a special case for it
%! [n, x] = hist (y, 30);
%! [nn, xx] = hist (logical (y), 30);
%! assert (nn, n);
%! assert (xx, x);

## Second output argument must be of class single if data is class single.
%!assert (class (nthargout (2, @hist, rand (10, 1, "single"))), "single")

## Handle second argument correctly, even when it's class integer
%!test
%! y = [2.4, 2.5, 2.6, 5.4, 5.5, 5.6];
%! n = [2, 3, 1];
%! x = [1, 4, 7];
%! [nn, xx] = hist (y, uint8 ([1 4 7]));
%! assert (nn, n);
%! assert (xx, x);

## Test bin centers
%!test
%! y = [2.4, 2.5, 2.6, 5.4, 5.5, 5.6];
%! s = (5.6 - 2.4) / 6;
%! x = [2.4+s, 4.0, 5.6-s];
%! n = [3, 0, 3];
%!
%! [nn, xx] = hist (y, 3);
%! assert (nn, n);
%! assert (xx, x, 2*eps);
%!
%! [nn, xx] = hist (y, uint8 (3));
%! assert (nn, n);
%! assert (xx, x, 2*eps);
%!
%! [nn, xx] = hist (y, single (3));
%! assert (nn, n);
%! assert (xx, single (x), 2*eps ("single"));

%!test <*53199>
%! a = [  1,  2,  3,  4, 0;
%!        5,  4,  6,  7, 8;
%!        9, 12, 11, 10, 0;
%!       13, 16, 15, 14, 0;
%!       17, 20, 19, 18, 0;
%!       21, 22, 23,  2, 0;
%!       24, 27, 26, 25, 0;
%!       28, 31, 30, 29, 0;
%!       32, 35, 34, 33, 0;
%!       36, 39, 38, 37, 0;
%!       40, 43, 42, 41, 0;
%!       44, 47, 46, 45, 0;
%!       48, 51, 50, 49, 0;
%!       52, 55, 54, 53, 0];
%! n = max (a(:));
%! [cnt1, ctr1] = hist(a(:), 1:n);
%! [cnt2, ctr2] = hist(a(:), n);
%! assert (cnt1, cnt2);
%! assert (ctr1, 1:n);
%! assert (ctr2, 0.5:n);

## Test Infinite values and calculation of bins
%!test
%! y = [-Inf, NaN, 10, Inf, 0];
%! [nn, xx] = hist (y);
%! assert (nn, [2 0 0 0 0 0 0 0 0 2]);
%! assert (xx, 0.5:10);

## Test return class of second output
%!test <*56465>
%! [nn, xx] = hist (double (1:10), single (7));
%! assert (isa (xx, "double"));
%! [nn, xx] = hist (single (1:10), double (7));
%! assert (isa (xx, "single"));
%! [nn, xx] = hist (single (1:10), double ([1, 5, 10]));
%! assert (isa (xx, "double"));
%! [nn, xx] = hist (double (1:10), single ([1, 5, 10]));
%! assert (isa (xx, "single"));

## Test input validation
%!error <Invalid call> hist ()
%!error <Y must be real-valued> hist (2+i)
%!error <bin specification must be a numeric> hist (1, {0,1,2})
%!error <number of bins NBINS must be positive> hist (1, 0)
%!test
%! hf = figure ("visible", "off");
%! hax = gca ();
%! unwind_protect
%!   fail ("hist (hax, 1, [2 1 0])", "warning", "bin values X not sorted");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
%!error <bin specification must be a scalar or vector> hist (1, ones (2,2))
%!error <NORM must be a numeric constant> hist (1,1, {1})
%!error <NORM must be a numeric constant . 0> hist (1,1, -1)
%!error <NORM must be scalar or vector> hist (1,1, ones (4))
