## Copyright (C) 1994-2017 John W. Eaton
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {} {} hist (@var{y})
## @deftypefnx {} {} hist (@var{y}, @var{x})
## @deftypefnx {} {} hist (@var{y}, @var{nbins})
## @deftypefnx {} {} hist (@var{y}, @var{x}, @var{norm})
## @deftypefnx {} {} hist (@dots{}, @var{prop}, @var{val}, @dots{})
## @deftypefnx {} {} hist (@var{hax}, @dots{})
## @deftypefnx {} {[@var{nn}, @var{xx}] =} hist (@dots{})
## Produce histogram counts or plots.
##
## With one vector input argument, @var{y}, plot a histogram of the values
## with 10 bins.  The range of the histogram bins is determined by the
## range of the data.  With one matrix input argument, @var{y}, plot a
## histogram where each bin contains a bar per input column.
##
## Given a second vector argument, @var{x}, use that as the centers of
## the bins, with the width of the bins determined from the adjacent
## values in the vector.
##
## If scalar, the second argument, @var{nbins}, defines the number of bins.
##
## If a third argument is provided, the histogram is normalized such that
## the sum of the bars is equal to @var{norm}.
##
## Extreme values are lumped into the first and last bins.
##
## The histogram's appearance may be modified by specifying property/value
## pairs.  For example the face and edge color may be modified.
##
## @example
## @group
## hist (randn (1, 100), 25, "facecolor", "r", "edgecolor", "b");
## @end group
## @end example
##
## @noindent
## The histogram's colors also depend upon the current colormap.
##
## @example
## @group
## hist (rand (10, 3));
## colormap (summer ());
## @end group
## @end example
##
## If the first argument @var{hax} is an axes handle, then plot into this axis,
## rather than the current axes returned by @code{gca}.
##
## With two output arguments, produce the values @var{nn} (numbers of elements)
## and @var{xx} (bin centers) such that @code{bar (@var{xx}, @var{nn})} will
## plot the histogram.
##
## @seealso{histc, bar, pie, rose}
## @end deftypefn

## Author: jwe

function [nn, xx] = hist (varargin)

  [hax, varargin, nargin] = __plt_get_axis_arg__ ("hist", varargin{:});

  if (nargin < 1)
    print_usage ();
  endif

  y = varargin{1};
  varargin = varargin(2:end);

  arg_is_vector = isvector (y);

  if (arg_is_vector)
    y = y(:);
  endif

  if (! isreal (y))
    error ("hist: Y must be real valued");
  endif

  max_val = max (y(:));
  min_val = min (y(:));
  ## Do not convert if input is of class single (or if already is double).
  if (! isfloat (y))
    max_val = double (max_val);
    min_val = double (min_val);
  endif

  iarg = 1;
  if (nargin == 1 || ischar (varargin{iarg}))
    n = 10;
    x = [0.5:n]'/n;
    x = (max_val - min_val) * x + min_val * ones (size (x));
  else
    ## nargin is either 2 or 3
    x = varargin{iarg++};
    ## Do not convert if input is of class single (or if already is double).
    if (! isfloat (x))
      x = double (x);
    endif

    if (isscalar (x))
      n = x;
      if (n <= 0)
        error ("hist: number of bins NBINS must be positive");
      endif
      x = [0.5:n]'/n;
      x = (max_val - min_val) * x  + min_val * ones (size (x));
    elseif (isreal (x))
      if (isvector (x))
        x = x(:);
      endif
      xsort = sort (x);
      if (any (xsort != x))
        warning ("hist: bin values not sorted on input");
        x = xsort;
      endif
    else
      error ("hist: second argument must be a scalar or a vector");
    endif
  endif

  cutoff = (x(1:end-1,:) + x(2:end,:)) / 2;
  if (isinteger (y))
    cutoff = floor (cutoff);
  endif

  n = rows (x);
  y_nc = columns (y);
  if (n < 30 && columns (x) == 1)
    ## The following algorithm works fastest for n less than about 30.
    chist = zeros (n+1, y_nc);
    for i = 1:n-1
      chist(i+1,:) = sum (y <= cutoff(i));
    endfor
    chist(n+1,:) = sum (! isnan (y));
  else
    ## The following algorithm works fastest for n greater than about 30.
    ## Put cutoff elements between boundaries, integrate over all
    ## elements, keep totals at boundaries.
    m = (nthargout (2, @sort, [y; repmat(cutoff, 1, y_nc)]) <= rows (y));
    chist = cumsum (m);
    chist = [(zeros (1, y_nc));
             (reshape (chist(! m), rows (cutoff), y_nc));
             (chist(end,:) - sum (isnan (y)))];
  endif

  freq = diff (chist);

  if (nargin > 2 && ! ischar (varargin{iarg}))
    ## Normalize the histogram.
    norm = varargin{iarg++};
    freq = bsxfun (@times, freq, norm ./ sum (! isnan (y)));
  endif

  if (nargout > 0)
    if (arg_is_vector)
      ## Matlab compatibility requires a row vector return
      nn = freq';
      xx = x';
    else
      nn = freq;
      xx = x;
    endif
  else
    if (isempty (hax))
      hax = gca ();
    endif
    bar (hax, x, freq, "hist", varargin{iarg:end});
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
%! [nn,xx] = hist ([1 1 1 NaN NaN NaN 2 2 3],[1 2 3]);
%! assert (xx, [1,2,3]);
%! assert (nn, [3,2,1]);
%!test
%! [nn,xx] = hist ([1 1 1 NaN NaN NaN 2 2 3],[1 2 3], 6);
%! assert (xx, [1,2,3]);
%! assert (nn, [3,2,1]);
%!test
%! [nn,xx] = hist ([[1:4]', [1:4]'], 3);
%! assert (xx, [1.5;2.5;3.5]);
%! assert (nn, [[2,1,1]',[2,1,1]']);
%!test
%! for n = [10, 30, 100, 1000]
%!   assert (sum (hist ([1:n], n)), n);
%!   assert (sum (hist ([1:n], [2:n-1])), n);
%!   assert (sum (hist ([1:n], [1:n])), n);
%!   assert (sum (hist ([1:n], 29)), n);
%!   assert (sum (hist ([1:n], 30)), n);
%! endfor
%!assert (hist (1,1), 1)
%!assert (size (hist (randn (750,240), 200)), [200,240])
%!assert <*42394> (isempty (hist (rand (10,2), 0:5, 1)), false)
%!assert <*42394> (isempty (hist (rand (10,2), 0:5, [1 1])), false)

%!test <*47707>
%! y = [1  9  2  2  9  3  8  9  1  7  1  1  3  2  4  4  8  2  1  9  4  1 ...
%!      2  3  1  1  6  5  5  3  9  9  1  1  8  7  7  2  4  1];
%! [n, x] = hist (y, 10);
%! [nn, xx] = hist (uint8 (y), 10);
%! assert (nn, n)
%! assert (xx, x)
%!
%! ## test again with N > 30 because there's a special case for it
%! [n, x] = hist (y, 35);
%! [nn, xx] = hist (uint8 (y), 35);
%! assert (nn, n)
%! assert (xx, x)

## Test logical input
%!test
%! y = [0  1  0  0  1  0  1  1  0  1  0  0  0  0  0  0  1  0];
%! [n, x] = hist (y, 10);
%! [nn, xx] = hist (logical (y), 10);
%! assert (nn, n)
%! assert (xx, x)
%!
%! ## test again with N > 30 because there's a special case for it
%! [n, x] = hist (y, 35);
%! [nn, xx] = hist (logical (y), 35);
%! assert (nn, n)
%! assert (xx, x)

## Second output argument must be of class single if data is class single.
%!assert (class (nthargout (2, @hist, rand (10, 1, "single"))), "single")

## Handle second argument correctly, even when it's class integer
%!test
%! y = [2.4 2.5 2.6 5.4 5.5 5.6];
%! n = [2  3  1];
%! x = [1  4  7];
%! [nn, xx] = hist (y, uint8 ([1 4 7]));
%! assert (nn, n)
%! assert (xx, x)

%!test
%! y = [2.4 2.5 2.6 5.4 5.5 5.6];
%! s = (5.6 - 2.4) / 6;
%! x = [2.4+s 4.0 5.6-s];
%! n = [3 0 3];
%!
%! [nn, xx] = hist (y, 3);
%! assert (nn, n)
%! assert (xx, x)
%!
%! [nn, xx] = hist (y, uint8 (3));
%! assert (nn, n)
%! assert (xx, x)
%!
%! [nn, xx] = hist (y, single (3));
%! assert (nn, n)
%! assert (xx, single (x))
