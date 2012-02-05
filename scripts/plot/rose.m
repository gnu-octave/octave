## Copyright (C) 2007-2012 David Bateman
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {Function File} {} rose (@var{th}, @var{r})
## @deftypefnx {Function File} {} rose (@var{h}, @dots{})
## @deftypefnx {Function File} {@var{h} =} rose (@dots{})
## @deftypefnx {Function File} {[@var{r}, @var{th}] =} rose (@dots{})
##
## Plot an angular histogram.  With one vector argument @var{th}, plots the
## histogram with 20 angular bins.  If @var{th} is a matrix, then each column
## of @var{th} produces a separate histogram.
##
## If @var{r} is given and is a scalar, then the histogram is produced with
## @var{r} bins.  If @var{r} is a vector, then the center of each bin are
## defined by the values of @var{r}.
##
## The optional return value @var{h} is a vector of graphics handles to the
## line objects representing each histogram.
##
## If two output arguments are requested then, rather than plotting the
## histogram, the polar vectors necessary to plot the histogram are
## returned.
##
## @example
## @group
## [r, t] = rose ([2*randn(1e5,1), pi + 2*randn(1e5,1)]);
## polar (r, t);
## @end group
## @end example
##
## @seealso{polar, compass, hist}
## @end deftypefn

function [thout, rout] = rose (varargin)

  [h, varargin, nargin] = __plt_get_axis_arg__ ((nargout > 1), "rose",
                                                varargin{:});

  if (nargin < 1)
    print_usage ();
  endif

  ## Force theta to [0,2*pi] range
  th = varargin {1};
  th = atan2  (sin (th), cos (th)) + pi;

  if (nargin > 1)
    x = varargin {2};
    if (isscalar (x))
      x = [0.5/x : 1/x : 1] * 2 * pi;
    else
      ## Force theta to [0,2*pi] range
      x = atan2  (sin (x), cos (x)) + pi;
    endif
  else
    x = [1/40 : 1/20 : 1] * 2 * pi;
  endif

  [nn, xx] = hist (th, x);
  xx = xx(:).';
  if (isvector (nn))
    nn = nn (:);
  endif
  x1 = xx(1:end-1) + diff (xx, 1) / 2;
  x1 = [x1 ; x1; x1; x1](:);
  th = [0; 0; x1; 2*pi ; 2*pi];
  r = zeros (4 * size (nn, 1), size (nn, 2));
  r(2:4:end, :) = nn;
  r(3:4:end, :) = nn;

  if (nargout < 2)
    oldh = gca ();
    unwind_protect
      axes (h);
      newplot ();
      hlist = polar (h, th, r);
    unwind_protect_cleanup
      axes (oldh);
    end_unwind_protect

    if (nargout > 0)
      thout = hlist;
    endif
  else
    thout = th;
    rout = r;
  endif

endfunction


%!demo
%! clf
%! rose ([2*randn(1e5, 1), pi + 2*randn(1e5, 1)]);

