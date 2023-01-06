########################################################################
##
## Copyright (C) 2007-2023 The Octave Project Developers
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
## @deftypefn  {} {} rose (@var{th})
## @deftypefnx {} {} rose (@var{th}, @var{nbins})
## @deftypefnx {} {} rose (@var{th}, @var{bins})
## @deftypefnx {} {} rose (@var{hax}, @dots{})
## @deftypefnx {} {@var{h} =} rose (@dots{})
## @deftypefnx {} {[@var{thout} @var{rout}] =} rose (@dots{})
## Plot an angular histogram.
##
## With one vector argument, @var{th}, plot the histogram with 20 angular bins.
## If @var{th} is a matrix then each column of @var{th} produces a separate
## histogram.
##
## If @var{nbins} is given and is a scalar, then the histogram is produced with
## @var{nbin} bins.  If @var{bins} is a vector, then the center of each bin is
## defined by the values in @var{bins} and the number of bins is
## given by the number of elements in @var{bins}.
##
## If the first argument @var{hax} is an axes handle, then plot into this axes,
## rather than the current axes returned by @code{gca}.
##
## The optional return value @var{h} is a vector of graphics handles to the
## line objects representing each histogram.
##
## If two output arguments are requested then no plot is made and
## the polar vectors necessary to plot the histogram are returned instead.
##
## Example
##
## @example
## @group
## [th, r] = rose ([2*randn(1e5,1), pi + 2*randn(1e5,1)]);
## polar (th, r);
## @end group
## @end example
##
## Programming Note: When specifying bin centers with the @var{bins} input,
## the edges for bins 2 to N-1 are spaced so that @code{@var{bins}(i)} is
## centered between the edges.  The final edge is drawn halfway between bin N
## and bin 1.  This guarantees that all input @var{th} will be placed into one
## of the bins, but also means that for some combinations bin 1 and bin N may
## not be centered on the user's given values.
## @seealso{hist, polar}
## @end deftypefn

function [thout, rout] = rose (varargin)

  [hax, varargin, nargin] = __plt_get_axis_arg__ ("rose", varargin{:});

  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif

  th = varargin{1};
  ## Force theta to [0,2*pi] range
  th = atan2 (sin (th), cos (th));
  th(th < 0) += 2*pi;

  custom_bins = false;
  if (nargin == 1)
    bins = [1/40 : 1/20 : 1] * 2*pi;
  else
    bins = varargin{2};
    if (isscalar (bins))
      bins = [0.5/bins : 1/bins : 1] * 2*pi;
    else
      custom_bins = true;
      ## Force angles to [0,2*pi] range
      bins = atan2 (sin (bins), cos (bins));
      bins(bins < 0) += 2*pi;
      bins = unique (bins);
    endif
  endif
  if (numel (bins) < 3)
    warning ("rose: bin sizes >= pi will not plot correctly");
  endif

  [counts, binctr] = hist (th, bins);
  binctr = binctr(:).';    # Force row vector
  if (isvector (counts))
    counts = counts(:);
  endif

  binedge = binctr(1:end-1) + diff (binctr) / 2;
  binedge = [binedge ; zeros(size(binedge)); zeros(size(binedge)); binedge];
  binedge = binedge(:);
  if (! custom_bins)
    ## Add in implicit edges at 0 and 2*pi
    th = [0; 0; binedge; 2*pi ; 0];
  else
    ## Add in final edge
    last_bin_edge = binctr(end) + diff ([binctr(end), (2*pi+binctr(1))])/2;
    if ((binedge(end) + last_bin_edge)/2 != binctr(end))
      warning ("rose: bin 1 and bin %d are not centered", numel (binctr));
    endif
    th = [0; last_bin_edge; binedge; last_bin_edge; 0];
  endif

  r = zeros (4 * rows (counts), columns (counts));
  r(2:4:end, :) = counts;
  r(3:4:end, :) = counts;

  if (nargout < 2)
    oldfig = [];
    if (! isempty (hax))
      oldfig = get (0, "currentfigure");
    endif
    unwind_protect
      hax = newplot (hax);
      htmp = polar (th, r);
    unwind_protect_cleanup
      if (! isempty (oldfig))
        set (0, "currentfigure", oldfig);
      endif
    end_unwind_protect

    if (nargout > 0)
      thout = htmp;
    endif
  else
    thout = th;
    rout = r;
  endif

endfunction


%!demo
%! clf;
%! rose (2*randn (1e5, 1), 8);
%! title ("rose() angular histogram plot with 8 bins");

%!demo
%! clf;
%! rose ([2*randn(1e5, 1), pi + 2*randn(1e5, 1)]);
%! title ("rose() angular histogram plot with 2 data series");

%!demo
%! clf;
%! rose ([0, 2, 3, 5], [0, pi/2, pi, 3*pi/2]);
%! title ("rose() angular histogram plot with specified bins");

## Test input validation
%!error <Invalid call> rose ()
%!error <Invalid call> rose (1,2,3)
%!warning <bin sizes .= pi will not plot correctly>
%! [th, r] = rose ([1 2 2 4 4 4], 2);
%!warning <bin 1 and bin 3 are not centered>
%! [th, r] = rose ([1 2 2 4 4 4], [1 2 3]);
