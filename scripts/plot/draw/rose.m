########################################################################
##
## Copyright (C) 2007-2024 The Octave Project Developers
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
## @deftypefn  {} {} rose (@var{theta})
## @deftypefnx {} {} rose (@var{theta}, @var{nbins})
## @deftypefnx {} {} rose (@var{theta}, @var{bins})
## @deftypefnx {} {} rose (@var{hax}, @dots{})
## @deftypefnx {} {@var{h} =} rose (@dots{})
## @deftypefnx {} {[@var{th} @var{r}] =} rose (@dots{})
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
## @seealso{hist, polar}
## @end deftypefn

## Programming note: Ranges are calculated in degrees and then converted to
## radians because the use of integers prevents accumulation of small errors
## that result when using floating point directly.
## The histogram counts are calculated using histc().  See the documentation.
## The final count from histc() contains any values *exactly* equal to the
## last bin edge which is always 2*pi.  Because the input mapping of
## "mod (th, 2*pi)" changes any 2*pi values to 0, this last bin should always
## be zero and can be safely deleted.

function [th, r] = rose (varargin)

  [hax, varargin, nargin] = __plt_get_axis_arg__ ("rose", varargin{:});

  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif

  ## Force theta to range [0,2*pi)
  th = mod (varargin{1}, 2*pi);

  custom_bins = false;
  if (nargin == 1)
    bins = [9 : 18 : 360] / 180 * pi;  
  else
    bins = varargin{2};
    if (isscalar (bins))
      bins = [180/bins : 360/bins : 360] / 180 * pi;
    else
      custom_bins = true;
      ## Force custom bins to [0,2*pi) range
      bins = mod (bins, 2*pi);
      bins = unique (bins);  # de-duplicate and sort bins
      bins = bins(:).';      # Force row vector
    endif
  endif

  binedge = bins(1:end-1) + diff (bins) / 2;  # halfway between bin centers
  if (! custom_bins)
    counts = histc (th, [0, binedge, 2*pi]);  # Add implicit edges at 0, 2*pi
    if (isrow (counts))
      counts = counts(:);
    endif
    ## FIXME: Remove in Octave 11 if no bug reports filed
    if (any (counts(end,:)))
      error ("rose: internal error, histc returned count for theta == 2*pi, please file a bug report");
    endif
    counts(end,:) = [];            # remove temporary bin
  else
    last_binedge = bins(end) + diff ([bins(end), 2*pi+bins(1)]) / 2;
    if (last_binedge >= 2*pi)
      counts = histc (th, [0, last_binedge - 2*pi, binedge, 2*pi]);
    else
      counts = histc (th, [0, binedge, last_binedge, 2*pi]);
    endif
    if (isrow (counts))
      counts = counts(:);
    endif
    counts(end-1,:) += counts(1,:);  # Combine counts for first, last bin
    ## FIXME: Remove in Octave 11 if no bug reports filed
    if (any (counts(end,:)))
      error ("rose: internal error, histc returned count for theta == 2*pi, please file a bug report");
    endif
    counts([1,end], :) = [];         # remove temporary bins
  endif

  binedge = [binedge ; zeros(size(binedge)); zeros(size(binedge)); binedge];
  binedge = binedge(:);
  if (! custom_bins)
    ## Add implicit edges at 0 and 2*pi
    th = [0; 0; binedge; 2*pi ; 0];
  else
    ## Add final edge for custom bin
    th = [0; last_binedge; binedge; last_binedge; 0];
  endif

  r = zeros (4 * rows (counts), columns (counts));
  r(2:4:end, :) = counts;
  r(3:4:end, :) = counts;

  if (nargout < 2)
    if (any (diff (bins) >= pi))
      warning ("rose: bin sizes >= pi will not plot correctly");
    endif

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
      th = htmp;
    endif
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

## Test mapping inputs to [0, 2*pi), 2*pi mapped to bin 1.
%!test
%! [t, r] = rose ([1:1:360]/180*pi + 2*pi);
%! assert (diff (t(2:4:end)), 2*pi/20 * ones (19, 1));
%! assert (r(2:4:end), 18*ones (20, 1));

## Custom # of bins, values exactly at 0 and 2*pi go to bin 1
%!test
%! [t,r] = rose ([0, 2*pi], 4);
%! assert (size (t), [16, 1]);
%! assert (size (r), [16, 1]);
%! assert ([t(2); t(3:4:end)], [0; pi/2; pi; 3*pi/2; 2*pi]);
%! assert (r(2:4:end), [2; 0; 0; 0]);

## Custom bins, synthesized bin1 cut-off is exactly 36 degrees 
%!test
%! [t,r] = rose (deg2rad ([35, 36]), pi * [1/2, 1, 1.5, 1.9]);
%! assert (r(2:4:end), [1; 0; 0; 1]);

## Custom bins, synthesized bin1 cut-off is exactly -36 degrees 
%!test
%! [t,r] = rose (deg2rad ([-36, -37, 360]), pi * [1/10, 1/2, 1, 1.5]);
%! assert (r(2:4:end), [0; 0; 1; 2]);

## Test input validation
%!error <Invalid call> rose ()
%!error <Invalid call> rose (1,2,3)
