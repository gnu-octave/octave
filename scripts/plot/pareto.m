## Copyright (C) 2007-2012 David Bateman
## Copyright (C) 2003 Alberto Terruzzi
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
## @deftypefn  {Function File} {} pareto (@var{y})
## @deftypefnx {Function File} {} pareto (@var{y}, @var{x})
## @deftypefnx {Function File} {} pareto (@var{hax}, @dots{})
## @deftypefnx {Function File} {@var{h} =} pareto (@dots{})
## Draw a Pareto chart.
##
## A Pareto chart is a bar graph that arranges information in such a way
## that priorities for process improvement can be established;  It organizes
## and displays information to show the relative importance of data.  The chart
## is similar to the histogram or bar chart, except that the bars are arranged
## in decreasing magnitude from left to right along the x-axis.
##
## The fundamental idea (Pareto principle) behind the use of Pareto
## diagrams is that the majority of an effect is due to a small subset of the
## causes.  For quality improvement, the first few contributing causes 
## (leftmost bars as presented on the diagram) to a problem usually account for
## the majority of the result.  Thus, targeting these "major causes" for
## elimination results in the most cost-effective improvement scheme.
##
## Typically only the magnitude data @var{y} is present in which case
## @var{x} is taken to be the range @code{1 : length (@var{y})}.  If @var{x}
## is given it may be a string array, a cell array of strings, or a numerical
## vector.
##
## If the first argument @var{hax} is an axes handle, then plot into this axis,
## rather than the current axes returned by @code{gca}.
##
## The optional return value @var{h} is a 2-element vector with a graphics
## handle for the created bar plot and a second handle for the created line
## plot.
##
## An example of the use of @code{pareto} is
##
## @example
## @group
## Cheese = @{"Cheddar", "Swiss", "Camembert", ...
##           "Munster", "Stilton", "Blue"@};
## Sold = [105, 30, 70, 10, 15, 20];
## pareto (Sold, Cheese);
## @end group
## @end example
## @seealso{bar, barh, hist, pie, plot}
## @end deftypefn

function h = pareto (varargin)

  if (nargin != 1 && nargin != 2)
    print_usage ();
  endif

  x = varargin {1}(:).';
  if (nargin == 2)
    y = varargin {2}(:).';
    if (! iscell (y))
      if (ischar (y))
        y = cellstr (y);
      else
        y = cellfun ("num2str", num2cell (y), "uniformoutput", false);
      endif
    endif
  else
    y = cellfun ("int2str", num2cell (1 : numel (x)),
                 "uniformoutput", false);
  endif

  [x, idx] = sort (x, "descend");
  y = y (idx);
  cdf = cumsum (x);
  maxcdf = max (cdf);
  cdf = cdf ./ maxcdf;
  cdf95 = cdf - 0.95;
  idx95 = find (sign (cdf95(1:end-1)) != sign (cdf95(2:end)))(1);

  [ax, hbar, hline] = plotyy (1 : idx95, x (1 : idx95),
                              1 : length (cdf), 100 .* cdf,
                              @bar, @plot);

  axis (ax(1), [1 - 0.6, idx95 + 0.6, 0, maxcdf]);
  axis (ax(2), [1 - 0.6, idx95 + 0.6, 0, 100]);
  set (ax(2), "ytick", [0, 20, 40, 60, 80, 100],
       "yticklabel", {"0%", "20%", "40%", "60%", "80%", "100%"});
  set (ax(1), "xtick", 1 : idx95, "xticklabel", y (1: idx95));
  set (ax(2), "xtick", 1 : idx95, "xticklabel", y (1: idx95));

  if (nargout > 0)
    h = [hbar; hline];
  endif

endfunction


%!demo
%! clf;
%! colormap (jet (64));
%! Cheese = {'Cheddar', 'Swiss', 'Camembert', 'Munster', 'Stilton', 'Blue'};
%! Sold = [105, 30, 70, 10, 15, 20];
%! pareto (Sold, Cheese);

%!demo
%! clf;
%! % Suppose that we want establish which products makes 80% of turnover.
%! Codes = {'AB4','BD7','CF8','CC5','AD11','BB5','BB3','AD8','DF3','DE7'};
%! Value = [2.35 7.9 2.45 1.1 0.15 13.45 5.4 2.05 0.85  1.65]';
%! SoldUnits = [54723 41114 16939 1576091 168000 687197 120222 168195, ...
%!              1084118 55576]';
%! pareto (Value.*SoldUnits, Codes);

