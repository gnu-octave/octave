########################################################################
##
## Copyright (C) 1993-2024 The Octave Project Developers
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
## @deftypefn  {} {} bar (@var{y})
## @deftypefnx {} {} bar (@var{x}, @var{y})
## @deftypefnx {} {} bar (@dots{}, @var{w})
## @deftypefnx {} {} bar (@dots{}, @var{style})
## @deftypefnx {} {} bar (@dots{}, @var{prop}, @var{val}, @dots{})
## @deftypefnx {} {} bar (@var{hax}, @dots{})
## @deftypefnx {} {@var{h} =} bar (@dots{}, @var{prop}, @var{val}, @dots{})
## Produce a bar graph from two vectors of X-Y data.
##
## If only one argument is given, @var{y}, it is taken as a vector of Y values
## and the X coordinates are the range @code{1:numel (@var{y})}.
##
## The optional input @var{w} controls the width of the bars.  A value of
## 1.0 will cause each bar to exactly touch any adjacent bars.
## The default width is 0.8.
##
## If @var{y} is a matrix, then each column of @var{y} is taken to be a
## separate bar graph plotted on the same graph.  By default the columns
## are plotted side-by-side.  This behavior can be changed by the @var{style}
## argument which can take the following values:
##
## @table @asis
## @item @qcode{"grouped"} (default)
## Side-by-side bars with a gap between bars and centered over the
## X-coordinate.
##
## @item  @qcode{"stacked"}
## Bars are stacked so that each X value has a single bar composed of
## multiple segments.
##
## @item @qcode{"hist"}
## Side-by-side bars with no gap between bars and centered over the
## X-coordinate.
##
## @item @qcode{"histc"}
## Side-by-side bars with no gap between bars and left-aligned to the
## X-coordinate.
## @end table
##
## Optional property/value pairs are passed directly to the underlying patch
## objects.  The full list of properties is documented at
## @ref{Patch Properties}.
##
## If the first argument @var{hax} is an axes handle, then plot into this axes,
## rather than the current axes returned by @code{gca}.
##
## The optional return value @var{h} is a vector of handles to the created
## "bar series" hggroups with one handle per column of the variable @var{y}.
## This series makes it possible to change a common element in one bar series
## object and have the change reflected in the other "bar series".
## For example,
##
## @example
## @group
## h = bar (rand (5, 10));
## set (h(1), "basevalue", 0.5);
## @end group
## @end example
##
## @noindent
## changes the position on the base of all of the bar series.
##
## The following example modifies the face and edge colors using
## property/value pairs.
##
## @example
## bar (randn (1, 100), "facecolor", "r", "edgecolor", "b");
## @end example
##
## @noindent
## The default color for bars is taken from the axes' @qcode{"ColorOrder"}
## property.  The default color for bars when a histogram option
## (@qcode{"hist"}, @qcode{"histc"} is used is the @qcode{"Colormap"} property
## of either the axes or figure.  The color of bars can also be set manually
## using the @qcode{"facecolor"} property as shown below.
##
## @example
## @group
## h = bar (rand (10, 3));
## set (h(1), "facecolor", "r")
## set (h(2), "facecolor", "g")
## set (h(3), "facecolor", "b")
## @end group
## @end example
##
## @seealso{barh, hist, pie, plot, patch}
## @end deftypefn

function varargout = bar (varargin)
  if (nargin < 1)
    print_usage;
  endif
  varargout = cell (nargout, 1);
  [varargout{:}] = __bar__ ("bar", true, varargin{:});
endfunction


%!demo
%! clf;
%! y = rand (11, 1);
%! h = bar (y);
%! set (h, "ydata", sort (rand (11, 1)));
%! title ("bar() graph");

%!demo
%! clf;
%! h = bar (rand (5, 3));
%! set (h(1), "facecolor", "r");
%! set (h(2), "facecolor", "g");
%! set (h(3), "facecolor", "b");
%! title ("bar() graph w/multiple bars");

%!demo
%! clf;
%! h = bar (rand (5, 3), "stacked");
%! title ("bar() graph with stacked style");

%!demo
%! clf;
%! y = -rand (3) .* eye (3) + rand (3) .* (! eye (3));
%! h = bar (y, "stacked");
%! title ("stacked bar() graph including intermingled negative values");

## Tests bar geometry without plotting, using undocumented [x, y] output form.
%!test
%! [x, y] = bar (1:3);
%! assert (x, [0.6, 1.6, 2.6; 0.6, 1.6, 2.6; 1.4, 2.4, 3.4; 1.4, 2.4, 3.4], eps);
%! assert (y, [0, 0, 0; 1, 2, 3; 1, 2, 3; 0, 0, 0]);

%!test
%! [x, y] = bar (1:3, 2:4);
%! assert (x, [0.6, 1.6, 2.6; 0.6, 1.6, 2.6; 1.4, 2.4, 3.4; 1.4, 2.4, 3.4], eps);
%! assert (y, [0, 0, 0; 2, 3, 4; 2, 3, 4; 0, 0, 0]);

%!test
%! [x, y] = bar (1:3, 2);
%! assert (x, [0, 1, 2; 0, 1, 2; 2, 3, 4; 2, 3, 4]);
%! assert (y, [0, 0, 0; 1, 2, 3; 1, 2, 3; 0, 0, 0]);

%!test
%! [x, y] = bar (-1:1:1);
%! assert (x, [0.6, 1.6, 2.6; 0.6, 1.6, 2.6; 1.4, 2.4, 3.4; 1.4, 2.4, 3.4], eps);
%! assert (y, [0, 0, 0; -1, 0, 1; -1, 0, 1; 0, 0, 0]);

%!test
%! [x, y] = bar ([1, 2; 3, 4]);
%! assert (x, cat (3, [0.68, 1.68; 0.68, 1.68; 0.96, 1.96; 0.96, 1.96], ...
%!                 [1.04, 2.04; 1.04, 2.04; 1.32, 2.32; 1.32, 2.32]), 2*eps);
%! assert (y, cat (3, [0, 0; 1, 3; 1, 3; 0, 0], [0, 0; 2, 4; 2, 4; 0, 0]));

%!test
%! [x, y] = bar ([1:3; 4:6]);
%! assert (x, cat (3, [2, 5; 2, 5; ; 64/25, 139/25; 64/25, 139/25]/3, ...
%!                 [68 143; 68 143; 82, 157; 82, 157]/75, ...
%!                 [86/25, 161/25; 86/25, 161/25; 4, 7; 4, 7]/3), 2*eps);
%! assert (y, cat (3, [0, 0; 1, 4; 1, 4; 0, 0], ...
%!                    [0, 0; 2, 5; 2, 5; 0, 0], ...
%!                    [0, 0; 3, 6; 3, 6; 0, 0]));

## Test styles
%!test
%! [x, y] = bar ([1:3, 4:6]);
%! [x1, y1] = bar ([1:3, 4:6], "grouped");
%! assert (x, x1);
%! assert (y, y1);

%!test
%! [x, y] = bar ([1:3; 4:6], "stacked");
%! assert (x, cat (3, [0.6, 1.6; 0.6, 1.6; 1.4, 2.4; 1.4, 2.4], ...
%!                    [0.6, 1.6; 0.6, 1.6; 1.4, 2.4; 1.4, 2.4], ...
%!                    [0.6, 1.6; 0.6, 1.6; 1.4, 2.4; 1.4, 2.4]), eps);
%! assert (y, cat (3, [0, 0; 1, 4; 1, 4; 0, 0], ...
%!                    [1, 4; 3, 9; 3, 9; 1, 4], ...
%!                    [3, 9; 6, 15; 6, 15; 3, 9]));

%!test
%! [x, y] = bar ([1:6], "hist");
%! assert (x, [0.5:1:5.5; 0.5:1:5.5; 1.5:1:6.5; 1.5:1:6.5], eps);
%! assert (y, [zeros(1, 6); 1:6; 1:6; zeros(1, 6)]);

%!test
%! [x, y] = bar ([1:3; 4:6], "hist");
%! assert (x, cat (3, [17, 42; 17, 42; 67/3, 142/3; 67/3, 142/3]/25, ...
%!                    [67, 142; 67, 142; 83, 158; 83, 158]/75, ...
%!                    [83/3, 158/3; 83/3, 158/3; 33, 58; 33, 58]/25), 2*eps);
%! assert (y, cat (3, [0, 0; 1, 4; 1, 4; 0, 0], ...
%!                    [0, 0; 2, 5; 2, 5; 0, 0], ...
%!                    [0, 0; 3, 6; 3, 6; 0, 0]));

%!test
%! [x, y] = bar ([1:6], "histc");
%! assert (x, [1:6; 1:6; 2:7; 2:7], eps);
%! assert (y, [zeros(1, 6); 1:6; 1:6; zeros(1, 6)]);

%!test
%! [x, y] = bar ([1:3; 4:6], "histc");
%! assert (x, cat (3, [75, 150; 75, 150; 91, 166; 91, 166]/75, ...
%!                    [91, 166; 91, 166; 107, 182; 107, 182]/75, ...
%!                    [107/3, 182/3; 107/3, 182/3; 41, 66; 41, 66]/25), 2*eps);
%! assert (y, cat (3, [0, 0; 1, 4; 1, 4; 0, 0], ...
%!                    [0, 0; 2, 5; 2, 5; 0, 0], ...
%!                    [0, 0; 3, 6; 3, 6; 0, 0]));

## Test plotting
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = axes ("parent", hf);
%!   hb = bar (hax, 2:4);
%!   bardata = get (hb);
%!   hl = bardata.baseline;
%!   baselinedata = get (hl);
%!   hp = bardata.children;
%!   patchdata = get (hp);
%!   axesdata = get (hax);
%!
%!   assert (isfield (bardata, "bargroup"));
%!   assert (isfield (bardata, "barlayout"));
%!   assert (isfield (bardata, "barwidth"));
%!   assert (isfield (bardata, "baseline"));
%!   assert (isfield (bardata, "basevalue"));
%!   assert (isfield (bardata, "horizontal"));
%!   assert (isfield (bardata, "showbaseline"));
%!   assert (isfield (bardata, "xdata"));
%!   assert (isfield (bardata, "ydata"));
%!
%!   assert (bardata.parent, hax);
%!   assert (bardata.bargroup, hb);
%!   assert (bardata.type, "hggroup");
%!   assert (bardata.barlayout, "grouped");
%!   assert (bardata.barwidth, 0.8);
%!   assert (bardata.basevalue, 0);
%!   assert (bardata.horizontal, "off");
%!   assert (bardata.showbaseline, "on");
%!   assert (bardata.xdata, [1; 2; 3]);
%!   assert (bardata.ydata, [2; 3; 4]);
%!
%!   assert (baselinedata.type, "line");
%!   assert (baselinedata.ydata, [0, 0]);
%!   assert (diff (baselinedata.xdata) > 0);
%!   assert (baselinedata.color, [0, 0, 0]);
%!   assert (baselinedata.linestyle, "-");
%!
%!   assert (patchdata.type, "patch");
%!   assert (patchdata.xdata, [0.6, 1.6, 2.6; ...
%!                             0.6, 1.6, 2.6; ...
%!                             1.4, 2.4, 3.4; ...
%!                             1.4, 2.4, 3.4], eps);
%!   assert (patchdata.ydata, [0, 0, 0; 2:4; 2:4; 0, 0, 0]);
%!   assert (size (patchdata.faces), [3, 4]);
%!   assert (size (patchdata.vertices), [12, 2]);
%!
%!   assert (axesdata.xscale, "linear");
%!   assert (axesdata.xlim, [0.5, 3.5]);
%!   assert (axesdata.xtick, 1:3);
%!   assert (get(hax, 'xticklabel'), {"1"; "2"; "3"});
%!   assert (axesdata.yscale, "linear");
%!   assert (axesdata.ylim, [0, 4]);
%!   assert (axesdata.ytick, 0:4);
%!   assert (axesdata.yticklabel, {"0"; "1"; "2"; "3"; "4"});
%!
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Style stacked (no difference for single group).
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = axes ("parent", hf);
%!   hb = bar (hax, 2:4, "grouped");
%!   bardata1 = get (hb);
%!   hl = bardata1.baseline;
%!   baselinedata1 = get (hl);
%!   hp = bardata1.children;
%!   patchdata1 = get (hp);
%!
%!   hb = bar (hax, 2:4, "stacked");
%!   bardata2 = get (hb);
%!   hl = bardata2.baseline;
%!   baselinedata2 = get (hl);
%!   hp = bardata2.children;
%!   patchdata2 = get (hp);
%!
%!   assert (bardata1.barlayout, "grouped");
%!   assert (bardata2.barlayout, "stacked");
%!   bardata1.bargroup = [];
%!   bardata1.barlayout = [];
%!   bardata1.baseline = [];
%!   bardata1.children = [];
%!
%!   bardata2.bargroup = [];
%!   bardata2.barlayout = [];
%!   bardata2.baseline = [];
%!   bardata2.children = [];
%!
%!   patchdata1.parent = [];
%!   patchdata2.parent = [];
%!
%!   assert (isequaln (bardata1, bardata2));
%!   assert (isequaln (baselinedata1, baselinedata2));
%!   assert (isequaln (patchdata1, patchdata2));
%!
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Style hist.
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = axes ("parent", hf);
%!   hb = bar (hax, 2:4, "hist");
%!   bardata = get (hb);
%!   assert (isempty (bardata.children));
%!   assert (! isfield (bardata, "baseline"));
%!   assert (! isfield (bardata, "barlayout"));
%!   assert (! isfield (bardata, "barwidth"));
%!   assert (bardata.type, "patch");
%!   assert (bardata.xdata,  [0.5, 1.5, 2.5; ...
%!                            0.5, 1.5, 2.5; ...
%!                            1.5, 2.5, 3.5; ...
%!                            1.5, 2.5, 3.5], eps);
%!   assert (bardata.ydata, [0, 0, 0; 2:4; 2:4; 0, 0, 0]);
%!   assert (size (bardata.faces), [3, 4]);
%!   assert (size (bardata.vertices), [12, 2]);
%!
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Style histc.
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = axes ("parent", hf);
%!   hb = bar (hax, 2:4, "histc");
%!   bardata = get (hb);
%!   assert (isempty (bardata.children));
%!   assert (! isfield (bardata, "baseline"));
%!   assert (! isfield (bardata, "barlayout"));
%!   assert (! isfield (bardata, "barwidth"));
%!   assert (bardata.type, "patch");
%!   assert (bardata.xdata,  [1:3; 1:3; 2:4; 2:4]);
%!   assert (bardata.ydata, [0, 0, 0; 2:4; 2:4; 0, 0, 0]);
%!   assert (size (bardata.faces), [3, 4]);
%!   assert (size (bardata.vertices), [12, 2]);
%!
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Bar groups grouped.
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = axes ("parent", hf);
%!   hb = bar (hax, [1:3; 4:6]);
%!   bardata = get (hb);
%!   hp = [bardata.children](:);
%!   patchdata = get (hp);
%!
%!   assert (numel (hb), 3);
%!   assert (numel (hp), 3);
%!   assert (all (strcmp ({bardata.type}, 'hggroup')));
%!   assert (bardata(1).baseline, bardata(2).baseline); # Common baseline
%!   assert (bardata(1).baseline, bardata(3).baseline); # Common baseline
%!   assert (bardata(1).bargroup, bardata(2).bargroup); # Common hggroup
%!   assert (bardata(1).bargroup, bardata(3).bargroup); # Common hggroup
%!   assert (all (strcmp ({bardata.barlayout}, 'grouped')));
%!   assert (all (strcmp ({bardata.horizontal}, 'off')));
%!   assert ([bardata.basevalue], [0, 0, 0]);
%!   assert ([bardata.xdata], [1, 1, 1; 2, 2, 2]);
%!   assert ([bardata.ydata], [1, 2, 3; 4, 5, 6]);
%!
%!   assert (all (strcmp ({patchdata.type}, 'patch')));
%!   assert (patchdata(1).xdata, [2, 5; ...
%!                                2, 5; ...
%!                                64/25, 139/25; ...
%!                                64/25, 139/25]/3, eps);
%!   assert (patchdata(1).ydata, [0, 0; 1, 4; 1, 4; 0, 0]);
%!   assert (size (patchdata(1).faces), [2, 4]);
%!   assert (size (patchdata(1).vertices), [8, 2]);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Bar groups stacked.
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = axes ("parent", hf);
%!   hb = bar (hax, [1:3; 4:6], "stacked");
%!   bardata = get (hb);
%!   hp = [bardata.children](:);
%!   patchdata = get (hp);
%!
%!   assert (numel (hb), 3);
%!   assert (numel (hp), 3);
%!   assert (all (strcmp ({bardata.type}, 'hggroup')));
%!   assert (bardata(1).baseline, bardata(2).baseline); # Common baseline
%!   assert (bardata(1).baseline, bardata(3).baseline); # Common baseline
%!   assert (bardata(1).bargroup, bardata(2).bargroup); # Common hggroup
%!   assert (bardata(1).bargroup, bardata(3).bargroup); # Common hggroup
%!   assert (all (strcmp ({bardata.barlayout}, 'stacked')));
%!   assert (all (strcmp ({bardata.horizontal}, 'off')));
%!
%!   assert ([bardata.basevalue], [0, 0, 0]);
%!   assert ([bardata.xdata], [1, 1, 1; 2, 2, 2]);
%!   assert ([bardata.ydata], [1, 2, 3; 4, 5, 6]);
%!
%!   assert (all (strcmp ({patchdata.type}, 'patch')));
%!   assert (all (cellfun (@isequal, {patchdata.xdata}, ...
%!                        {[0.6, 1.6; 0.6, 1.6; 1.4, 2.4; 1.4, 2.4]})));
%!   assert (patchdata(1).ydata, [0, 0; 1, 4; 1, 4; 0, 0]);
%!   assert (patchdata(2).ydata, [1, 4; 3, 9; 3, 9; 1, 4]);
%!   assert (patchdata(3).ydata, [3, 9; 6, 15; 6, 15; 3, 9]);
%!   assert (size (patchdata(1).faces), [2, 4]);
%!   assert (size (patchdata(1).vertices), [8, 2]);
%!
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Stacked with negative values.
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = axes ("parent", hf);
%!   hb = bar (hax, [-2, 1, 3; 4, -5, 6], "stacked");
%!   bardata = get (hb);
%!   hp = [bardata.children](:);
%!   patchdata = get (hp);
%!
%!   assert (numel (hb), 3);
%!   assert (numel (hp), 3);
%!   assert (all (strcmp ({bardata.type}, 'hggroup')));
%!   assert (bardata(1).baseline, bardata(2).baseline); # Common baseline
%!   assert (bardata(1).baseline, bardata(3).baseline); # Common baseline
%!   assert (bardata(1).bargroup, bardata(2).bargroup); # Common hggroup
%!   assert (bardata(1).bargroup, bardata(3).bargroup); # Common hggroup
%!   assert (all (strcmp ({bardata.barlayout}, 'stacked')));
%!   assert (all (strcmp ({bardata.horizontal}, 'off')));
%!
%!   assert ([bardata.basevalue], [0, 0, 0]);
%!   assert ([bardata.xdata], [1, 1, 1; 2, 2, 2]);
%!   assert ([bardata.ydata], [-2, 1, 3; 4, -5, 6]);
%!
%!   assert (all (strcmp ({patchdata.type}, 'patch')));
%!   assert (all (cellfun (@isequal, {patchdata.xdata}, ...
%!                        {[0.6, 1.6; 0.6, 1.6; 1.4, 2.4; 1.4, 2.4]})));
%!   assert (patchdata(1).ydata, [0, 0; -2, 4; -2, 4; 0, 0]);
%!   assert (patchdata(2).ydata, [0, 0; 1, -5; 1, -5; 0, 0]);
%!   assert (patchdata(3).ydata, [1, 4; 4, 10; 4, 10; 1, 4]);
%!   assert (size (patchdata(1).faces), [2, 4]);
%!   assert (size (patchdata(1).vertices), [8, 2]);
%!
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Test plot property settings/updates
## Note - Not testing plot and children visibile settings to avoid creating
##        test suite artifacts.

## Switch from grouped to stacked.
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = axes ("parent", hf);
%!   hb = bar (hax, [1:3; 4:6], "grouped");
%!   bardata = get (hb);
%!   hp = [bardata.children](:);
%!   patchdata = get (hp);
%!
%!   ## Verify base behavior.
%!   assert (all (strcmp ({bardata.barlayout}, 'grouped')));
%!   assert ([bardata.xdata], [1, 1, 1; 2, 2, 2]);
%!   assert ([bardata.ydata], [1, 2, 3; 4, 5, 6]);
%!   assert (patchdata(1).xdata, [2, 5; ...
%!                                2, 5; ...
%!                                64/25, 139/25; ...
%!                                64/25, 139/25]/3, eps);
%!   assert (patchdata(1).ydata, [0, 0; 1, 4; 1, 4; 0, 0]);
%!
%!   ## Verify changed behavior.
%!   set (hb, "barlayout", "stacked");
%!   bardata = get (hb);
%!   hp = [bardata.children](:);
%!   patchdata = get (hp);
%!
%!   assert (all (strcmp ({bardata.barlayout}, 'stacked')));
%!   assert ([bardata.xdata], [1, 1, 1; 2, 2, 2]);
%!   assert ([bardata.ydata], [1, 2, 3; 4, 5, 6]);
%!   assert (all (cellfun (@isequal, {patchdata.xdata}, ...
%!                        {[0.6, 1.6; 0.6, 1.6; 1.4, 2.4; 1.4, 2.4]})));
%!   assert (patchdata(1).ydata, [0, 0; 1, 4; 1, 4; 0, 0]);
%!   assert (patchdata(2).ydata, [1, 4; 3, 9; 3, 9; 1, 4]);
%!   assert (patchdata(3).ydata, [3, 9; 6, 15; 6, 15; 3, 9]);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Non-zero baseline
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = axes ("parent", hf);
%!   hb = bar (hax, 2:4, "basevalue", 3);
%!   bardata = get (hb);
%!   hl = bardata.baseline;
%!   baselinedata = get (hl);
%!   hp = bardata.children;
%!   patchdata = get (hp);
%!
%!   assert (bardata.basevalue, 3);
%!   assert (bardata.xdata, [1; 2; 3]);
%!   assert (bardata.ydata, [2; 3; 4]);
%!
%!   assert (baselinedata.ydata, [3, 3]);
%!   assert (diff (baselinedata.xdata) > 0);
%!
%!   assert (patchdata.xdata, [0.6, 1.6, 2.6; ...
%!                             0.6, 1.6, 2.6; ...
%!                             1.4, 2.4, 3.4; ...
%!                             1.4, 2.4, 3.4], eps);
%!   assert (patchdata.ydata, [3, 3, 3; 2:4; 2:4; 3, 3, 3]);
%!   assert (size (patchdata.faces), [3, 4]);
%!   assert (size (patchdata.vertices), [12, 2]);
%!
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Color settings:
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = axes ("parent", hf);
%!   hb = bar (hax, 2:4, "facecolor", "r", "edgecolor", "g");
%!   bardata = get (hb);
%!   hp = bardata.children;
%!   patchdata = get (hp);
%!
%!   assert (bardata.facecolor, [1, 0, 0]);
%!   assert (bardata.edgecolor, [0, 1, 0]);
%!   assert (patchdata.facecolor, [1, 0, 0]);
%!   assert (patchdata.edgecolor, [0, 1, 0]);
%!
%!   set (hb, "facecolor", "b");
%!   assert (get (hb, "facecolor"), [0, 0, 1]);
%!   assert (get (hp, "facecolor"), [0, 0, 1]);
%!   set (hb, "edgecolor", "k");
%!   assert (get (hb, "edgecolor"), [0, 0, 0]);
%!   assert (get (hp, "edgecolor"), [0, 0, 0]);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = axes ("parent", hf);
%!   hb = bar (hax, [1:3; 4:6], "facecolor", "r", "edgecolor", "g");
%!   bardata = get (hb);
%!   hp = [bardata.children](:);
%!   patchdata = get (hp);
%!
%!   assert (all (vertcat (bardata.facecolor) == [1, 0, 0]));
%!   assert (all (vertcat (bardata.edgecolor) == [0, 1, 0]));
%!   assert (all (vertcat (patchdata.facecolor) == [1, 0, 0]));
%!   assert (all (vertcat (patchdata.edgecolor) == [0, 1, 0]));
%!
%!   set (hb, "facecolor", "b");
%!   set (hb, "edgecolor", "k");
%!   bardata = get (hb);
%!   patchdata = get (hp);
%!   assert (all (vertcat (bardata.facecolor) == [0, 0, 1]));
%!   assert (all (vertcat (bardata.edgecolor) == [0, 0, 0]));
%!   assert (all (vertcat (patchdata.facecolor) == [0, 0, 1]));
%!   assert (all (vertcat (patchdata.edgecolor) == [0, 0, 0]));
%!
%!   set (hb(2), "facecolor", "w");
%!   set (hb(2), "edgecolor", "r");
%!   bardata = get (hb);
%!   patchdata = get (hp);
%!   assert (all (vertcat (bardata([1,3]).facecolor) == [0, 0, 1]));
%!   assert (all (vertcat (bardata([1,3]).edgecolor) == [0, 0, 0]));
%!   assert (all (vertcat (patchdata([1,3]).facecolor) == [0, 0, 1]));
%!   assert (all (vertcat (patchdata([1,3]).edgecolor) == [0, 0, 0]));
%!   assert (bardata(2).facecolor, [1, 1, 1]);
%!   assert (bardata(2).edgecolor, [1, 0, 0]);
%!   assert (patchdata(2).facecolor, [1, 1, 1]);
%!   assert (patchdata(2).edgecolor, [1, 0, 0]);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Change to horizontal:

%!test <65671>  # Baseline should change to horizontal with bars.
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = axes ("parent", hf);
%!   hb = bar (hax, 2:4);
%!
%!   set (hb, "horizontal", "on");
%!
%!   bardata = get (hb);
%!   hl = bardata.baseline;
%!   baselinedata = get (hl);
%!   hp = bardata.children;
%!   patchdata = get (hp);
%!
%!   assert (bardata.horizontal, "on");
%!   assert (bardata.basevalue, 0);
%!   assert (bardata.xdata, [1; 2; 3]);
%!   assert (bardata.ydata, [2; 3; 4]);
%!
%!   assert (baselinedata.xdata, [0, 0]);
%!   assert (diff (baselinedata.ydata) > 0);
%!
%!   assert (patchdata.ydata, [0.6, 1.6, 2.6; ...
%!                             0.6, 1.6, 2.6; ...
%!                             1.4, 2.4, 3.4; ...
%!                             1.4, 2.4, 3.4], eps);
%!   assert (patchdata.xdata, [0, 0, 0; 2:4; 2:4; 0, 0, 0]);
%!   assert (size (patchdata.faces), [3, 4]);
%!   assert (size (patchdata.vertices), [12, 2]);
%!
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test <65734> # Axis ticks after change to horizontal should match barh.
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = axes ("parent", hf);
%!   hb = bar (hax, 2:4);
%!
%!   set (hb, "horizontal", "on");
%!
%!   bardata = get (hb);
%!   axesdata = get (hax);
%!
%!   assert (bardata.horizontal, "on");
%!   assert (bardata.basevalue, 0);
%!   assert (bardata.xdata, [1; 2; 3]);
%!   assert (bardata.ydata, [2; 3; 4]);
%!
%!   assert (axesdata.yscale, "linear");
%!   assert (axesdata.ylim, [0.5, 3.5]);
%!   assert (axesdata.ytick, 1:3);
%!   assert (get(hax, 'yticklabel'), {"1"; "2"; "3"});
%!   assert (axesdata.xscale, "linear");
%!   assert (axesdata.xlim, [0, 4]);
%!   assert (axesdata.xtick, 0:4);
%!   assert (axesdata.xticklabel, {"0"; "1"; "2"; "3"; "4"});
%!
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Check baseline updating subfunctions.
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = axes ("parent", hf);
%!   hb = bar (hax, 2:4);
%!   bardata = get (hb);
%!   hl = bardata.baseline;
%!   baselinedata = get (hl);
%!   hp = bardata.children;
%!   patchdata = get (hp);
%!
%!   assert (bardata.basevalue, 0);
%!   assert (bardata.xdata, [1; 2; 3]);
%!   assert (bardata.ydata, [2; 3; 4]);
%!   assert (baselinedata.ydata, [0, 0]);
%!   assert (baselinedata.xdata, [0.5, 3.5], eps);
%!   assert (patchdata.xdata, [0.6, 1.6, 2.6; ...
%!                             0.6, 1.6, 2.6; ...
%!                             1.4, 2.4, 3.4; ...
%!                             1.4, 2.4, 3.4], eps);
%!   assert (patchdata.ydata, [0, 0, 0; 2:4; 2:4; 0, 0, 0]);
%!
%!   set (hax, "xlim", [0, 5]); # Change axes limits, verify baseline match.
%!   assert (get (hl, "xdata"), [0, 5]);
%!
%!   set (hb, "basevalue", 2); # Change line position through bar object.
%!   axesdata = get (hax);
%!   assert (get (hb, "basevalue"), 2);
%!   assert (get (hl, "ydata"), [2, 2]);
%!   assert (axesdata.ylim, [2, 4]);
%!   assert (axesdata.ytick, [2:0.5:4], eps);
%!   assert (axesdata.yticklabel, {"2"; "2.5"; "3"; "3.5"; "4"});
%!   assert (get (hp, "ydata"), [2, 2, 2; 2:4; 2:4; 2, 2, 2]);
%!
%!   set (hl, "ydata", [-1 -1]);  # Change line position directly.
%!   axesdata = get (hax);
%!   assert (get (hb, "basevalue"), -1);
%!   assert (get (hl, "ydata"), [-1, -1]);
%!   assert (axesdata.ylim, [-1, 4]);
%!   assert (axesdata.ytick, [-1:4], eps);
%!   assert (axesdata.yticklabel, {"-1"; "0"; "1"; "2"; "3"; "4"});
%!   assert (get (hp, "ydata"), [-1, -1, -1; 2:4; 2:4; -1, -1, -1]);
%!
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Change linear/log scale and move baseline/patch values if needed.
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = axes ("parent", hf);
%!   hb = bar (hax, 2:4);
%!   bardata = get (hb);
%!   hl = bardata.baseline;
%!   baselinedata = get (hl);
%!   hp = bardata.children;
%!
%!   assert (bardata.basevalue, 0);
%!   assert (bardata.xdata, [1; 2; 3]);
%!   assert (bardata.ydata, [2; 3; 4]);
%!   assert (baselinedata.ydata, [0, 0]);
%!   assert (baselinedata.xdata, [0.5, 3.5], eps);
%!   assert (get (hax, "yscale"), "linear");
%!   assert (get (hp, "ydata"), [0, 0, 0; 2:4; 2:4; 0, 0, 0]);
%!
%!   warning ("off", "Octave:negative-data-log-axis", "local");
%!   set (hax, "yscale", "log");
%!   assert (get (hax, "yscale"), "log");
%!   assert (get (hb, "basevalue"), 1);
%!   assert (get (hl, "ydata"), [1, 1]);
%!   assert (get (hp, "ydata"), [1, 1, 1; 2:4; 2:4; 1, 1, 1]);
%!
%!   set (hax, "yscale", "linear");
%!   assert (get (hax, "yscale"), "linear");
%!   assert (get (hb, "basevalue"), 0);
%!   assert (get (hl, "ydata"), [0, 0]);
%!   assert (get (hp, "ydata"), [0, 0, 0; 2:4; 2:4; 0, 0, 0]);
%!
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Updating base plot xdata.
%!test <65734> # changing xdata should update xlim/ticks for new locations.
%! hf = figure ("visible", "off");
%! unwind_protect
%!   xd = [0.6, 1.6, 2.6; 0.6, 1.6, 2.6; 1.4, 2.4, 3.4; 1.4, 2.4, 3.4];
%!
%!   hax = axes ("parent", hf);
%!   hb = bar (hax, 2:4);
%!   hp = get (hb, "children");
%!
%!   ## Verify base behavior.
%!   assert (get (hb, "xdata"), [1:3]');
%!   assert (get (hp, "xdata"), xd, eps);
%!   assert (get (hax, "xlim"), [0.5, 3.5], eps);
%!   assert (get (hax, "xtick"), 1:3);
%!   assert (get (hax, "xticklabel"), {"1"; "2"; "3"});
%!
%!   ## Verify changed behavior.
%!   set (hb, "xdata", [3:5]');
%!   assert (get (hb, "xdata"), [3:5]');
%!   assert (get (hp, "xdata"), xd + 2, eps);
%!   assert (get (hax, "xlim"), [0.5, 3.5] + 2, eps);
%!   assert (get (hax, "xtick"), 3:5);
%!   assert (get (hax, "xticklabel"), {"3"; "4"; "5"});
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Updating base plot ydata.
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = axes ("parent", hf);
%!   hb = bar (hax, 2:4);
%!   hp = get (hb, "children");
%!
%!   ## Verify base behavior.
%!   assert (get (hb, "ydata"), [2:4]');
%!   assert (get (hp, "ydata"), [0, 0, 0; 2:4; 2:4; 0, 0, 0]);
%!   assert (get (hax, "ylim"), [0, 4]);
%!   assert (get (hax, "ytick"), 0:4);
%!   assert (get (hax, "yticklabel"), {"0"; "1"; "2"; "3"; "4"});
%!
%!   ## Verify changed behavior.
%!   set (hb, "ydata", [-2, 1, 3]');
%!   assert (get (hb, "ydata"), [-2, 1, 3]');
%!   assert (get (hp, "ydata"), [0, 0, 0; -2, 1, 3; -2, 1, 3; 0, 0, 0]);
%!   assert (get (hax, "ylim"), [-2, 3]);
%!   assert (get (hax, "ytick"), -2:3);
%!   assert (get (hax, "yticklabel"), {"-2"; "-1"; "0"; "1"; "2"; "3"});
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Updating barwidth.
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = axes ("parent", hf);
%!   hb = bar (hax, 2:4);
%!   hp = get (hb, "children");
%!
%!   ## Verify base behavior.
%!   assert (get (hb, "barwidth"), 0.8, eps);
%!   assert (get (hb, "xdata"), [1:3]');
%!   assert (get (hp, "xdata"), [0.6, 1.6, 2.6; ...
%!                             0.6, 1.6, 2.6; ...
%!                             1.4, 2.4, 3.4; ...
%!                             1.4, 2.4, 3.4], eps);
%!
%!   ## Verify changed behavior.
%!   set (hb, "barwidth", 0.5);
%!   assert (get (hb, "xdata"), [1:3]');
%!   assert (get (hp, "xdata"), [0.75, 1.75, 2.75; ...
%!                               0.75, 1.75, 2.75; ...
%!                               1.25, 2.25, 3.25; ...
%!                               1.25, 2.25, 3.25], eps);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect


%% Test input validation
%!error <Invalid call> bar ()
%!error <Y must be numeric> bar ("foo")
%!error <X must be a vector> bar ([1 2; 3 4], [1 2 3 4])
%!error <X vector values must be unique> bar ([1 2 3 3], [1 2 3 4])
%!error <length of X and Y must be equal> bar ([1 2 3], [1 2 3 4])
%!error <length of X and Y must be equal> bar ([1 2 3 4], [1 2 3])
