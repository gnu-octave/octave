########################################################################
##
## Copyright (C) 1996-2024 The Octave Project Developers
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
## @deftypefn  {} {} barh (@var{y})
## @deftypefnx {} {} barh (@var{x}, @var{y})
## @deftypefnx {} {} barh (@dots{}, @var{w})
## @deftypefnx {} {} barh (@dots{}, @var{style})
## @deftypefnx {} {} barh (@dots{}, @var{prop}, @var{val}, @dots{})
## @deftypefnx {} {} barh (@var{hax}, @dots{})
## @deftypefnx {} {@var{h} =} barh (@dots{}, @var{prop}, @var{val}, @dots{})
## Produce a horizontal bar graph from two vectors of X-Y data.
##
## If only one argument is given, it is taken as a vector of Y values
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
## Y-coordinate.
##
## @item  @qcode{"stacked"}
## Bars are stacked so that each Y value has a single bar composed of
## multiple segments.
##
## @item @qcode{"hist"}
## Side-by-side bars with no gap between bars and centered over the
## Y-coordinate.
##
## @item @qcode{"histc"}
## Side-by-side bars with no gap between bars and left-aligned to the
## Y-coordinate.
## @end table
##
## Optional property/value pairs are passed directly to the underlying patch
## objects.  The full list of properties is documented at
## @ref{Patch Properties}.
##
## If the first argument @var{hax} is an axes handle, then plot into this axes,
## rather than the current axes returned by @code{gca}.
##
## The optional return value @var{h} is a graphics handle to the created
## bar series hggroup.  For a description of the use of the
## bar series, @pxref{XREFbar,,@code{bar}}.
## @seealso{bar, hist, pie, plot, patch}
## @end deftypefn

function varargout = barh (varargin)
  if (nargin < 1)
    print_usage;
  endif
  varargout = cell (nargout, 1);
  [varargout{:}] = __bar__ ("barh", false, varargin{:});
endfunction


%!demo
%! clf;
%! x = rand (10, 1);
%! barh (x);
%! title ("barh() graph");

%!demo
%! clf;
%! h = barh (rand (5, 3));
%! set (h(1), "facecolor", "r");
%! set (h(2), "facecolor", "g");
%! set (h(3), "facecolor", "b");
%! title ("barh() graph w/multiple bars");

%!demo
%! clf;
%! x = -rand (3) .* eye (3) + rand (3) .* (! eye (3));
%! h = barh (x, "stacked");
%! title ("stacked barh() graph including intermingled negative values");

## Tests of bar geometry without plotting, using undocumented [x, y] output
%!test
%! [x, y] = barh (1:3);
%! assert (y, [0.6, 1.6, 2.6; 0.6, 1.6, 2.6; 1.4, 2.4, 3.4; 1.4, 2.4, 3.4], eps);
%! assert (x, [0, 0, 0; 1, 2, 3; 1, 2, 3; 0, 0, 0]);

%!test
%! [x, y] = barh (1:3, 2:4);
%! assert (y, [0.6, 1.6, 2.6; 0.6, 1.6, 2.6; 1.4, 2.4, 3.4; 1.4, 2.4, 3.4], eps);
%! assert (x, [0, 0, 0; 2, 3, 4; 2, 3, 4; 0, 0, 0]);

%!test
%! [x, y] = barh (1:3, 2);
%! assert (y, [0, 1, 2; 0, 1, 2; 2, 3, 4; 2, 3, 4]);
%! assert (x, [0, 0, 0; 1, 2, 3; 1, 2, 3; 0, 0, 0]);

%!test
%! [x, y] = barh (-1:1:1);
%! assert (y, [0.6, 1.6, 2.6; 0.6, 1.6, 2.6; 1.4, 2.4, 3.4; 1.4, 2.4, 3.4], eps);
%! assert (x, [0, 0, 0; -1, 0, 1; -1, 0, 1; 0, 0, 0]);

%!test
%! [x, y] = barh ([1, 2; 3, 4]);
%! assert (y, cat (3, [0.68, 1.68; 0.68, 1.68; 0.96, 1.96; 0.96, 1.96], ...
%!                 [1.04, 2.04; 1.04, 2.04; 1.32, 2.32; 1.32, 2.32]), 2*eps);
%! assert (x, cat (3, [0, 0; 1, 3; 1, 3; 0, 0], [0, 0; 2, 4; 2, 4; 0, 0]));

%!test
%! [x, y] = barh ([1:3; 4:6]);
%! assert (y, cat (3, [2, 5; 2, 5; ; 64/25, 139/25; 64/25, 139/25]/3, ...
%!                 [68 143; 68 143; 82, 157; 82, 157]/75, ...
%!                 [86/25, 161/25; 86/25, 161/25; 4, 7; 4, 7]/3), 2*eps);
%! assert (x, cat (3, [0, 0; 1, 4; 1, 4; 0, 0], ...
%!                    [0, 0; 2, 5; 2, 5; 0, 0], ...
%!                    [0, 0; 3, 6; 3, 6; 0, 0]));

## Test styles grouped staced hist histc
%!test
%! [x, y] = barh ([1:3, 4:6]);
%! [x1, y1] = barh ([1:3, 4:6], "grouped");
%! assert (x, x1);
%! assert (y, y1);

%!test
%! [x, y] = barh ([1:3; 4:6], "stacked");
%! assert (y, cat (3, [0.6, 1.6; 0.6, 1.6; 1.4, 2.4; 1.4, 2.4], ...
%!                    [0.6, 1.6; 0.6, 1.6; 1.4, 2.4; 1.4, 2.4], ...
%!                    [0.6, 1.6; 0.6, 1.6; 1.4, 2.4; 1.4, 2.4]), eps);
%! assert (x, cat (3, [0, 0; 1, 4; 1, 4; 0, 0], ...
%!                    [1, 4; 3, 9; 3, 9; 1, 4], ...
%!                    [3, 9; 6, 15; 6, 15; 3, 9]));

%!test
%! [x, y] = barh ([1:6], "hist");
%! assert (y, [0.5:1:5.5; 0.5:1:5.5; 1.5:1:6.5; 1.5:1:6.5], eps);
%! assert (x, [zeros(1, 6); 1:6; 1:6; zeros(1, 6)]);

%!test
%! [x, y] = barh ([1:3; 4:6], "hist");
%! assert (y, cat (3, [17, 42; 17, 42; 67/3, 142/3; 67/3, 142/3]/25, ...
%!                    [67, 142; 67, 142; 83, 158; 83, 158]/75, ...
%!                    [83/3, 158/3; 83/3, 158/3; 33, 58; 33, 58]/25), 2*eps);
%! assert (x, cat (3, [0, 0; 1, 4; 1, 4; 0, 0], ...
%!                    [0, 0; 2, 5; 2, 5; 0, 0], ...
%!                    [0, 0; 3, 6; 3, 6; 0, 0]));

%!test
%! [x, y] = barh ([1:6], "histc");
%! assert (y, [1:6; 1:6; 2:7; 2:7], eps);
%! assert (x, [zeros(1, 6); 1:6; 1:6; zeros(1, 6)]);

%!test
%! [x, y] = barh ([1:3; 4:6], "histc");
%! assert (y, cat (3, [75, 150; 75, 150; 91, 166; 91, 166]/75, ...
%!                    [91, 166; 91, 166; 107, 182; 107, 182]/75, ...
%!                    [107/3, 182/3; 107/3, 182/3; 41, 66; 41, 66]/25), 2*eps);
%! assert (x, cat (3, [0, 0; 1, 4; 1, 4; 0, 0], ...
%!                    [0, 0; 2, 5; 2, 5; 0, 0], ...
%!                    [0, 0; 3, 6; 3, 6; 0, 0]));


## Test plotting
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = axes ("parent", hf);
%!   hb = barh (hax, 2:4);
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
%!   assert (bardata.horizontal, "on");
%!   assert (bardata.showbaseline, "on");
%!   assert (bardata.xdata, [1; 2; 3]);
%!   assert (bardata.ydata, [2; 3; 4]);
%!
%!   assert (patchdata.type, "patch");
%!   assert (patchdata.ydata, [0.6, 1.6, 2.6;
%!                             0.6, 1.6, 2.6;
%!                             1.4, 2.4, 3.4;
%!                             1.4, 2.4, 3.4], eps);
%!   assert (patchdata.xdata, [0, 0, 0; 2:4; 2:4; 0, 0, 0]);
%!   assert (size (patchdata.faces), [3, 4]);
%!   assert (size (patchdata.vertices), [12, 2]);
%!
%!
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Baseline must be vertical for barh.
%!test <*65671>
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = axes ("parent", hf);
%!   hb = barh (hax, 2:4);
%!   hl = get (hb, "baseline");
%!   baselinedata = get (hl);
%!
%!   assert (baselinedata.type, "line");
%!   assert (baselinedata.xdata, [0, 0]);
%!   assert (diff (baselinedata.ydata) > 0);
%!   assert (baselinedata.color, [0, 0, 0]);
%!   assert (baselinedata.linestyle, "-");
%!
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## barh axes limits and labels should match inverse of bar.
%!test <65671>
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = axes ("parent", hf);
%!   hb = barh (hax, 2:4);
%!   axesdata = get (hax);
%!   assert (axesdata.ylim, [0.5, 3.5]);
%!   assert (axesdata.ytick, 1:3);
%!   assert (axesdata.yticklabel, {"1"; "2"; "3"});
%!   assert (axesdata.xscale, "linear");
%!   assert (axesdata.xlim, [0, 4]);
%!   assert (axesdata.xtick, 0:4);
%!   assert (axesdata.xticklabel, {"0"; "1"; "2"; "3"; "4"});
%!
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Style "stacked" (no difference for single group).
%!test
%! hf1 = figure ("visible", "off");
%! hf2 = figure ("visible", "off");
%! unwind_protect
%!   hax1 = axes ("parent", hf1);
%!   hb1 = barh (hax1, 2:4, "grouped");
%!   bardata1 = get (hb1);
%!   hl1 = bardata1.baseline;
%!   baselinedata1 = get (hl1);
%!   hp1 = bardata1.children;
%!   patchdata1 = get (hp1);
%!
%!   hax2 = axes ("parent", hf2);
%!   hb2 = barh (hax2, 2:4, "stacked");
%!   bardata2 = get (hb2);
%!   hl2 = bardata2.baseline;
%!   baselinedata2 = get (hl2);
%!   hp2 = bardata2.children;
%!   patchdata2 = get (hp2);
%!
%!   assert (bardata1.barlayout, "grouped");
%!   assert (bardata2.barlayout, "stacked");
%!   bardata1.bargroup = [];
%!   bardata1.barlayout = [];
%!   bardata1.baseline = [];
%!   bardata1.children = [];
%!   bardata1.parent = [];
%!
%!   bardata2.bargroup = [];
%!   bardata2.barlayout = [];
%!   bardata2.baseline = [];
%!   bardata2.children = [];
%!   bardata2.parent = [];
%!
%!   baselinedata1.parent = [];
%!   baselinedata2.parent = [];
%!
%!   patchdata1.parent = [];
%!   patchdata2.parent = [];
%!
%!   assert (isequaln (bardata1, bardata2));
%!   assert (isequaln (baselinedata1, baselinedata2));
%!   assert (isequaln (patchdata1, patchdata2));
%!
%! unwind_protect_cleanup
%!   close (hf1);
%!   close (hf2);
%! end_unwind_protect

## Style "hist"
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = axes ("parent", hf);
%!   hb = barh (hax, 2:4, "hist");
%!   bardata = get (hb);
%!   assert (isempty (bardata.children));
%!   assert (! isfield (bardata, "baseline"));
%!   assert (! isfield (bardata, "barlayout"));
%!   assert (! isfield (bardata, "barwidth"));
%!   assert (bardata.type, "patch");
%!   assert (bardata.ydata,  [0.5, 1.5, 2.5;
%!                            0.5, 1.5, 2.5;
%!                            1.5, 2.5, 3.5;
%!                            1.5, 2.5, 3.5], eps);
%!   assert (bardata.xdata, [0, 0, 0; 2:4; 2:4; 0, 0, 0]);
%!   assert (size (bardata.faces), [3, 4]);
%!   assert (size (bardata.vertices), [12, 2]);
%!
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Style "histc"
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = axes ("parent", hf);
%!   hb = barh (hax, 2:4, "histc");
%!   bardata = get (hb);
%!   assert (isempty (bardata.children));
%!   assert (! isfield (bardata, "baseline"));
%!   assert (! isfield (bardata, "barlayout"));
%!   assert (! isfield (bardata, "barwidth"));
%!   assert (bardata.type, "patch");
%!   assert (bardata.ydata,  [1:3; 1:3; 2:4; 2:4]);
%!   assert (bardata.xdata, [0, 0, 0; 2:4; 2:4; 0, 0, 0]);
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
%!   hb = barh (hax, [1:3; 4:6]);
%!   bardata = get (hb);
%!   hp = [bardata.children](:);
%!   patchdata = get (hp);
%!
%!   assert (numel (hb), 3);
%!   assert (numel (hp), 3);
%!   assert (all (strcmp ({bardata.type}, "hggroup")));
%!   assert (bardata(1).baseline, bardata(2).baseline);  # Common baseline
%!   assert (bardata(1).baseline, bardata(3).baseline);  # Common baseline
%!   assert (bardata(1).bargroup, bardata(2).bargroup);  # Common hggroup
%!   assert (bardata(1).bargroup, bardata(3).bargroup);  # Common hggroup
%!   assert (all (strcmp ({bardata.barlayout}, "grouped")));
%!   assert (all (strcmp ({bardata.horizontal}, "on")));
%!   assert ([bardata.basevalue], [0, 0, 0]);
%!   assert ([bardata.xdata], [1, 1, 1; 2, 2, 2]);
%!   assert ([bardata.ydata], [1, 2, 3; 4, 5, 6]);
%!
%!   assert (all (strcmp ({patchdata.type}, "patch")));
%!   assert (patchdata(1).ydata, [2, 5;
%!                                2, 5;
%!                                64/25, 139/25;
%!                                64/25, 139/25]/3, eps);
%!   assert (patchdata(1).xdata, [0, 0; 1, 4; 1, 4; 0, 0]);
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
%!   hb = barh (hax, [1:3; 4:6], "stacked");
%!   bardata = get (hb);
%!   hp = [bardata.children](:);
%!   patchdata = get (hp);
%!
%!   assert (numel (hb), 3);
%!   assert (numel (hp), 3);
%!   assert (all (strcmp ({bardata.type}, "hggroup")));
%!   assert (bardata(1).baseline, bardata(2).baseline);  # Common baseline
%!   assert (bardata(1).baseline, bardata(3).baseline);  # Common baseline
%!   assert (bardata(1).bargroup, bardata(2).bargroup);  # Common hggroup
%!   assert (bardata(1).bargroup, bardata(3).bargroup);  # Common hggroup
%!   assert (all (strcmp ({bardata.barlayout}, "stacked")));
%!   assert (all (strcmp ({bardata.horizontal}, "on")));
%!
%!   assert ([bardata.basevalue], [0, 0, 0]);
%!   assert ([bardata.xdata], [1, 1, 1; 2, 2, 2]);
%!   assert ([bardata.ydata], [1, 2, 3; 4, 5, 6]);
%!
%!   assert (all (strcmp ({patchdata.type}, "patch")));
%!   assert (all (cellfun (@isequal, {patchdata.ydata}, ...
%!                        {[0.6, 1.6; 0.6, 1.6; 1.4, 2.4; 1.4, 2.4]})));
%!   assert (patchdata(1).xdata, [0, 0; 1, 4; 1, 4; 0, 0]);
%!   assert (patchdata(2).xdata, [1, 4; 3, 9; 3, 9; 1, 4]);
%!   assert (patchdata(3).xdata, [3, 9; 6, 15; 6, 15; 3, 9]);
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
%!   hb = barh (hax, [-2, 1, 3; 4, -5, 6], "stacked");
%!   bardata = get (hb);
%!   hp = [bardata.children](:);
%!   patchdata = get (hp);
%!
%!   assert (numel (hb), 3);
%!   assert (numel (hp), 3);
%!   assert (all (strcmp ({bardata.type}, "hggroup")));
%!   assert (bardata(1).baseline, bardata(2).baseline);  # Common baseline
%!   assert (bardata(1).baseline, bardata(3).baseline);  # Common baseline
%!   assert (bardata(1).bargroup, bardata(2).bargroup);  # Common hggroup
%!   assert (bardata(1).bargroup, bardata(3).bargroup);  # Common hggroup
%!   assert (all (strcmp ({bardata.barlayout}, "stacked")));
%!   assert (all (strcmp ({bardata.horizontal}, "on")));
%!
%!   assert ([bardata.basevalue], [0, 0, 0]);
%!   assert ([bardata.xdata], [1, 1, 1; 2, 2, 2]);
%!   assert ([bardata.ydata], [-2, 1, 3; 4, -5, 6]);
%!
%!   assert (all (strcmp ({patchdata.type}, "patch")));
%!   assert (all (cellfun (@isequal, {patchdata.ydata}, ...
%!                        {[0.6, 1.6; 0.6, 1.6; 1.4, 2.4; 1.4, 2.4]})));
%!   assert (patchdata(1).xdata, [0, 0; -2, 4; -2, 4; 0, 0]);
%!   assert (patchdata(2).xdata, [0, 0; 1, -5; 1, -5; 0, 0]);
%!   assert (patchdata(3).xdata, [1, 4; 4, 10; 4, 10; 1, 4]);
%!   assert (size (patchdata(1).faces), [2, 4]);
%!   assert (size (patchdata(1).vertices), [8, 2]);
%!
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Test plot property settings/updates
## Note: Not testing plot and children visibility settings to avoid creating
##       test suite artifacts.

## Switch from grouped to stacked.
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = axes ("parent", hf);
%!   hb = barh (hax, [1:3; 4:6], "grouped");
%!   bardata = get (hb);
%!   hp = [bardata.children](:);
%!   patchdata = get (hp);
%!
%!   ## Verify base behavior.
%!   assert (all (strcmp ({bardata.barlayout}, "grouped")));
%!   assert ([bardata.xdata], [1, 1, 1; 2, 2, 2]);
%!   assert ([bardata.ydata], [1, 2, 3; 4, 5, 6]);
%!   assert (patchdata(1).ydata, [2, 5;
%!                                2, 5;
%!                                64/25, 139/25;
%!                                64/25, 139/25]/3, eps);
%!   assert (patchdata(1).xdata, [0, 0; 1, 4; 1, 4; 0, 0]);
%!
%!   ## Verify changed behavior.
%!   set (hb, "barlayout", "stacked");
%!   bardata = get (hb);
%!   hp = [bardata.children](:);
%!   patchdata = get (hp);
%!
%!   assert (all (strcmp ({bardata.barlayout}, "stacked")));
%!   assert ([bardata.xdata], [1, 1, 1; 2, 2, 2]);
%!   assert ([bardata.ydata], [1, 2, 3; 4, 5, 6]);
%!   assert (all (cellfun (@isequal, {patchdata.ydata}, ...
%!                        {[0.6, 1.6; 0.6, 1.6; 1.4, 2.4; 1.4, 2.4]})));
%!   assert (patchdata(1).xdata, [0, 0; 1, 4; 1, 4; 0, 0]);
%!   assert (patchdata(2).xdata, [1, 4; 3, 9; 3, 9; 1, 4]);
%!   assert (patchdata(3).xdata, [3, 9; 6, 15; 6, 15; 3, 9]);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Non-zero baseline
%!test <*65671>
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = axes ("parent", hf);
%!   hb = barh (hax, 2:4, "basevalue", 3);
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
%!   assert (baselinedata.xdata, [3, 3]);
%!   assert (diff (baselinedata.ydata) > 0);
%!
%!   assert (patchdata.ydata, [0.6, 1.6, 2.6;
%!                             0.6, 1.6, 2.6;
%!                             1.4, 2.4, 3.4;
%!                             1.4, 2.4, 3.4], eps);
%!   assert (patchdata.xdata, [3, 3, 3; 2:4; 2:4; 3, 3, 3]);
%!   assert (size (patchdata.faces), [3, 4]);
%!   assert (size (patchdata.vertices), [12, 2]);
%!
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Color setting and changing:
%!test  # Single group
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = axes ("parent", hf);
%!   hb = barh (hax, 2:4, "facecolor", "r", "edgecolor", "g");
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

%!test # Multiple groups
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = axes ("parent", hf);
%!   hb = barh (hax, [1:3; 4:6], "facecolor", "r", "edgecolor", "g");
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

## Change from horizontal:
%!test <*65671>  # Baseline should change to/from horizontal with bars.
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = axes ("parent", hf);
%!   hb = barh (hax, 2:4);
%!
%!   set (hb, "horizontal", "off");
%!
%!   bardata = get (hb);
%!   hl = bardata.baseline;
%!   baselinedata = get (hl);
%!   hp = bardata.children;
%!   patchdata = get (hp);
%!
%!   assert (bardata.horizontal, "off");
%!   assert (bardata.basevalue, 0);
%!   assert (bardata.xdata, [1; 2; 3]);
%!   assert (bardata.ydata, [2; 3; 4]);
%!
%!   assert (baselinedata.ydata, [0, 0]);
%!   assert (diff (baselinedata.xdata) > 0);
%!
%!   assert (patchdata.xdata, [0.6, 1.6, 2.6;
%!                             0.6, 1.6, 2.6;
%!                             1.4, 2.4, 3.4;
%!                             1.4, 2.4, 3.4], eps);
%!   assert (patchdata.ydata, [0, 0, 0; 2:4; 2:4; 0, 0, 0]);
%!   assert (size (patchdata.faces), [3, 4]);
%!   assert (size (patchdata.vertices), [12, 2]);
%!
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test <*65734>  # Axis ticks after change from horizontal should match bar.
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = axes ("parent", hf);
%!   hb = barh (hax, 2:4);
%!
%!   set (hb, "horizontal", "off");
%!
%!   bardata = get (hb);
%!   axesdata = get (hax);
%!
%!   assert (bardata.horizontal, "off");
%!   assert (bardata.basevalue, 0);
%!   assert (bardata.xdata, [1; 2; 3]);
%!   assert (bardata.ydata, [2; 3; 4]);
%!
%!   assert (axesdata.xscale, "linear");
%!   assert (axesdata.xlim, [0.5, 3.5]);
%!   assert (axesdata.xtick, 1:3);
%!   assert (axesdata.xticklabel, {"1"; "2"; "3"});
%!   assert (axesdata.yscale, "linear");
%!   assert (axesdata.ylim, [0, 4]);
%!   assert (axesdata.ytick, 0:4);
%!   assert (axesdata.yticklabel, {"0"; "1"; "2"; "3"; "4"});
%!
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Change linear/log scale and move baseline/patch values if needed.
%!test <*65671>
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = axes ("parent", hf);
%!   hb = barh (hax, 2:4);
%!   bardata = get (hb);
%!   hl = bardata.baseline;
%!   baselinedata = get (hl);
%!   hp = bardata.children;
%!
%!   assert (bardata.basevalue, 0);
%!   assert (bardata.xdata, [1; 2; 3]);
%!   assert (bardata.ydata, [2; 3; 4]);
%!   assert (baselinedata.xdata, [0, 0]);
%!   assert (baselinedata.ydata, [0.5, 3.5], eps);
%!   assert (get (hax, "xscale"), "linear");
%!   assert (get (hp, "xdata"), [0, 0, 0; 2:4; 2:4; 0, 0, 0]);
%!
%!   warning ("off", "Octave:negative-data-log-axis", "local");
%!   set (hax, "xscale", "log");
%!   assert (get (hax, "xscale"), "log");
%!   assert (get (hb, "basevalue"), 1);
%!   assert (get (hl, "xdata"), [1, 1]);
%!   assert (get (hp, "xdata"), [1, 1, 1; 2:4; 2:4; 1, 1, 1]);
%!
%!   set (hax, "xscale", "linear");
%!   assert (get (hax, "xscale"), "linear");
%!   assert (get (hb, "basevalue"), 0);
%!   assert (get (hl, "xdata"), [0, 0]);
%!   assert (get (hp, "xdata"), [0, 0, 0; 2:4; 2:4; 0, 0, 0]);
%!
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Check baseline updating subfunctions
%!test <*65671>  # Baseline move or basevalue change: update line, bar, patch.
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = axes ("parent", hf);
%!   hb = barh (hax, 2:4);
%!   bardata = get (hb);
%!   hl = bardata.baseline;
%!   baselinedata = get (hl);
%!   hp = bardata.children;
%!   patchdata = get (hp);
%!
%!   assert (bardata.basevalue, 0);
%!   assert (bardata.xdata, [1; 2; 3]);
%!   assert (bardata.ydata, [2; 3; 4]);
%!   assert (baselinedata.xdata, [0, 0]);
%!   assert (baselinedata.ydata, [0.5, 3.5], eps);
%!   assert (patchdata.xdata, [0, 0, 0; 2:4; 2:4; 0, 0, 0]);
%!   assert (patchdata.ydata, [0.6, 1.6, 2.6;
%!                             0.6, 1.6, 2.6;
%!                             1.4, 2.4, 3.4;
%!                             1.4, 2.4, 3.4], eps);
%!
%!   set (hax, "ylim", [0, 5]); # Change axes limits, verify baseline match.
%!   assert (get (hl, "ydata"), [0, 5]);
%!
%!   set (hb, "basevalue", 2); # Change line position through bar object.
%!   axesdata = get (hax);
%!   assert (get (hb, "basevalue"), 2);
%!   assert (get (hl, "xdata"), [2, 2]);
%!   assert (axesdata.xlim, [2, 4]);
%!   assert (axesdata.xtick, [2:0.5:4], eps);
%!   assert (axesdata.xticklabel, {"2"; "2.5"; "3"; "3.5"; "4"});
%!   assert (get (hp, "xdata"), [2, 2, 2; 2:4; 2:4; 2, 2, 2]);
%!
%!   set (hl, "xdata", [-1 -1]);  # Change line position directly.
%!   axesdata = get (hax);
%!   assert (get (hb, "basevalue"), -1);
%!   assert (get (hl, "xdata"), [-1, -1]);
%!   assert (axesdata.xlim, [-1, 4]);
%!   assert (axesdata.xtick, [-1:4]);
%!   assert (axesdata.xticklabel, {"-1"; "0"; "1"; "2"; "3"; "4"});
%!   assert (get (hp, "xdata"), [-1, -1, -1; 2:4; 2:4; -1, -1, -1]);
%!
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Updating base plot ydata.
%!test <*65734>  # Changing xdata should update xlim/ticks for new locations.
%! hf = figure ("visible", "off");
%! unwind_protect
%!   yd = [0.6, 1.6, 2.6; 0.6, 1.6, 2.6; 1.4, 2.4, 3.4; 1.4, 2.4, 3.4];
%!
%!   hax = axes ("parent", hf);
%!   hb = barh (hax, 2:4);
%!   hp = get (hb, "children");
%!
%!   ## Verify base behavior.
%!   assert (get (hb, "xdata"), [1:3]');
%!   assert (get (hp, "ydata"), yd, eps);
%!   assert (get (hax, "ylim"), [0.5, 3.5], eps);
%!   assert (get (hax, "ytick"), 1:3);
%!   assert (get (hax, "yticklabel"), {"1"; "2"; "3"});
%!
%!   ## Verify changed behavior.
%!   set (hb, "xdata", [3:5]');
%!   assert (get (hb, "xdata"), [3:5]');
%!   assert (get (hp, "ydata"), yd + 2, eps);
%!   assert (get (hax, "ylim"), [0.5, 3.5] + 2, eps);
%!   assert (get (hax, "ytick"), 3:5);
%!   assert (get (hax, "yticklabel"), {"3"; "4"; "5"});
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Updating base plot xdata.
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = axes ("parent", hf);
%!   hb = barh (hax, 2:4);
%!   hp = get (hb, "children");
%!
%!   ## Verify base behavior.
%!   assert (get (hb, "ydata"), [2:4]');
%!   assert (get (hp, "xdata"), [0, 0, 0; 2:4; 2:4; 0, 0, 0]);
%!   assert (get (hax, "xlim"), [0, 4]);
%!   assert (get (hax, "xtick"), 0:4);
%!   assert (get (hax, "xticklabel"), {"0"; "1"; "2"; "3"; "4"});
%!
%!   ## Verify changed behavior.
%!   set (hb, "ydata", [-2, 1, 3]');
%!   assert (get (hb, "ydata"), [-2, 1, 3]');
%!   assert (get (hp, "xdata"), [0, 0, 0; -2, 1, 3; -2, 1, 3; 0, 0, 0]);
%!   assert (get (hax, "xlim"), [-2, 3]);
%!   assert (get (hax, "xtick"), -2:3);
%!   assert (get (hax, "xticklabel"), {"-2"; "-1"; "0"; "1"; "2"; "3"});
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Updating barwidth.
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = axes ("parent", hf);
%!   hb = barh (hax, 2:4);
%!   hp = get (hb, "children");
%!
%!   ## Verify base behavior.
%!   assert (get (hb, "barwidth"), 0.8, eps);
%!   assert (get (hb, "xdata"), [1:3]');
%!   assert (get (hp, "ydata"), [0.6, 1.6, 2.6;
%!                             0.6, 1.6, 2.6;
%!                             1.4, 2.4, 3.4;
%!                             1.4, 2.4, 3.4], eps);
%!
%!   ## Verify changed behavior.
%!   set (hb, "barwidth", 0.5);
%!   assert (get (hb, "xdata"), [1:3]');
%!   assert (get (hp, "ydata"), [0.75, 1.75, 2.75;
%!                               0.75, 1.75, 2.75;
%!                               1.25, 2.25, 3.25;
%!                               1.25, 2.25, 3.25], eps);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect


%% Test input validation
%!error <Invalid call> bar ()
%!error <Y must be numeric> barh ("foo")
%!error <X must be a vector> barh ([1 2; 3 4], [1 2 3 4])
%!error <X vector values must be unique> barh ([1 2 3 3], [1 2 3 4])
%!error <length of X and Y must be equal> barh ([1 2 3], [1 2 3 4])
%!error <length of X and Y must be equal> barh ([1 2 3 4], [1 2 3])
