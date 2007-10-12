## Copyright (C) 1994, 1995, 1996, 1997, 1999, 2000, 2002, 2003, 2004,
##               2005, 2006, 2007 John W. Eaton
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
## @deftypefn {Function File} {} axis (@var{limits})
## Set axis limits for plots.
##
## The argument @var{limits} should be a 2, 4, or 6 element vector.  The
## first and second elements specify the lower and upper limits for the x
## axis.  The third and fourth specify the limits for the y axis, and the
## fifth and sixth specify the limits for the z axis.
##
## Without any arguments, @code{axis} turns autoscaling on.  
##
## With one output argument, @code{x=axis} returns the current axes 
## (this is not yet implemented for automatic axes).
##
## The vector argument specifying limits is optional, and additional
## string arguments may be used to specify various axis properties.  For
## example,
##
## @example
## axis ([1, 2, 3, 4], "square");
## @end example
##
## @noindent
## forces a square aspect ratio, and
##
## @example
## axis ("labely", "tic");
## @end example
##
## @noindent
## turns tic marks on for all axes and tic mark labels on for the y-axis
## only.
##
## @noindent
## The following options control the aspect ratio of the axes.
##
## @table @code
## @item "square"
## Force a square aspect ratio.
## @item "equal"
## Force x distance to equal y-distance.
## @item "normal"
## Restore the balance.
## @end table
##
## @noindent
## The following options control the way axis limits are interpreted.
##
## @table @code
## @item "auto" 
## Set the specified axes to have nice limits around the data
## or all if no axes are specified.
## @item "manual" 
## Fix the current axes limits.
## @item "tight"
## Fix axes to the limits of the data (not implemented).
## @end table
##
## @noindent
## The option @code{"image"} is equivalent to @code{"tight"} and
## @code{"equal"}.
##
## @noindent
## The following options affect the appearance of tic marks.
##
## @table @code
## @item "on" 
## Turn tic marks and labels on for all axes.
## @item "off"
## Turn tic marks off for all axes.
## @item "tic[xyz]"
## Turn tic marks on for all axes, or turn them on for the
## specified axes and off for the remainder.
## @item "label[xyz]"
## Turn tic labels on for all axes, or turn them on for the 
## specified axes and off for the remainder.
## @item "nolabel"
## Turn tic labels off for all axes.
## @end table
## Note, if there are no tic marks for an axis, there can be no labels.
##
## @noindent
## The following options affect the direction of increasing values on
## the axes.
##
## @table @code
## @item "ij"
## Reverse y-axis, so lower values are nearer the top.
## @item "xy" 
## Restore y-axis, so higher values are nearer the top. 
## @end table
## 
## @end deftypefn

## Author: jwe

## PKG_ADD: mark_as_command axis

function curr_axis = axis (ax, varargin)

  ca = gca ();

  if (nargin == 0)
    if (nargout == 0)
      set (ca, "xlimmode", "auto", "ylimmode", "auto", "zlimmode", "auto");
    else
      xlim = get (ca, "xlim");
      ylim = get (ca, "ylim");
      zlim = get (ca, "zlim");
      curr_axis = [xlim, ylim, zlim];
    endif

  elseif (ischar (ax))
    ax = tolower (ax);
    len = length (ax);

    ## 'matrix mode' to reverse the y-axis
    if (strcmp (ax, "ij"))
      set (ca, "ydir", "reverse");
    elseif (strcmp (ax, "xy"))
      set (ca, "ydir", "normal");

      ## aspect ratio
    elseif (strcmp (ax, "image"))
      set (ca, "dataaspectratio", [1, 1, 1]);
      ## FIXME should be the same as "tight"
      set (ca, "xlimmode", "auto", "ylimmode", "auto", "zlimmode", "auto");
    elseif (strcmp (ax, "equal") || strcmp (ax, "square"))
      set (ca, "dataaspectratio", [1, 1, 1]);
    elseif (strcmp (ax, "normal"))
      set (ca, "dataaspectratiomode", "auto");

      ## axis limits
    elseif (len >= 4 && strcmp (ax(1:4), "auto"))
      if (len > 4)
	if (any (ax == "x"))
	  set (ca, "xlimmode", "auto");
	endif
	if (any (ax == "y"))
	  set (ca, "ylimmode", "auto");
	endif
	if (any (ax == "z"))
	  set (ca, "zlimmode", "auto");
	endif
      else
	set (ca, "xlimmode", "auto", "ylimmode", "auto", "zlimmode", "auto");
      endif
    elseif (strcmp (ax, "manual"))
      ## fixes the axis limits, like axis(axis) should;
      set (ca, "xlimmode", "manual", "ylimmode", "manual", "zlimmode", "manual");
    elseif (strcmp (ax, "tight"))
      ## FIXME if tight, plot must set ranges to limits of the
      ## all the data on the current plot, even if from a previous call.
      ## Instead, just let gnuplot do as it likes.
      set (ca, "xlimmode", "auto", "ylimmode", "auto", "zlimmode", "auto");

      ## tic marks
    elseif (strcmp (ax, "on") || strcmp (ax, "tic"))
      set (ca, "xtickmode", "auto", "ytickmode", "auto", "ztickmode", "auto");
      set (ca, "xticklabelmode", "auto", "yticklabelmode", "auto",
	   "zticklabelmode", "auto");
      set (ca, "visible", "on");
    elseif (strcmp (ax, "off"))
      set (ca, "xtick", [], "ytick", [], "ztick", []);
      set (ca, "visible", "off");
    elseif (len > 3 && strcmp (ax(1:3), "tic"))
      if (any (ax == "x"))
	set (ca, "xtickmode", "auto");
      else
	set (ca, "xtick", []);
      endif
      if (any (ax == "y"))
	set (ca, "ytickmode", "auto");
      else
	set (ca, "ytick", []);
      endif
      if (any (ax == "z"))
	set (ca, "ztickmode", "auto");
      else
	set (ca, "ztick", []);
      endif
    elseif (strcmp (ax, "label"))
      set (ca, "xticklabelmode", "auto", "yticklabelmode", "auto",
	   "zticklabelmode", "auto");
    elseif (strcmp (ax, "nolabel"))
      set (ca, "xticklabel", "", "yticklabel", "", "zticklabel", "");
    elseif (len > 5 && strcmp (ax(1:5), "label"))
      if (any (ax == "x"))
	set (ca, "xticklabelmode", "auto");
      else
	set (ca, "xticklabel", "");
      endif
      if (any (ax == "y"))
	set (ca, "yticklabelmode", "auto");
      else
	set (ca, "yticklabel", "");
      endif
      if (any (ax == "z"))
	set (ca, "zticklabelmode", "auto");
      else
	set (ca, "zticklabel", "");
      endif

    else
      warning ("unknown axis option '%s'", ax);
    endif

  elseif (isvector (ax))

    len = length (ax);

    if (len != 2 && len != 4 && len != 6)
      error ("axis: expecting vector with 2, 4, or 6 elements");
    endif

    for i = 1:2:len
      if (ax(i) == ax(i+1))
	error ("axis: limits(%d) cannot equal limits(%d)", i, i+1);
      endif
    endfor

    if (len > 1)
      set (ca, "xlim", [ax(1), ax(2)]);
    endif

    if (len > 3)
      set (ca, "ylim", [ax(3), ax(4)]);
    endif

    if (len > 5)
      set (ca, "zlim", [ax(5), ax(6)]);
    endif

  else
    error ("axis: expecting no args, or a vector with 2, 4, or 6 elements");
  endif

  if (nargin > 1)
    axis (varargin{:});
  endif

endfunction

%!demo
%! t=0:0.01:2*pi; x=sin(t);
%!
%! subplot(221);    title("normal plot");
%! plot(t, x);
%!
%! subplot(222);    title("square plot");
%! axis("square");  plot(t, x);
%!
%! subplot(223);    title("equal plot");
%! axis("equal");   plot(t, x);
%! 
%! subplot(224);    title("normal plot again");
%! axis("normal");  plot(t, x);

%!demo
%! t=0:0.01:2*pi; x=sin(t);
%!
%! subplot(121);   title("ij plot");
%! axis("ij");     plot(t, x);
%!
%! subplot(122);   title("xy plot");
%! axis("xy");     plot(t, x);

%!demo
%! t=0:0.01:2*pi; x=sin(t);
%!
%! subplot(331);   title("x tics & labels");
%! axis("ticx");   plot(t, x);
%!
%! subplot(332);   title("y tics & labels");
%! axis("ticy");   plot(t, x);
%!
%! subplot(334);     title("x & y tics, x labels");
%! axis("labelx","tic");   plot(t, x);
%!
%! subplot(335);     title("x & y tics, y labels");
%! axis("labely","tic");   plot(t, x);
%!
%! subplot(337);     title("x tics, no labels");
%! axis("nolabel","ticx");   plot(t, x);
%!
%! subplot(338);     title("y tics, no labels");
%! axis("nolabel","ticy");   plot(t, x);
%!
%! subplot(333);     title("no tics or labels");
%! axis("off");    plot(t, x);
%!
%! subplot(336);     title("all tics but no labels");
%! axis("nolabel","tic");    plot(t, x);
%!
%! subplot(339);     title("all tics & labels");
%! axis("on");       plot(t, x);

%!demo
%! t=0:0.01:2*pi; x=sin(t);
%!
%! subplot(321);    title("axes at [0 3 0 1]")
%! axis([0,3,0,1]); plot(t, x);
%!
%! subplot(322);    title("auto");
%! axis("auto");    plot(t, x);
%!
%! subplot(323);    title("manual");
%! plot(t, x, ";sine [0:2pi];"); hold on;
%! axis("manual");
%! plot(-3:3,-3:3, ";line (-3,-3)->(3,3);"); hold off;
%!
%! subplot(324);    title("axes at [0 3 0 1], then autox");
%! axis([0,3,0,1]); axis("autox");
%! plot(t, x, ";sine [0:2pi];");
%!
%! subplot(325);    title("axes at [3 6 0 1], then autoy");
%! axis([3,6,0,1]); axis("autoy");
%! plot(t, x, ";sine [0:2p];");
%!
%! subplot(326);    title("tight");
%! axis("tight");   plot(t, x);
%! % The last plot should not have any whitespace outside the data
%! % limits, but "tight" isn't implemented yet.
