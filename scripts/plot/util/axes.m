########################################################################
##
## Copyright (C) 2005-2023 The Octave Project Developers
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
## @deftypefn  {} {} axes ()
## @deftypefnx {} {} axes (@var{property}, @var{value}, @dots{})
## @deftypefnx {} {} axes (@var{hpar}, @var{property}, @var{value}, @dots{})
## @deftypefnx {} {} axes (@var{hax})
## @deftypefnx {} {@var{h} =} axes (@dots{})
## Create a Cartesian axes object and return a handle to it, or set the current
## axes to @var{hax}.
##
## Called without any arguments, or with @var{property}/@var{value} pairs,
## construct a new axes.  The optional argument @var{hpar} is a graphics handle
## specifying the parent for the new axes and may be a figure, uipanel, or
## uitab.
##
## Called with a single axes handle argument @var{hax}, the function makes
## @var{hax} the current axes (as returned by @code{gca}).  It also makes
## the figure which contains @var{hax} the current figure (as returned by
## @code{gcf}).  Finally, it restacks the parent object's @code{children}
## property so that the axes @var{hax} appears before all other axes handles
## in the list.  This causes @var{hax} to be displayed on top of any other axes
## objects (Z-order stacking).  In addition it restacks any legend or colorbar
## objects associated with @var{hax} so that they are also visible.
##
## Programming Note: The full list of properties is documented at
## @ref{Axes Properties}.
## @seealso{gca, set, get}
## @end deftypefn

function h = axes (varargin)

  htmp = hpar = [];
  if (nargin > 0)
    if (ishghandle (varargin{1}(1)))
      htmp = varargin{1};
      if (! isscalar (htmp))
        error ("axes: H must be a scalar handle");
      endif
      typ = get (htmp, "type");
      if (strcmp (typ, "axes") && nargin == 1)
        cf = ancestor (htmp, "figure");
        if (__is_handle_visible__ (htmp))
          set (0, "currentfigure", cf);
          set (cf, "currentaxes", htmp);
        endif
        restack_axes (htmp, get (htmp, "parent"));

        if (nargout > 0)
          h = htmp;
        endif
        return;

      elseif (any (strcmp (typ, {"figure", "uipanel", "uitab"})))
        hpar = htmp;
        htmp = [];
        varargin(1) = [];

      else
        error ("axes: H must be a handle to an axes or container");
      endif
    endif
  endif

  if (isempty (hpar))
    ## Find a parent if not given as first argument.
    idx = find (strcmpi (varargin(1:2:end), "parent"), 1, "first");
    if (! isempty (idx) && numel (varargin) >= 2*idx)
      hpar = varargin{2*idx};
      varargin([2*idx-1, 2*idx]) = [];
    else
      hpar = gcf ();
    endif
  endif

  ## If there is an annotation axes currently on top of the children stack,
  ## then it must be placed back on top.
  ## FIXME: It may be necessary to keep uiXXX objects above all axes objects
  ##        including even the transparent scribe axes layer.
  ch = allchild (hpar);
  h_annotation = ch(strcmp (get (ch, "tag"), "scribeoverlay"));

  ## Create an axes object.
  htmp = __go_axes__ (hpar, varargin{:});
  if (__is_handle_visible__ (htmp))
    set (ancestor (hpar, "figure"), "currentaxes", htmp);
  endif

  ## Restack annotation object if necessary
  if (! isempty (h_annotation))
    ## FIXME: This will put annotation layer first, above even uicontrol
    ## objects.  May need to keep annotation layer above all axes only.
    shh = get (0, "ShowHiddenHandles");
    unwind_protect
      set (0, "ShowHiddenHandles", "on");
      ch(ch == h_annotation) = htmp;
      ch = [h_annotation; ch];
      set (hpar, "children", ch);
    unwind_protect_cleanup
      set (0, "ShowHiddenHandles", shh);
    end_unwind_protect
  endif

  if (nargout > 0)
    h = htmp;
  endif

endfunction

function restack_axes (h, hpar)

  shh = get (0, "ShowHiddenHandles");
  unwind_protect
    set (0, "ShowHiddenHandles", "on");
    ch = get (hpar, "children");
    axidx = strcmp (get (ch, "type"), "axes");
    ## Strip out any annotation axes layer, unless h itself is annotation axes.
    if (! strcmp (get (h, "tag"), "scribeoverlay"))
      axidx(axidx) = ! strcmp (get (ch(axidx), "tag"), "scribeoverlay");
    endif
    hax = ch(axidx);  # List of axes

    ## Find and stack any legend belonging to this axes above this axes.
    try
      hleg = get (h, "__legend_handle__");
    catch
      hleg = false;
    end_try_catch

    ## Find and stack any colorbar belonging to this axes above this axes.
    try
      hcb = get (h, "__colorbar_handle__");
    catch
      hcb = false;
    end_try_catch

    ## Preserve order of colorbars and legends above this axes
    if (hleg || hcb)
      if (hleg && ! hcb)
        h = [hleg; h];
      elseif (hcb && ! hleg)
        h = [hcb; h];
      else
        hcb_idx = find (hcb == hax);
        hleg_idx = find (hleg == hax);
        if (hleg_idx < hcb_idx)
          h = [hleg; hcb; h];
        else
          h = [hcb; hleg; h];
        endif
      endif
    endif

    ## FIXME: ismember call is very slow (2/3rds of runtime for function)
    ch(axidx) = [h; hax(! ismember (hax, h))];
    set (hpar, "children", ch);

  unwind_protect_cleanup
    set (0, "ShowHiddenHandles", shh);
  end_unwind_protect

endfunction


## FIXME: These demos actually just show how axes objects behave.
##        They do not show how the axes() function itself works.
%!demo
%! clf;
%! x = -10:10;
%! plot (x,x, x,-x);
%! set (gca, "yscale", "log");
%! legend ({"x >= 1", "x <= 1"}, "location", "north");
%! title ({"log axes discard negative data", "ylim = [1, 10]"});

%!demo
%! clf;
%! x = -10:0.1:10;
%! y = sin (x)./(1 + abs (x)) + 0.1*x - 0.4;
%! plot (x, y);
%! set (gca, "xaxislocation", "origin");
%! set (gca, "yaxislocation", "origin");
%! box off;
%! title ({"no plot box", "xaxislocation = origin, yaxislocation = origin"});

%!demo
%! clf;
%! x = -10:0.1:10;
%! y = sin (x)./(1+abs (x)) + 0.1*x - 0.4;
%! plot (x, y);
%! set (gca, "xaxislocation", "origin");
%! set (gca, "yaxislocation", "left");
%! box off;
%! title ({"no plot box", "xaxislocation = origin, yaxislocation = left"});

%!demo
%! clf;
%! x = -10:0.1:10;
%! y = sin (x)./(1+abs (x)) + 0.1*x - 0.4;
%! plot (x, y);
%! title ("no plot box");
%! set (gca, "xaxislocation", "origin");
%! set (gca, "yaxislocation", "right");
%! box off;
%! title ({"no plot box", "xaxislocation = origin, yaxislocation = right"});

%!demo
%! clf;
%! x = -10:0.1:10;
%! y = sin (x)./(1+abs (x)) + 0.1*x - 0.4;
%! plot (x, y);
%! set (gca, "xaxislocation", "bottom");
%! set (gca, "yaxislocation", "origin");
%! box off;
%! title ({"no plot box", "xaxislocation = bottom, yaxislocation = origin"});

%!demo
%! clf;
%! x = -10:0.1:10;
%! y = sin (x)./(1+abs (x)) + 0.1*x - 0.4;
%! plot (x, y);
%! set (gca, "xaxislocation", "top");
%! set (gca, "yaxislocation", "origin");
%! box off;
%! title ({"no plot box", "xaxislocation = top, yaxislocation = origin"});

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax1 = axes ("tag", "axes1");
%!   plot (1:10, "b");
%!   hleg1 = legend ("leg1", "location", "east");
%!   hcb1 = colorbar ("location", "east");
%!   hanno = annotation ("arrow");
%!   hscribe = get (hanno, "parent");
%!
%!   hax2 = axes ("tag", "axes2");
%!   plot (10:-1:1, "r");
%!   hcb2 = colorbar ("location", "east");
%!   hleg2 = legend ("hax2");
%!
%!   ## Verify base configuration
%!   ch = allchild (hf);
%!   hax = ch(isaxes (ch));
%!   hax1pos = find (hax1 == hax);
%!   hax2pos = find (hax2 == hax);
%!   hleg1pos = find (hleg1 == hax);
%!   hleg2pos = find (hleg2 == hax);
%!   hcb1pos = find (hcb1 == hax);
%!   hcb2pos = find (hcb2 == hax);
%!   hscribepos = find (hscribe == hax);
%!
%!   assert (all (hscribepos < ...
%!                [hax1pos, hax2pos, hleg1pos, hleg2pos, hcb1pos, hcb2pos]));
%!   assert (hax2pos < hax1pos);
%!   assert (hleg2pos < hcb2pos && hcb2pos < hax2pos);
%!   assert (hcb1pos < hleg1pos && hleg1pos < hax1pos);
%!
%!   ## Restack axes1 on top
%!   axes (hax1);
%!   ch = allchild (hf);
%!   hax = ch(isaxes (ch));
%!   hax1pos = find (hax1 == hax);
%!   hax2pos = find (hax2 == hax);
%!   hleg1pos = find (hleg1 == hax);
%!   hleg2pos = find (hleg2 == hax);
%!   hcb1pos = find (hcb1 == hax);
%!   hcb2pos = find (hcb2 == hax);
%!   hscribepos = find (hscribe == hax);
%!
%!   assert (all (hscribepos < ...
%!                [hax1pos, hax2pos, hleg1pos, hleg2pos, hcb1pos, hcb2pos]));
%!   assert (hax1pos < hax2pos);
%!   assert (hcb1pos < hleg1pos && hleg1pos < hax1pos);
%!   assert (hleg2pos < hcb2pos && hcb2pos < hax2pos);
%!
%!   ## Restack axes2 on top
%!   axes (hax2);
%!   ch = allchild (hf);
%!   hax = ch(isaxes (ch));
%!   hax1pos = find (hax1 == hax);
%!   hax2pos = find (hax2 == hax);
%!   hleg1pos = find (hleg1 == hax);
%!   hleg2pos = find (hleg2 == hax);
%!   hcb1pos = find (hcb1 == hax);
%!   hcb2pos = find (hcb2 == hax);
%!   hscribepos = find (hscribe == hax);
%!
%!   assert (all (hscribepos < ...
%!                [hax1pos, hax2pos, hleg1pos, hleg2pos, hcb1pos, hcb2pos]));
%!   assert (hax2pos < hax1pos);
%!   assert (hleg2pos < hcb2pos && hcb2pos < hax2pos);
%!   assert (hcb1pos < hleg1pos && hleg1pos < hax1pos);
%!
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!error <H must be a scalar handle> axes ([0, 0])
%!error <H must be a handle to an axes or container> axes (0)
