########################################################################
##
## Copyright (C) 2005-2025 The Octave Project Developers
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
## @deftypefn  {} {} newplot ()
## @deftypefnx {} {} newplot (@var{hfig})
## @deftypefnx {} {} newplot (@var{hax})
## @deftypefnx {} {@var{hax} =} newplot (@dots{})
## Prepare graphics engine to produce a new plot.
##
## This function is called at the beginning of all high-level plotting
## functions.  It is not normally required in user programs.  @code{newplot}
## queries the @qcode{"NextPlot"} field of the current figure and axes to
## determine what to do.
##
## @multitable @columnfractions .25 .75
## @headitem Figure NextPlot @tab Action
## @item @qcode{"add"} (default) @tab Add new graphic objects to the current
## figure.
##
## @item @qcode{"new"} @tab Create a new figure and make it the current figure.
##
## @item @qcode{"replace"} @tab Delete all child objects of the figure and
## reset all figure properties to their defaults.  However, the following
## four properties are not reset: Position, Units, PaperPosition, PaperUnits.
## In addition, the NextPlot property is set to @qcode{"add"} regardless of
## user-defined defaults.  This is equivalent to @code{clf reset}.
##
## @item @qcode{"replacechildren"} @tab Delete child objects whose
## HandleVisibility is set to @qcode{"on"}.  Set NextPlot property to
## @qcode{"add"}.  This typically clears a figure, but leaves in place hidden
## objects such as menubars.  This is equivalent to @code{clf}.
## @end multitable
##
## @multitable @columnfractions .25 .75
## @headitem Axes NextPlot @tab Action
## @item @qcode{"add"} @tab Add new graphic objects to the current axes.  This
## is equivalent to @code{hold on}.
##
## @item @qcode{"replace"} (default) @tab Delete all child objects of the
## axes and reset all axes properties to their defaults.  However, the
## following properties are not reset: Position, Units.  This is equivalent
## to @code{cla reset}.
##
## @c FIXME: Uncomment when "replaceall" property value added to axes
## @c @item @qcode{"replaceall"} @tab For plots with one y-axis this is equivalent
## @c to @qcode{"replace"}.  For plots created with @code{plotyy}, operate on both
## @c axes rather than just the single axes object @code{hax}.
##
## @item @qcode{"replacechildren"} @tab Delete child objects whose
## HandleVisibility is set to @qcode{"on"}, but leave axes properties
## unmodified except for ColorOrderIndex and LineStyleOrderIndex which are set
## to 1.  This typically clears a plot, but preserves special settings such as
## log scaling for axes.  This is equivalent to @code{cla}.
##
## @end multitable
##
## If the optional input @var{hfig} or @var{hax} is given then prepare the
## specified figure or axes rather than the current figure and axes.
##
## The optional return value @var{hax} is a graphics handle to the created
## axes object (not figure).
## @end deftypefn

function hax = newplot (hg = [])

  old_cf = [];
  cf = ca = [];

  if (! isempty (hg))
    if (! isscalar (hg))
      error ("newplot: first argument must be scalar graphics handle");
    endif

    if (isaxes (hg))
      ca = hg;
      cf = ancestor (ca, "figure", "toplevel");
    elseif (isfigure (hg))
      cf = hg;
    else
      error ("newplot: first argument must be an axes or figure handle");
    endif
  endif

  do_reset = true;
  if (isempty (cf))
    ## get current figure, or create a new one if necessary
    old_cf = get (0, "currentfigure");
    if (isempty (old_cf))
      cf = figure ();
      do_reset = false;
    else
      cf = old_cf;
    endif
  else
    ## switch to figure provided without causing other updates
    old_cf = get (0, "currentfigure");
    set (0, "currentfigure", cf);
  endif

  fnp = get (cf, "nextplot");
  switch (fnp)

    case "add"
      ## Default case.  Doesn't require action.

    case "new"
      ## Ordinarily, create a new figure to hold plot.
      ## But, if user has requested preparing a specific axis, then
      ## use the existing figure to hold the requested axis.
      if (isempty (ca))
        cf = figure ();
      endif

    case "replace"
      if (do_reset)
        kids = allchild (cf);
        if (! isempty (ca))
          kids(kids == ca) = [];
        endif
        delete (kids);
        reset (cf);
      endif
      set (cf, "nextplot", "add");  # Matlab compatibility

    case "replacechildren"
      kids = get (cf, "children");
      if (! isempty (ca))
        kids(kids == ca) = [];
      endif
      delete (kids);
      set (cf, "nextplot", "add");  # Matlab compatibility

  endswitch

  do_reset = true;
  if (isempty (ca))
    ca = get (cf, "currentaxes");
    if (isempty (ca))
      ca = axes ();
      do_reset = false;
    endif
  else
    ## FIXME: Does ca need to be re-parented as well?
    set (cf, "currentaxes", ca);
  endif

  anp = get (ca, "nextplot");
  switch (anp)
    case "add"
      ## Default case.  Doesn't require action.

    case "replace"
      if (isprop (ca, "__plotyy_axes__") ...
          && ! any (strcmp({dbstack().name}, "plotyy")))
        ## Hack for bug #44246.  There is no way to reset or remove a property
        ## created with addproperty short of deleting the object.
        old_units = get (ca, "units");
        old_position = get (ca, "position");
        delete (ca);
        ca = axes ("units", old_units, "position", old_position);
      elseif (do_reset)
        rcn = getappdata (ca, "__subplotrcn__");
        delete (allchild (ca));
        reset (ca);
        ## Reinstall listeners for subplot
        if (! isempty (rcn))
          subplot (rcn{:}, ca)
        endif
      endif

    ## FIXME: Uncomment when "replaceall" property value added to axes
    #{
    ## Only difference over "replace" is that both axes of plotyy are cleared
    case "replaceall"
      if (isprop (ca, "__plotyy_axes__") ...
          && ! any (strcmp({dbstack().name}, "plotyy")))
        ## Hack for bug #44246.  There is no way to reset or remove a property
        ## created with addproperty short of deleting the object.
        ca2 = get (ca, '__plotyy_axes__');
        ca2(ca2 == ca) = [];
        old_units = get (ca, "units");
        old_position = get (ca, "position");
        delete (ca);
        ca = axes ("units", old_units, "position", old_position);
        old_units = get (ca2, "units");
        old_position = get (ca2, "position");
        delete (ca2);
        axes ("units", old_units, "position", old_position);
      elseif (do_reset)
        rcn = getappdata (ca, "__subplotrcn__");
        delete (allchild (ca));
        reset (ca);
        ## Reinstall listeners for subplot
        if (! isempty (rcn))
          subplot (rcn{:}, ca)
        endif
      endif
    #}

    case "replacechildren"
      delete (get (ca, "children"));
      set (ca, "colororderindex", 1, "linestyleorderindex", 1);

  endswitch

  ## Restore figure and axes if necessary
  if (! isempty (old_cf))
    set (0, 'currentfigure', old_cf);
    cf = old_cf;
  endif

  if (nargout > 0)
    hax = ca;
  endif

endfunction


%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   p = plot ([0, 1]);
%!   hax = newplot ();
%!   assert (hax, gca);
%!   assert (isempty (get (gca, "children")));
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Test double axes with plotyy
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = axes ("units", "normalized", "position", [0.1, 0.1, 0.8, 0.3]);
%!   plotyy (hax, 1:4, 1:4, 1:4, 4:-1:1);
%!   hax2 = newplot ();
%!   assert (get (hax2, "units"), "normalized");
%!   assert (get (hax2, "position"), [0.1, 0.1, 0.8, 0.3]);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Test that current figure is preserved
%!test
%! hf1 = figure ('visible', 'off');
%! hax1 = axes ();
%! hf2 = figure ('visible', 'off');
%! hax2 = axes;
%! unwind_protect
%!   cf = gcf ();
%!   ca = gca ();
%!   hax = newplot (hax1);
%!   assert (gcf (), cf);
%!   assert (gca (), ca);
%!   assert (hax, hax1);
%! unwind_protect_cleanup
%!   close ([hf1, hf2]);
%! end_unwind_protect
