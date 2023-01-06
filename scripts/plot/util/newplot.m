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
## @item @qcode{"new"} @tab Create a new figure and make it the current figure.
##
## @item @qcode{"add"} (default) @tab Add new graphic objects to the current
## figure.
##
## @item @qcode{"replacechildren"} @tab Delete child objects whose
## HandleVisibility is set to @qcode{"on"}.  Set NextPlot property to
## @qcode{"add"}.  This typically clears a figure, but leaves in place hidden
## objects such as menubars.  This is equivalent to @code{clf}.
##
## @item @qcode{"replace"} @tab Delete all child objects of the figure and
## reset all figure properties to their defaults.  However, the following
## four properties are not reset: Position, Units, PaperPosition, PaperUnits.
## This is equivalent to @code{clf reset}.
## @end multitable
##
## @multitable @columnfractions .25 .75
## @headitem Axes NextPlot @tab Action
## @item @qcode{"add"} @tab Add new graphic objects to the current axes.  This
## is equivalent to @code{hold on}.
##
## @item @qcode{"replacechildren"} @tab Delete child objects whose
## HandleVisibility is set to @qcode{"on"}, but leave axes properties
## unmodified.  This typically clears a plot, but preserves special settings
## such as log scaling for axes.  This is equivalent to @code{cla}.
##
## @item @qcode{"replace"} (default) @tab Delete all child objects of the
## axes and reset all axes properties to their defaults.  However, the
## following properties are not reset: Position, Units.  This is equivalent
## to @code{cla reset}.
## @end multitable
##
## If the optional input @var{hfig} or @var{hax} is given then prepare the
## specified figure or axes rather than the current figure and axes.
##
## The optional return value @var{hax} is a graphics handle to the created
## axes object (not figure).
##
## @strong{Caution:} Calling @code{newplot} may change the current figure and
## current axes.
## @end deftypefn

## FIXME: The Matlab function takes an optional list of file handles, hsave,
##        which are not deleted when the figure and axes are prepared.
##        I'm sure there is a good reason for that, but coding such
##        compatibility is really tricky and doesn't serve much purpose since
##        newplot is nearly exclusively used by Octave's internal plotting
##        functions.  In Octave's case the argument is almost always null,
##        or occasionally the axes handle to plot into.

function hax = newplot (hsave = [])

  cf = [];
  ca = [];

  if (! isempty (hsave))
    ## Find the first valid axes
    ca = ancestor (hsave, "axes", "toplevel");
    if (iscell (ca))
      ca = [ca{:}];
    endif
    ca = ca(find (ca, 1));
    hsave(hsave == ca) = [];
    ## Next, find the figure associated with any axis found
    if (! isempty (ca))
      cf = ancestor (ca, "figure", "toplevel");
    else
      cf = ancestor (hsave, "figure", "toplevel");
      if (iscell (cf))
        cf = [cf{:}];
      endif
      cf = cf(find (cf, 1));
    endif
  endif

  do_reset = true;
  if (isempty (cf))
    ## get current figure, or create a new one if necessary
    cf = get (0, "currentfigure");
    if (isempty (cf))
      cf = figure ();
      do_reset = false;
    endif
  else
    ## switch to figure provided without causing other updates
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
    case "replacechildren"
      kids = get (cf, "children");
      if (! isempty (ca))
        kids(kids == ca) = [];
      endif
      delete (kids);
    case "replace"
      if (do_reset)
        kids = allchild (cf);
        if (! isempty (ca))
          kids(kids == ca) = [];
        endif
        delete (kids);
        reset (cf);
      endif
  endswitch
  set (cf, "nextplot", "add");  # Matlab compatibility

  do_reset = true;
  if (isempty (ca))
    ca = get (cf, "currentaxes");
    if (isempty (ca))
      ca = axes ();
      do_reset = false;
    endif
    deleteall = true;
  else
    set (cf, "currentaxes", ca);
    deleteall = false;
  endif

  anp = get (ca, "nextplot");
  switch (anp)
    case "add"
      ## Default case.  Doesn't require action.
    case "replacechildren"
      if (! deleteall && ca != hsave)
        ## preserve hsave and its parents, uncles, ...
        kids = allchild (ca);
        hkid = hsave;
        while (! any (hkid == kids))
          hkid = get (hkid, "parent");
        endwhile
        kids(kids == hkid) = [];
        delete (kids);
      else
        delete (get (ca, "children"));
      endif
    case "replace"
      if (! deleteall && ca != hsave)
        ## preserve hsave and its parents, uncles, ...
        kids = allchild (ca);
        hkid = hsave;
        while (! any (hkid == kids))
          hkid = get (hkid, "parent");
        endwhile
        kids(kids == hkid) = [];
        delete (kids);
      else
        if (isprop (ca, "__plotyy_axes__") ...
            && ! any (strcmp({dbstack().name}, "plotyy")))
          ## Hack for bug #44246.  There is no way to reset or remove a
          ## property created with addproperty short of deleting the object.
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
      endif
  endswitch

  ## Reset line and color styles when hold is not on
  if (! strcmp (anp, "add"))
    set (ca, "colororderindex", 1, "linestyleorderindex", 1);
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

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = axes ();
%!   hold on;
%!   hg1 = hggroup ();
%!   hg2 = hggroup ("parent", hg1);
%!   li0 = line (1:10, 1:10);
%!   li1 = line (1:10, -1:-1:-10, "parent", hg1);
%!   li2 = line (1:10, sin (1:10), "parent", hg2);
%!   hold off;
%!   newplot (hg2);
%!   assert (ishghandle (li0), false);
%!   assert (get (hax, "children"), hg1);
%!
%!   ## kids are preserved for hggroups
%!   kids = get (hg1, "children");
%!   newplot (hg1);
%!   assert (get (hg1, "children"), kids);
%!
%!   ## preserve objects
%!   newplot (li1);
%!   assert (ishghandle (li1));
%!
%!   ## kids are deleted for axes
%!   newplot (hax);
%!   assert (isempty (get (hax, "children")));
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

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
