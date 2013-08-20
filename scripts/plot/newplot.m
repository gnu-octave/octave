## Copyright (C) 2005-2012 John W. Eaton
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
## @deftypefn  {Function File} {} newplot ()
## @deftypefnx {Function File} {} newplot (@var{hfig})
## @deftypefnx {Function File} {} newplot (@var{hax})
## @deftypefnx {Function File} {@var{hax} =} newplot (@dots{})
## Prepare graphics engine to produce a new plot.
##
## This function is called at the beginning of all high-level plotting
## functions.  It is not normally required in user programs.  @code{newplot}
## queries the @qcode{"NextPlot"} field of the current figure and axis to
## determine what to do.
##
## @multitable @columnfractions .25 .75
## @headitem Figure NextPlot @tab Action
## @item @qcode{"new"} @tab Create a new figure and make it the current figure.
##
## @item @qcode{"add"} (default) @tab Add new graphic objects to the current figure.
##
## @item @qcode{"replacechildren"} @tab Delete child objects whose HandleVisibility is
## set to @qcode{"on"}.  Set NextPlot property to @qcode{"add"}.  This
## typically clears a figure, but leaves in place hidden objects such as
## menubars.  This is equivalent to @code{clf}.
##
## @item @qcode{"replace"} @tab Delete all child objects of the figure and
## reset all figure properties to their defaults.  However, the following
## four properties are not reset: Position, Units, PaperPosition, PaperUnits.
##  This is equivalent to @code{clf reset}.
## @end multitable
##
## @multitable @columnfractions .25 .75
## @headitem Axis NextPlot @tab Action
## @item @qcode{"add"} @tab Add new graphic objects to the current axes.  This is
## equivalent to @code{hold on}.
##
## @item @qcode{"replacechildren"} @tab Delete child objects whose HandleVisibility is
## set to @qcode{"on"}, but leave axis properties unmodified.  This typically
## clears a plot, but preserves special settings such as log scaling for
## axes.  This is equivalent to @code{cla}.
##
## @item @qcode{"replace"} (default) @tab Delete all child objects of the
## axis and reset all axis properties to their defaults.  However, the
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
## current axis.
## @end deftypefn

## FIXME: The Matlab function takes an optional list of file handles, hsave,
##        which are not deleted when the figure and axes are prepared.
##        I'm sure there is a good reason for that, but coding such
##        compatibility is really tricky and doesn't serve much purpose since
##        newplot is nearly exclusively used by Octave's internal plotting
##        functions.  In Octave's case the argument is almost always null,
##        or occasionally the axis handle to plot into.

function hax = newplot (hsave = [])

  if (nargin > 1)
    print_usage ();
  endif

  cf = [];
  ca = [];

  if (! isempty (hsave))
    ## Find the first valid axes 
    ca = ancestor (hsave, "axes", "toplevel"); 
    ca = ca(find (ca, 1));
    ## Next, find the figure associated with any axis found
    if (! isempty (ca))
      cf = ancestor (ca, "figure", "toplevel");
    else
      cf = ancestor (hsave, "figure", "toplevel"); 
      cf = cf(find (cf, 1));
    endif
  endif

  if (isempty (cf))
    ## get current figure, or create a new one if necessary
    cf = gcf ();
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
      kids = allchild (cf);
      if (! isempty (ca))
        kids(kids == ca) = [];
      endif
      delete (kids);
      reset (cf);
  endswitch
  set (cf, "nextplot", "add");  # Matlab compatibility

  if (isempty (ca))
    ca = gca ();
  else
    set (cf, "currentaxes", ca);
  endif

  ## FIXME: Is this necessary anymore?
  ##        It seems like a kluge that belongs somewhere else.
  if (strcmp (get (ca, "__hold_all__"), "off"))
    __next_line_color__ (true);
    __next_line_style__ (true);
  else
    __next_line_color__ (false);
    __next_line_style__ (false);
  endif

  anp = get (ca, "nextplot");
  switch (anp)
    case "add"
      ## Default case.  Doesn't require action.
    case "replacechildren"
      delete (get (ca, "children"));
    case "replace"
      __go_axes_init__ (ca, "replace");
      __request_drawnow__ ();
      ## FIXME: The code above should perform the following:
      ###########################
      ## delete (allchild (ca));
      ## reset (ca);
      ###########################
      ## Actually, __go_axes_init__ does both less and more.
      ## It doesn't really remove all children since it re-instantiates
      ## xlabel, ylabel, zlabel, and title text objects.
      ## Also it preserves font properties like fontsize.
      ## For the time being, in order to have axis labels and title work,
      ## the above code is is required.
  endswitch

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

