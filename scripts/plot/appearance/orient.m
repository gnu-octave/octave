########################################################################
##
## Copyright (C) 2001-2023 The Octave Project Developers
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
## @deftypefn  {} {} orient (@var{orientation})
## @deftypefnx {} {} orient (@var{hfig}, @var{orientation})
## @deftypefnx {} {@var{orientation} =} orient ()
## @deftypefnx {} {@var{orientation} =} orient (@var{hfig})
## Query or set the print orientation for figure @var{hfig}.
##
## Valid values for @var{orientation} are @qcode{"portrait"},
## @qcode{"landscape"}, and @qcode{"tall"}.
##
## The @qcode{"landscape"} option changes the orientation so the plot width
## is larger than the plot height.  The @qcode{"paperposition"} is also
## modified so that the plot fills the page, while leaving a 0.25 inch border.
##
## The @qcode{"tall"} option sets the orientation to @qcode{"portrait"} and
## fills the page with the plot, while leaving a 0.25 inch border.
##
## The @qcode{"portrait"} option (default) changes the orientation so the plot
## height is larger than the plot width.  It also restores the default
## @qcode{"paperposition"} property.
##
## When called with no arguments, return the current print orientation.
##
## If the argument @var{hfig} is omitted, then operate on the current figure
## returned by @code{gcf}.
## @seealso{print, saveas}
## @end deftypefn

function orientation = orient (varargin)

  cf = [];
  if (nargin > 0 && isscalar (varargin{1}) && isfigure (varargin{1}))
    cf = varargin{1};
    varargin(1) = [];
    nargin = nargin - 1;
  endif

  if (nargin > 1)
    print_usage ();
  endif

  if (isempty (cf))
    cf = gcf ();
  endif

  paperunits = get (cf, "paperunits");
  unwind_protect
    set (cf, "paperunits", "inches");  # All Matlab calculations assume inches.

    if (nargin == 0)
      orientation = get (cf, "paperorientation");
      if (strcmp (orientation, "portrait"))
        papersize = get (cf, "papersize");
        paperposition = get (cf, "paperposition");
        if (paperposition == [0.25 0.25 (papersize - 0.5)])
          orientation = "tall";
        endif
      endif
    else
      paporient = varargin{1};
      if (strcmpi (paporient, "landscape")
          || strcmpi (paporient, "portrait"))
        if (! strcmpi (get (cf, "paperorientation"), paporient))
          ## FIXME: with the proper listeners in place there won't be a need to
          ##        set the papersize and paperposition here.
          papersize = get (cf, "papersize");
          paperposition = get (cf, "paperposition");
          set (cf, "paperorientation", paporient,
                   "papersize", papersize([2, 1]),
                   "paperposition", paperposition([2, 1, 4, 3]));
        endif
        if (strcmpi (paporient, "portrait"))
          ## portrait restores the default
          ## FIXME: Should use "default" here, but Octave complains
          ##        that "paperposition" is not a default property.
          set (cf, "paperposition", "factory");
        else
          ## landscape also sets the plot to occupy the entire page
          papersize = get (cf, "papersize");
          set (cf, "paperposition", [0.25, 0.25, (papersize - 0.5)]);
        endif
      elseif (strcmpi (paporient, "tall"))
        orient ("portrait");
        papersize = get (cf, "papersize");
        set (cf, "paperposition", [0.25, 0.25, (papersize - 0.5)]);
      else
        error ("orient: unknown ORIENTATION");
      endif
    endif

  unwind_protect_cleanup
    set (cf, "paperunits", paperunits);
  end_unwind_protect

endfunction


%!test
%! papersize = [8.5, 11];
%! paperposition = [1.342185258002766, 3.319138943502075, 5.815629483994468, 4.361722112995850];
%! fullpaperposition = [0.25, 0.25, (papersize-0.5)];
%! hfig = figure ("visible", "off");
%! unwind_protect
%!   set (hfig, "paperunits", "inches");
%!   set (hfig, "paperorientation", "portrait");
%!   set (hfig, "papersize", papersize);
%!   set (hfig, "paperposition", paperposition);
%!
%!   orient portrait;
%!   assert (orient, "portrait");  # default
%!   assert (get (hfig, "papersize"), papersize);
%!   assert (get (hfig, "paperposition"), paperposition);
%!
%!   orient landscape;
%!   assert (orient,"landscape");  # change to landscape
%!   assert (get (hfig, "papersize"), papersize([2, 1]));
%!   assert (get (hfig, "paperposition"), fullpaperposition([1, 2, 4, 3]));
%!
%!   orient portrait   # change back to portrait
%!   assert (orient, "portrait");
%!   assert (get (hfig, "papersize"), papersize);
%!   assert (get (hfig, "paperposition"), paperposition);
%!
%!   orient landscape;
%!   orient tall;
%!   assert (orient, "tall");
%!   assert (get (hfig, "papersize"), papersize);
%!   assert (get (hfig, "paperposition"), fullpaperposition);
%!
%!   orient portrait   # errors don't change the state
%!   assert (orient, "portrait");
%!   assert (get (hfig, "papersize"), papersize);
%!   assert (get (hfig, "paperposition"), paperposition);
%! unwind_protect_cleanup
%!   close (hfig);
%! end_unwind_protect

## Test input validation
%!error orient (1.73, 2.5)
%!error <unknown ORIENTATION>
%! hfig = figure ("visible", "off");
%! unwind_protect
%!   orient ("nobody");
%! unwind_protect_cleanup
%!   close (hfig);
%! end_unwind_protect
