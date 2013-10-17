## Copyright (C) 2001-2012 Paul Kienzle
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
## @deftypefn  {Function File} {} orient (@var{orientation})
## @deftypefnx {Function File} {} orient (@var{hfig}, @var{orientation})
## @deftypefnx {Function File} {@var{orientation} =} orient ()
## @deftypefnx {Function File} {@var{orientation} =} orient (@var{hfig})
## Query or set the default print orientation.
##
## Valid values for @var{orientation} are @qcode{"landscape"},
## @qcode{"portrait"}, and @qcode{"tall"}.
##
## The @qcode{"tall"} option sets the orientation to portrait and fills
## the page with the plot, while leaving a 0.25 inch border.
##
## When called with no arguments, return the default print orientation.
##
## If the first argument @var{hfig} is a figure handle, then operate on this
## figure rather than the current figure returned by @code{gcf}.
## @seealso{print, saveas}
## @end deftypefn

## Author: Paul Kienzle
## Adapted-By: jwe

function retval = orient (varargin)

  nargs = nargin;

  if (nargs > 0 && numel (varargin{1}) == 1 && isfigure (varargin{1}))
    cf = varargin{1};
    varargin(1) = [];
    nargs--;
  else
    cf = gcf ();
  endif

  if (nargs == 0)
    retval = get (cf, "paperorientation");
  elseif (nargin == 1)
    orientation = varargin{1};
    if (strcmpi (orientation, "landscape") || strcmpi (orientation, "portrait"))
      if (! strcmpi (get (cf, "paperorientation"), orientation))
        ## FIXME: with the proper listeners in place there won't be a need to
        ##        set the papersize and paperpostion here.
        papersize = get (cf, "papersize");
        paperposition = get (cf, "paperposition");
        set (cf, "paperorientation", orientation);
        set (cf, "papersize", papersize([2, 1]));
        set (cf, "paperposition", paperposition([2, 1, 4, 3]));
      endif
      ## landscape also sets the plot to occupy the entire page
      if (strcmpi (orientation, "landscape"))
        papersize = get (cf, "papersize");
        set (cf, "paperposition", [0.25, 0.25, (papersize - 0.5)]);
      endif
    elseif (strcmpi (varargin{1}, 'tall'))
      orient ("portrait");
      papersize = get (cf, "papersize");
      set (cf, "paperposition", [0.25, 0.25, (papersize - 0.5)]);
    else
      error ("orient: unknown ORIENTATION");
    endif
  else
    print_usage ();
  endif

endfunction


%!shared papersize, paperposition, tallpaperposition, hfig
%! papersize = [8.5, 11];
%! paperposition = [0.25, 2.5, 8, 6];
%! tallpaperposition = [0.25, 0.25, (papersize-0.5)];
%! hfig = figure ("visible", "off");
%! set (hfig, "paperorientation", "portrait");
%! set (hfig, "papersize", papersize);
%! set (hfig, "paperposition", paperposition);

%!test
%! orient portrait;
%! assert (orient, "portrait")   # default
%! assert (get (hfig, "papersize"), papersize);
%! assert (get (hfig, "paperposition"), paperposition);

%!test
%! orient landscape;
%! assert (orient,"landscape")   # change to landscape
%! assert (get (hfig, "papersize"), papersize([2, 1]));
%! assert (get (hfig, "paperposition"), paperposition([2, 1, 4, 3]));

%!test
%! orient portrait   # change back to portrait
%! assert (orient, "portrait");
%! assert (get (hfig, "papersize"), papersize);
%! assert (get (hfig, "paperposition"), paperposition);

%!test
%! orient landscape;
%! orient tall;
%! assert (orient, "portrait");
%! assert (get (hfig, "papersize"), papersize);
%! assert (get (hfig, "paperposition"), tallpaperposition);

%!fail ("orient ('nobody')", "unknown ORIENTATION")

%!test
%! orient portrait   # errors don't change the state
%! assert (orient, "portrait");
%! assert (get (hfig, "papersize"), papersize);
%! assert (get (hfig, "paperposition"), tallpaperposition);

%!test
%! close (hfig);

