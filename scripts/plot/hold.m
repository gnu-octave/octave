## Copyright (C) 2005, 2006, 2007, 2008, 2009 John W. Eaton
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
## @deftypefn  {Function File} {} hold
## @deftypefnx {Function File} {} hold @var{state}
## @deftypefnx {Function File} {} hold (@var{hax}, @dots{})
## Toggle or set the 'hold' state of the plotting engine which determines
## whether new graphic objects are added to the plot or replace the existing
## objects.  
## 
## @table @code
## @item hold on
## Retain plot data and settings so that subsequent plot commands are displayed
## on a single graph.
##
## @item hold off
## Clear plot and restore default graphics settings before each new plot
## command.  (default).
##
## @item hold
## Toggle the current 'hold' state.
## @end table
## 
## When given the additional argument @var{hax}, the hold state is modified
## only for the given axis handle.
##
## To query the current 'hold' state use the @code{ishold} function.
## @seealso{ishold, cla, newplot, clf}
## @end deftypefn

function hold (varargin)

  if (nargin > 0 && numel (varargin{1}) == 1 && ishandle (varargin{1}(1))
      && strcmp (get (varargin{1}, "type"), "axes"))
    [h, varargin, nargs] = __plt_get_axis_arg__ ("hold", varargin{:});
  elseif (nargin > 0 && numel (varargin{1}) > 1 && ishandle (varargin{1}(1)))
    print_usage ();
  else
    h = gcf ();
    nargs = numel (varargin);
  endif

  hold_state = get (h, "nextplot");

  if (nargs == 0)
    if (strcmpi (hold_state, "add"))
      hold_state = "replace";
    else
      hold_state = "add";
    endif
  elseif (nargs == 1)
    state = varargin{1};
    if (ischar (state))
      if (strcmpi (state, "off"))
	hold_state = "replace";
      elseif (strcmpi (state, "on"))
	hold_state = "add";
      else
	print_usage ();
      endif
    endif
  else
    print_usage ();
  endif

  if (isfigure (h))
    if (isempty (get (h, "currentaxes")))
      set (h, "currentaxes", __go_axes__ (h))
    endif
    axes_objs = findobj (h, "type", "axes");
    h = [h; axes_objs];
  endif

  set (h, "nextplot", hold_state);

endfunction

%!demo
%! clf
%! A = rand (100);
%! [X, Y] = find (A > 0.9);
%! imshow (A)
%! hold on
%! plot (X, Y, 'o')
%! hold off

%!demo
%! clf
%! hold on
%! imagesc(1./hilb(4));
%! plot (1:4, "-s")
%! hold off

%!demo
%! clf
%! hold on
%! imagesc(1./hilb(2));
%! imagesc(1./hilb(4));
%! hold off

%!demo
%! clf
%! hold on
%! plot (1:4, "-s")
%! imagesc(1./hilb(4));
%! hold off

