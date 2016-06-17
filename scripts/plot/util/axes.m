## Copyright (C) 2005-2015 John W. Eaton
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
## @deftypefn  {} {} axes ()
## @deftypefnx {} {} axes (@var{property}, @var{value}, @dots{})
## @deftypefnx {} {} axes (@var{hax})
## @deftypefnx {} {@var{h} =} axes (@dots{})
## Create an axes object and return a handle to it, or set the current axes
## to @var{hax}.
##
## Called without any arguments, or with @var{property}/@var{value} pairs,
## construct a new axes.  For accepted properties and corresponding
## values, @pxref{XREFset,,set}.
##
## Called with a single axes handle argument @var{hax}, the function makes
## @var{hax} the current axis.  It also restacks the axes in the
## corresponding figure so that @var{hax} is the first entry in the list
## of children.  This causes @var{hax} to be displayed on top of any other
## axes objects (Z-order stacking).
##
## @seealso{gca, set, get}
## @end deftypefn

## Author: jwe

function h = axes (varargin)

  if (nargin == 0 || nargin > 1)
    ## Parent figure
    idx = find (strcmpi (varargin(1:2:end), "parent"), 1, "first");
    if (! isempty (idx) && length (varargin) >= 2*idx)
      cf = varargin{2*idx};
      varargin([2*idx-1, 2*idx]) = [];
    else
      cf = gcf ();
    endif

    ## If there is an annotation axes currently on top of the
    ## figure children stack, then we will put it back on top
    ## FIXME: Should all annotation axes always be put ahead of regular axes?
    do_restack = false;
    ch = allchild (cf);
    hax = ch(isaxes (ch));
    if (! isempty (hax) && strcmp (get (hax(1), "tag"), "scribeoverlay"))
      h_annotation = hax(1);
      do_restack = true;
    endif

    ## Create an axes object.
    htmp = __go_axes__ (cf, varargin{:});
    if (__is_handle_visible__ (htmp))
      set (ancestor (cf, "figure"), "currentaxes", htmp);
    endif

    ## Restack if necessary
    if (do_restack)
      restack_axes (h_annotation, cf);
    endif
  else
    ## ARG is axes handle.
    htmp = varargin{1};
    if (isscalar (htmp) && isaxes (htmp))
      cf = ancestor (htmp, "figure");
      if (__is_handle_visible__ (htmp))
        set (0, "currentfigure", cf);
        set (cf, "currentaxes", htmp);
      endif

      ## restack
      restack_axes (htmp, cf);
    else
      error ("axes: H must be a scalar axes handle");
    endif
  endif

  if (nargout > 0)
    h = htmp;
  endif

endfunction

function restack_axes (h, cf)

  show = get (0, "showhiddenhandles");
  unwind_protect
    set (0, "showhiddenhandles", "on");
    ch = get (cf, "children");
    axidx = isaxes (ch);
    hax = ch(axidx);
    ## Stack the legend associated with this axes on top of the axes itself
    hleg = hax(strcmp (get (hax, "tag"), "legend"));
    if (any (hleg))
      ## Get field "handle" from structure stored in "userdata" property
      if (isscalar (hleg))
        hlegaxes = get (hleg, "userdata").handle;
      else
        hlegaxes = [[get(hleg, "userdata"){:}].handle](:);
      endif
      hleg = hleg(hlegaxes == h);
      h = [hleg; h];
    endif
    ch(axidx) = [h; hax(! ismember (hax, h))];
    set (cf, "children", ch);
  unwind_protect_cleanup
    set (0, "showhiddenhandles", show);
  end_unwind_protect

endfunction


%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax1 = axes ();
%!   plot (1:10, "b");
%!   hleg1 = legend ("hax1");
%!   hax2 = axes ();
%!   plot (10:-1:1, "r");
%!   hleg2 = legend ("hax2");
%!
%!   ch = allchild (hf);
%!   hax = ch(isaxes (ch));
%!   assert (find (hax == hax2) < find (hax == hax1));
%!   assert (find (hax == hleg1) < find (hax == hax1));
%!   assert (find (hax == hleg2) < find (hax == hax2));
%!
%!   axes (hax1);
%!   ch = allchild (hf);
%!   hax = ch(isaxes (ch));
%!   assert (find (hax == hax2) > find (hax == hax1));
%!   assert (find (hax == hleg1) < find (hax == hax1));
%!
%!   axes (hax2);
%!   ch = allchild (hf);
%!   hax = ch(isaxes (ch));
%!   assert (find (hax == hax2) < find (hax == hax1));
%!   assert (find (hax == hleg2) < find (hax == hax2));
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

