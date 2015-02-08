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
## @deftypefn  {Function File} {} axes ()
## @deftypefnx {Function File} {} axes (@var{property}, @var{value}, @dots{})
## @deftypefnx {Function File} {} axes (@var{hax})
## @deftypefnx {Function File} {@var{h} =} axes (@dots{})
## Create an axes object and return a handle to it, or set the current
## axes to @var{hax}.
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
## @seealso {gca, set, get}
## @end deftypefn

## Author: jwe

function h = axes (varargin)

  if (nargin == 0 || nargin > 1)
    ## Create an axes object.
    idx = find (strcmpi (varargin(1:2:end), "parent"), 1, "first");
    if (! isempty (idx) && length (varargin) >= 2*idx)
      cf = varargin{2*idx};
      varargin([2*idx-1, 2*idx]) = [];
    else
      cf = gcf ();
    endif
    htmp = __go_axes__ (cf, varargin{:});
    if (__is_handle_visible__ (htmp))
      set (ancestor (cf, "figure"), "currentaxes", htmp);
    endif
  else
    ## ARG is axes handle.
    htmp = varargin{1};
    if (isscalar (htmp) && isaxes (htmp))
      if (__is_handle_visible__ (htmp))
        cf = ancestor (htmp, "figure");
        set (0, "currentfigure", cf);
        set (cf, "currentaxes", htmp);

        ## restack
        ch = get (cf, "children")(:);
        idx = (ch == htmp);
        ch = [ch(idx); ch(!idx)];
        set (cf, "children", ch);
      endif
    else
      error ("axes: H must be a scalar axes handle");
    endif
  endif
  
  ## FIXME: In order to have the overlay axes on top of all other axes
  ##        we restack the figure children. Does Matlab use a similar
  ##        hack?
  show = get (0, "showhiddenhandles");
  set (0, "showhiddenhandles", "on");
  unwind_protect
    ch = get (cf, "children");
    idx = strcmp (get (ch, "tag"), "scribeoverlay");
    hover = ch(idx);
    if (! isempty (hover))
      hax = ch(isaxes (ch));
      if (numel (hax) > 1)
        ch(isaxes (ch)) = [hover; hax(hax != hover)];
        set (cf, "children", ch);
      endif
    endif
  unwind_protect_cleanup
    set (0, "showhiddenhandles", show);
  end_unwind_protect

  if (nargout > 0)
    h = htmp;
  endif

endfunction

