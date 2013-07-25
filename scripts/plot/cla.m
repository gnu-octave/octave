## Copyright (C) 2008-2012 Ben Abbott
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
## @deftypefn  {Function File} {} cla
## @deftypefnx {Function File} {} cla reset
## @deftypefnx {Function File} {} cla (@var{hax})
## @deftypefnx {Function File} {} cla (@var{hax}, "reset")
## Clear the current axes by deleting child graphic objects with visible
## handles (HandleVisibility = "on").
##
## If the optional argument "reset" is specified, delete all child objects
## including those with hidden handles and reset all axis properties to
## their defaults.  However, the following properties are not reset:
## Position, Units.
##
## If an axes object handle @var{hax} is specified, operate on it instead of
## the current axes.
## @seealso{clf}
## @end deftypefn

## Author: Ben Abbott <bpabbott@mac.com>
## Created: 2008-10-03

function cla (varargin)

  if (nargin > 2)
    print_usage ();
  elseif (nargin == 0)
    hax = gca;
    do_reset = false;
  elseif (nargin == 1)
    if (isscalar (varargin{1}) && ishandle (varargin{1})
        && strcmp (get (varargin{1}, "type"), "axes"))
      hax = varargin{1};
      do_reset = false;
    elseif (ischar (varargin{1}) && strcmpi (varargin{1}, "reset"))
      hax = gca;
      do_reset = true;
    else
      print_usage ();
    endif
  else
    if (isscalar (varargin{1}) && ishandle (varargin{1})
        && strcmp (get (varargin{1}, "type"), "axes")
        && ischar (varargin{2}) && strcmpi (varargin{2}, "reset"))
      hax = varargin{1};
      do_reset = true;
    else
      print_usage ();
    endif
  endif

  if (! do_reset)
    delete (get (hax, "children"));
  else
    __go_axes_init__ (hax, "replace");
    __request_drawnow__ ();
  endif

endfunction


%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   plot (1:10);
%!   cla ();
%!   kids = get (gca, "children");
%!   cla ();
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
%! assert (numel (kids), 0);

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = gca;
%!   plot (hax, 1:10);
%!   set (hax, "interpreter", "tex");
%!   cla (hax);
%!   kids = get (hax, "children");
%!   assert (numel (kids), 0);
%!   assert (get (hax, "interpreter"), "tex");
%!   plot (hax, 1:10);
%!   cla (hax, "reset");
%!   kids = get (hax, "children");
%!   assert (numel (kids), 0);
%!   assert (get (hax, "interpreter"), "none");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

