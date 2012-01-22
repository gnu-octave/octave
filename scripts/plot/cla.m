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
## @deftypefn  {Function File} {} cla ()
## @deftypefnx {Function File} {} cla ("reset")
## @deftypefnx {Function File} {} cla (@var{hax})
## @deftypefnx {Function File} {} cla (@var{hax}, "reset")
## Delete the children of the current axes with visible handles.
## If @var{hax} is specified and is an axes object handle, operate on it
## instead of the current axes.  If the optional argument @code{"reset"}
## is specified, also delete the children with hidden handles.
## @seealso{clf}
## @end deftypefn

## Author: Ben Abbott <bpabbott@mac.com>
## Created: 2008-10-03

function cla (varargin)

  if (nargin > 2)
    print_usage ();
  elseif (nargin > 1)
    if (ishandle (varargin{1})
        && strcmp (get (varargin{1}, "type"), "axes")
        && ischar (varargin{2}) && strcmpi (varargin{2}, "reset"))
      oldhax = gca;
      hax = varargin{1};
      do_reset = true;
    else
      print_usage ();
    endif
  elseif (nargin == 1)
    if (ishandle (varargin{1})
        && strcmp (get (varargin{1}, "type"), "axes"))
      oldhax = gca;
      hax = varargin{1};
      do_reset = false;
    elseif (ischar (varargin{1}) && strcmpi (varargin{1}, "reset"))
      hax = gca;
      oldhax = hax;
      do_reset = true;
    else
      print_usage ();
    endif
  else
    hax = gca;
    oldhax = hax;
    do_reset = false;
  endif

  hc = get (hax, "children");

  if (! do_reset && ! isempty (hc))
    hc = findobj (hc, "flat", "visible", "on");
    hc = setdiff (hc, hax);
  endif

  if (! isempty (hc))
    ## Delete the children of the axis.
    delete (hc);
  endif

  ## FIXME: The defaults should be "reset()" below, but so far there is
  ## no method to determine the defaults, much less return an object's
  ## properties to their default values.  Instead make a close
  ## approximation.

  axes (hax);
  axis ("auto");

  ## Set the current axis back to where it was upon entry.
  axes (oldhax);

endfunction

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   plot (1:10)
%!   cla ()
%!   kids = get (gca, "children");
%!   cla ()
%! unwind_protect_cleanup
%!   close (hf)
%! end_unwind_protect
%! assert (numel (kids), 0)
