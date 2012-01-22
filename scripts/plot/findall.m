## Copyright (C) 2008-2012 Bill Denney
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
## @deftypefn  {Function File} {@var{h} =} findall ()
## @deftypefnx {Function File} {@var{h} =} findall (@var{prop_name}, @var{prop_value})
## @deftypefnx {Function File} {@var{h} =} findall (@var{h}, @dots{})
## @deftypefnx {Function File} {@var{h} =} findall (@var{h}, "-depth", @var{d}, @dots{})
## Find graphics object with specified property values including hidden handles.
##
## This function performs the same function as @code{findobj}, but it
## includes hidden objects in its search.  For full documentation, see
## @code{findobj}.
## @seealso{get, set, findobj, allchild}
## @end deftypefn

## Author: Bill Denney <bill@denney.ws>

function h = findall (varargin)

  unwind_protect
    shh = get (0, "showhiddenhandles");
    set (0, "showhiddenhandles", "on");
    h = findobj (varargin{:});
  unwind_protect_cleanup
    set (0, "showhiddenhandles", shh);
  end_unwind_protect

endfunction


%!testif HAVE_FLTK
%! toolkit = graphics_toolkit ();
%! graphics_toolkit ("fltk");
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = findall (hf);
%!   all_handles(1:13,1) = {"uimenu"};
%!   all_handles(14) = {"figure"};
%!   assert (get (h, "type"), all_handles);
%! unwind_protect_cleanup
%!   close (hf);
%!   graphics_toolkit (toolkit);
%! end_unwind_protect

