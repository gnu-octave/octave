## Copyright (C) 2008-2012 David Bateman
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
## @deftypefn {Function File} {@var{hlink} =} linkprop (@var{h}, @var{prop})
## Link graphics object properties, such that a change in one is
## propagated to the others.  The properties to link are given as a
## string of cell string array by @var{prop} and the objects containing
## these properties by the handle array @var{h}.
##
## An example of the use of linkprop is
##
## @example
## @group
## x = 0:0.1:10;
## subplot (1,2,1);
## h1 = plot (x, sin (x));
## subplot (1,2,2);
## h2 = plot (x, cos (x));
## hlink = linkprop ([h1, h2], @{"color","linestyle"@});
## set (h1, "color", "green");
## set (h2, "linestyle", "--");
## @end group
## @end example
##
## @end deftypefn

function hlink = linkprop (h, prop)
  if (ischar (prop))
    prop = {prop};
  elseif (!iscellstr (prop))
    error ("linkprop: properties must be a string or cell string array");
  endif

  for i = 1 : numel (h)
    for j = 1 : numel (prop)
      addlistener (h(i), prop{j}, {@update_prop, h, prop{j}});
    endfor
  endfor

  ## This should be an object that when destroyed removes the links
  ## The below is not quite right. As when you call "clear hlink" the
  ## hggroup continues to exist.
  hlink = hggroup ();
  set (hlink, "deletefcn", {@delete_prop, h, prop});
endfunction

function update_prop (h, d, hlist, prop)
  persistent recursion = false;

  ## Don't allow recursion
  if (! recursion)
    unwind_protect
      recursion = true;
      val = get (h, prop);
      for hh = hlist(:)'
        if (hh != h)
          oldval = get (hh, prop);
          if (! isequal (val, oldval))
            set (hh, prop, val);
          endif
        endif
      endfor
    unwind_protect_cleanup
      recursion = false;
    end_unwind_protect
  endif
endfunction

function delete_prop (h, d, hlist, prop)
  ## FIXME. Actually need to delete the linked properties.
  ## However, only warn if the graphics objects aren't being deleted.
  warn = false;
  for h = hlist(:)'
    if (ishandle (h) && !strcmpi (get (h, "beingdeleted"), "on"))
      warn = true;
      break;
    endif
  endfor
  if (warn)
    warning ("linkprop: can not remove linked properties");
  endif
endfunction
