## Copyright (C) 2008-2012 Michael Goffioul
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
## @deftypefn  {Function File} {@var{name} =} graphics_toolkit ()
## @deftypefnx {Function File} {@var{old_name} =} graphics_toolkit (@var{name})
## @deftypefnx {Function File} {} graphics_toolkit (@var{hlist}, @var{name})
## Query or set the default graphics toolkit to @var{name}.  If the
## toolkit is not already loaded, it is first initialized by calling the
## function @code{__init_@var{name}__}.
##
## When called with a list of figure handles, @var{hlist}, the graphics
## toolkit is changed only for the listed figures.
## @seealso{available_graphics_toolkits}
## @end deftypefn

function retval = graphics_toolkit (name, hlist = [])

  if (nargin > 2)
    print_usage ();
  endif

  if (nargout > 0 || nargin == 0)
    retval = get (0, "defaultfigure__graphics_toolkit__");
  endif

  if (nargin == 0)
    return;
  elseif (nargin == 1)
    if (! ischar (name))
      error ("graphics_toolkit: invalid graphics toolkit NAME");
    endif
  elseif (nargin == 2)
    ## Swap input arguments
    [hlist, name] = deal (name, hlist);
    if (! all (isfigure (hlist)))
      error ("graphics_toolkit: invalid figure handle list HLIST");
    elseif (! ischar (name))
      error ("graphics_toolkit: invalid graphics toolkit NAME");
    endif
  endif

  if (! any (strcmp (loaded_graphics_toolkits (), name)))
    feval (["__init_", name, "__"]);
    if (! any (strcmp (loaded_graphics_toolkits (), name)))
      error ("graphics_toolkit: %s toolkit was not correctly loaded", name);
    endif
  endif

  if (isempty (hlist))
    set (0, "defaultfigure__graphics_toolkit__", name);
  else
    set (hlist, "__graphics_toolkit__", name);
  endif

endfunction


%!testif HAVE_FLTK
%! unwind_protect
%!   hf = figure ("visible", "off"); 
%!   toolkit = graphics_toolkit ();
%!   assert (get (0, "defaultfigure__graphics_toolkit__"), toolkit);
%!   graphics_toolkit (hf, "fltk"); 
%!   assert (get (hf, "__graphics_toolkit__"), "fltk");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!testif HAVE_FLTK
%!  old_toolkit = graphics_toolkit ();
%!  switch old_toolkit
%!    case {"gnuplot"}
%!      new_toolkit = "fltk";
%!    otherwise
%!      new_toolkit = "gnuplot";
%!  endswitch
%!  assert (graphics_toolkit (new_toolkit), old_toolkit)
%!  assert (graphics_toolkit (old_toolkit), new_toolkit)

