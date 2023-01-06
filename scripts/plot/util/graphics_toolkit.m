########################################################################
##
## Copyright (C) 2008-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{tkit} =} graphics_toolkit ()
## @deftypefnx {} {@var{tkit} =} graphics_toolkit (@var{hlist})
## @deftypefnx {} {} graphics_toolkit (@var{name})
## @deftypefnx {} {} graphics_toolkit (@var{hlist}, @var{name})
## Query or set the default graphics toolkit which is assigned to new figures.
##
## With no inputs, return the current default graphics toolkit.  If the input
## is a list of figure graphic handles, @var{hlist}, then return the name
## of the graphics toolkit in use for each figure.
##
## When called with a single input @var{name} set the default graphics toolkit
## to @var{name}.  If the toolkit is not already loaded, it is initialized by
## calling the function @code{__init_@var{name}__}.  If the first input
## is a list of figure handles, @var{hlist}, then the graphics toolkit is set
## to @var{name} for these figures only.
##
## @seealso{available_graphics_toolkits}
## @end deftypefn

function tkit = graphics_toolkit (name, hlist = [])

  if (nargout > 0 || nargin == 0)
    tkit = get (0, "defaultfigure__graphics_toolkit__");
    ## Handle case where graphics_toolkit has been called before any plotting
    if (isempty (tkit))
      toolkits = available_graphics_toolkits ();
      if (any (strcmp ("qt", toolkits)))
        tkit = "qt";
      elseif (any (strcmp ("fltk", toolkits)))
        tkit = "fltk";
      elseif (! isempty (toolkits))
        tkit = toolkits{1};
      endif
    endif
  endif

  if (nargin == 0)
    return;
  elseif (nargin == 1)
    if (all (isfigure (name)))
      hlist = name;
      tkit = get (hlist, "__graphics_toolkit__");
      return;
    elseif (! ischar (name))
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

  if (! any (strcmp (available_graphics_toolkits (), name)))
    error ("graphics_toolkit: %s toolkit is not available", name);
  endif

  if (! any (strcmp (loaded_graphics_toolkits (), name)))
    if (strcmp (name, "gnuplot"))
      valid_version = __gnuplot_has_feature__ ("minimum_version");
      if (valid_version != 1)
        error ("graphics_toolkit: gnuplot version too old");
      endif
    endif
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


%!testif HAVE_OPENGL, HAVE_QT; have_window_system () && any (strcmp ("qt", available_graphics_toolkits ()))
%! unwind_protect
%!   hf = figure ("visible", "off");
%!   toolkit = graphics_toolkit ();
%!   assert (get (0, "defaultfigure__graphics_toolkit__"), toolkit);
%!   graphics_toolkit (hf, "qt");
%!   assert (graphics_toolkit (hf), "qt");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!testif HAVE_OPENGL, HAVE_QT; have_window_system () && any (strcmp ("gnuplot", available_graphics_toolkits ())) && any (strcmp ("qt", available_graphics_toolkits ()))
%! old_toolkit = graphics_toolkit ();
%! switch (old_toolkit)
%!   case {"gnuplot"}
%!     new_toolkit = "qt";
%!   otherwise
%!     new_toolkit = "gnuplot";
%! endswitch
%! assert (graphics_toolkit (new_toolkit), old_toolkit);
%! assert (graphics_toolkit (old_toolkit), new_toolkit);
