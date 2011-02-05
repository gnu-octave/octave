## Copyright (C) 2008-2011 Michael Goffioul
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
## @deftypefn  {Function File} {} graphics_toolkit (@var{name})
## @deftypefnx {Function File} {} graphics_toolkit (@var{hlist}, @var{name})
## Change the default graphics toolkit to @var{name}.  If the
## toolkit is not already loaded, it is first initialized by calling the
## function @code{__init_@var{name}__}.
##
## When called with a list of figure handles, @var{hlist}, the graphics
## toolkit is changed only for the listed figures.
## @seealso{available_graphics_toolkits}
## @end deftypefn

function graphics_toolkit (varargin)

  name = "";
  hlist = [];

  if (nargin == 1)
    if (ischar (varargin{1}))
      name = varargin{1};
    else
      error ("graphics_toolkit: invalid graphics toolkit NAME");
    endif
  elseif (nargin == 2)
    if (isnumeric (varargin{1}) && ischar (varargin{2}))
      hlist = varargin{1};
      name = varargin{2};
    elseif (ischar (varargin{2}))
      error ("graphics_toolkit: invalid handle list");
    else
      error ("graphics_toolkit: invalid graphics toolkit NAME");
    endif
  else
    print_usage ();
  endif

  if (! any (strcmp (available_graphics_toolkits (), name)))
    feval (["__init_", name, "__"]);
    if (! any (strcmp (available_graphics_toolkits (), name)))
      error ("graphics_toolkit: %s toolkit was not correctly registered",
             name);
    endif
  endif

  if (isempty (hlist))
    set (0, "defaultfigure__graphics_toolkit__", name);
  else
    for h = hlist(:)'
      set (h, "__graphics_toolkit__", name);
    endfor
  endif

endfunction
