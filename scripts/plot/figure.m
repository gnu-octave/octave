## Copyright (C) 1996-2012 John W. Eaton
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
## @deftypefn  {Function File} {} figure (@var{n})
## @deftypefnx {Function File} {} figure (@var{n}, @var{property}, @var{value}, @dots{})
## Set the current plot window to plot window @var{n}.  If no arguments are
## specified, the next available window number is chosen.
##
## Multiple property-value pairs may be specified for the figure, but they
## must appear in pairs.
## @end deftypefn

## Author: jwe, Bill Denney

function h = figure (varargin)

  nargs = nargin;

  f = NaN;

  init_new_figure = false;
  if (mod (nargs, 2) == 1)
    tmp = varargin{1};
    if (ishandle (tmp) && strcmp (get (tmp, "type"), "figure"))
      f = tmp;
      varargin(1) = [];
      nargs--;
    elseif (isnumeric (tmp) && tmp > 0 && tmp == fix (tmp))
      f = tmp;
      init_new_figure = true;
      varargin(1) = [];
      nargs--;
    else
      error ("figure: expecting figure handle or figure number");
    endif
  endif

  ## Check to see if we already have a figure on the screen.  If we do,
  ## then update it if it is different from the figure we are creating
  ## or switching to.
  cf = get (0, "currentfigure");
  if (! isempty (cf) && cf != 0)
    if (isnan (f) || cf != f)
      drawnow ();
    endif
  endif

  if (rem (nargs, 2) == 0)
    if (isnan (f) || init_new_figure)
      if (ismac () && strcmp (graphics_toolkit (), "fltk"))
        ## FIXME - Hack for fltk-aqua to work around bug # 31931
        f = __go_figure__ (f);
        drawnow ();
        if (! isempty (varargin))
          set (f, varargin{:});
        endif
      else
        f = __go_figure__ (f, varargin{:});
      endif
    elseif (nargs > 0)
      set (f, varargin{:});
    endif
    set (0, "currentfigure", f);
  else
    print_usage ();
  endif

  cf = get (0, "currentfigure");
  if (strcmp (get (cf, "__graphics_toolkit__"), "fltk"))
    __add_default_menu__ (cf);
  endif

  if (nargout > 0)
    h = f;
  endif

endfunction

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   assert (gcf, hf);
%!   assert (isfigure (hf));
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
