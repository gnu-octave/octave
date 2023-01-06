########################################################################
##
## Copyright (C) 1996-2023 The Octave Project Developers
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
## @deftypefn  {} {} figure
## @deftypefnx {} {} figure @var{n}
## @deftypefnx {} {} figure (@var{n})
## @deftypefnx {} {} figure (@dots{}, "@var{property}", @var{value}, @dots{})
## @deftypefnx {} {@var{h} =} figure (@dots{})
## Create a new figure window for plotting.
##
## If no arguments are specified, a new figure with the next available number
## is created.
##
## If called with an integer @var{n}, and no such numbered figure exists, then
## a new figure with the specified number is created.  If the figure already
## exists then it is made visible and becomes the current figure for plotting.
##
## Multiple property-value pairs may be specified for the figure object, but
## they must appear in pairs.
##
## The optional return value @var{h} is a graphics handle to the created figure
## object.
##
## Programming Note: The full list of properties is documented at
## @ref{Figure Properties}.
## @seealso{axes, gcf, shg, clf, close}
## @end deftypefn

function h = figure (varargin)

  nargs = nargin;

  init_new_figure = true;
  if (nargs == 0)
    f = NaN;
  else
    arg = varargin{1};
    if (nargs == 1 && ischar (arg))
      arg = str2double (arg);
      if (isnan (arg))
        arg = varargin{1};
      endif
    endif
    if (isscalar (arg) && isnumeric (arg))
      if (isfigure (arg))
        f = arg;
        init_new_figure = false;
        varargin(1) = [];
        nargs -= 1;
      elseif (arg > 0 && arg == fix (arg))
        f = arg;
        varargin(1) = [];
        nargs -= 1;
      else
        error ("figure: N must be figure handle or figure number");
      endif
    else
      f = NaN;
    endif
  endif

  ## Check to see if we already have a figure on the screen.  If we do,
  ## then update it if it is different from the figure we are creating
  ## or switching to.
  cf = get (0, "currentfigure");   # Can't use gcf() because it calls figure()
  if (! isempty (cf) && cf != 0)
    if (init_new_figure || cf != f)
      drawnow ();
    endif
  endif

  if (init_new_figure)
    f = __go_figure__ (f, varargin{:});
    __add_default_menu__ (f);
    __set_default_mouse_modes__ (f);
  elseif (nargs > 0)
    set (f, varargin{:});
  endif

  if (strcmp (get (f, "handlevisibility"), "on"))
    set (0, "currentfigure", f);
  endif

  ## When switching to figure N, make figure visible and on top of stack,
  ## unless visibility is explicitly switched off.
  if (! init_new_figure)
    vis_on = true;
    idx = find (strcmpi (varargin(1:2:end), "visible"), 1) * 2 - 1;
    if (! isempty (idx))
      if (idx < numel (varargin) && strcmpi (varargin{idx+1}, "off"))
        vis_on = false;
      endif
    endif
    if (vis_on)
      set (f, "visible", "on");
    endif
    __show_figure__ (f);
  endif

  if (nargout > 0)
    h = f;
  endif

endfunction


%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   assert (hf, gcf);
%!   assert (isfigure (hf));
%!   hf2 = figure (hf, "visible", "off");
%!   assert (hf, hf2);
%!   assert (hf2, gcf);
%!   assert (isfigure (hf2));
%!   assert (get (hf2, "visible"), "off");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!error <N must be figure handle or figure number> figure (-1)
%!error <N must be figure handle or figure number> figure (1.5)
