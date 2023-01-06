########################################################################
##
## Copyright (C) 2021-2023 The Octave Project Developers
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
## @deftypefn {} {} __check_rendering_capability__ (@var{who}, @var{fig})
## Undocumented internal function.
## @end deftypefn

function __check_rendering_capability__ (who, fig)

  if (strcmp (get (fig, "visible"), "on"))
    return;
  endif

  toolkit = get (fig, "__graphics_toolkit__");
  display = getenv ("DISPLAY");

  if (! strcmp (toolkit, "qt"))
    error ("%s: rendering with %s toolkit requires visible figure (DISPLAY='%s')",
           who, toolkit, display);
  endif

  gl_window = get (fig, "__gl_window__");
  qt_offscreen = __have_feature__ ("QT_OFFSCREEN");

  if (strcmp (gl_window, "on") || qt_offscreen)
    return;
  endif

  error ("%s: offscreen rendering with %s toolkit requires __gl_window__='on' or QT_OFFSCREEN feature (__gl_window__='%s'; QT_OFFSCREEN=%d, DISPLAY='%s')",
         who, toolkit, gl_window, qt_offscreen, display);

endfunction
