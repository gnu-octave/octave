########################################################################
##
## Copyright (C) 2026 The Octave Project Developers
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
## @deftypefn {} {@var{rgb} =} __graph_plot_validate_colorspec__ (@var{spec}, @var{name})
## Validate a color specification for a @code{GraphPlot} cosmetic
## property.
##
## @var{spec} may be either a @code{1x3} numeric RGB triplet with
## entries in @code{[0, 1]}, or one of the short / long MATLAB color
## names (@qcode{"r"}, @qcode{"red"}, @qcode{"g"}, @qcode{"green"},
## @qcode{"b"}, @qcode{"blue"}, @qcode{"c"}, @qcode{"cyan"},
## @qcode{"m"}, @qcode{"magenta"}, @qcode{"y"}, @qcode{"yellow"},
## @qcode{"k"}, @qcode{"black"}, @qcode{"w"}, @qcode{"white"}).  The
## return value is always an @code{1x3} @code{double} RGB row.
##
## @var{name} is the name of the property being set, used in the error
## message if validation fails.
##
## This is a private helper for @file{GraphPlot.m}.
## @end deftypefn

function rgb = __graph_plot_validate_colorspec__ (spec, name)

  if (nargin < 2)
    name = "color";
  endif

  if (ischar (spec) && isrow (spec))
    switch (lower (spec))
      case {"r", "red"}
        rgb = [1 0 0];
      case {"g", "green"}
        rgb = [0 1 0];
      case {"b", "blue"}
        rgb = [0 0 1];
      case {"c", "cyan"}
        rgb = [0 1 1];
      case {"m", "magenta"}
        rgb = [1 0 1];
      case {"y", "yellow"}
        rgb = [1 1 0];
      case {"k", "black"}
        rgb = [0 0 0];
      case {"w", "white"}
        rgb = [1 1 1];
      otherwise
        error ("Octave:invalid-input-arg", ...
               "GraphPlot: %s value '%s' is not a recognised color", ...
               name, spec);
    endswitch
    return;
  endif

  if (isnumeric (spec) && isrow (spec) && numel (spec) == 3 ...
      && isreal (spec) && all (isfinite (spec)) ...
      && all (spec >= 0) && all (spec <= 1))
    rgb = double (spec);
    return;
  endif

  error ("Octave:invalid-input-arg", ...
         "GraphPlot: %s must be an RGB triplet in [0,1] or a color name", ...
         name);

endfunction


## ---------------- BIST ----------------

## Short color names.
%!assert (__graph_plot_validate_colorspec__ ("r", "C"), [1 0 0])
%!assert (__graph_plot_validate_colorspec__ ("g", "C"), [0 1 0])
%!assert (__graph_plot_validate_colorspec__ ("b", "C"), [0 0 1])
%!assert (__graph_plot_validate_colorspec__ ("k", "C"), [0 0 0])
%!assert (__graph_plot_validate_colorspec__ ("w", "C"), [1 1 1])
%!assert (__graph_plot_validate_colorspec__ ("c", "C"), [0 1 1])
%!assert (__graph_plot_validate_colorspec__ ("m", "C"), [1 0 1])
%!assert (__graph_plot_validate_colorspec__ ("y", "C"), [1 1 0])

## Long color names.
%!assert (__graph_plot_validate_colorspec__ ("red", "C"), [1 0 0])
%!assert (__graph_plot_validate_colorspec__ ("green", "C"), [0 1 0])
%!assert (__graph_plot_validate_colorspec__ ("blue", "C"), [0 0 1])
%!assert (__graph_plot_validate_colorspec__ ("black", "C"), [0 0 0])
%!assert (__graph_plot_validate_colorspec__ ("white", "C"), [1 1 1])

## Color names are case-insensitive.
%!assert (__graph_plot_validate_colorspec__ ("RED", "C"), [1 0 0])
%!assert (__graph_plot_validate_colorspec__ ("Black", "C"), [0 0 0])

## Numeric RGB triplet.
%!assert (__graph_plot_validate_colorspec__ ([0.5 0.5 0.5], "C"), ...
%!        [0.5 0.5 0.5])
%!assert (__graph_plot_validate_colorspec__ ([0 0 0], "C"), [0 0 0])
%!assert (__graph_plot_validate_colorspec__ ([1 1 1], "C"), [1 1 1])

## Out-of-range RGB rejected.
%!error <RGB triplet> __graph_plot_validate_colorspec__ ([2 0 0], "C")
%!error <RGB triplet> __graph_plot_validate_colorspec__ ([-1 0 0], "C")

## Wrong shape rejected.
%!error <RGB triplet> __graph_plot_validate_colorspec__ ([0 0], "C")
%!error <RGB triplet> __graph_plot_validate_colorspec__ ([0;0;0], "C")

## Non-color string rejected.
%!error <recognised color> __graph_plot_validate_colorspec__ ("bogus", "C")

## Error message uses the supplied property name.
%!error <NodeColor> __graph_plot_validate_colorspec__ ([2 0 0], "NodeColor")
%!error <EdgeColor> __graph_plot_validate_colorspec__ ("xx", "EdgeColor")
