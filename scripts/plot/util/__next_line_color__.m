########################################################################
##
## Copyright (C) 2007-2023 The Octave Project Developers
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
## @deftypefn {} {@var{rgb} =} __next_line_color__ (@var{reset})
## Undocumented internal function.
## @end deftypefn

## Return the next line color in the rotation.

function rgb = __next_line_color__ ()

  ca = gca ();

  colororder = get (ca, "colororder");
  if (isempty (colororder))
    rgb = [0 0 0];   # black
    return;
  endif

  color_idx = fix (get (ca, "colororderindex"));
  num_colors = rows (colororder);
  color_idx = mod (color_idx, num_colors);
  if (color_idx == 0)
    color_idx = num_colors;
  elseif (color_idx < 0)
    color_idx = 1;
  endif

  rgb = colororder(color_idx, :);

  if (++color_idx > num_colors)
    color_idx = mod (color_idx, num_colors);
    if (color_idx == 0)
      color_idx = 1;
    endif
    ## Rollover through all colors also switches to next linestyle.
    style_idx = fix (get (ca, "linestyleorderindex"));
    set (ca, "linestyleorderindex", ++style_idx);
  endif
  set (ca, "colororderindex", color_idx);

endfunction


%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = axes ();
%!   set (hax, "colororder", [1 0 0; 0 1 0; 0 0 1]);
%!   hold on;
%!   h = plot (1:2,1:2,"o", 2:3,2:3,"x", 3:4,3:4,"d");
%!   assert (get (h, "color"), {[1 0 0]; [0 1 0]; [0 0 1]});
%!   cla (hax);
%!   hold on;
%!   h1 = plot (1:2,1:2, "o");
%!   h2 = plot (2:3,2:3, "x");
%!   h3 = plot (3:4,3:4, "d");
%!   assert (get ([h1;h2;h3], "color"), {[1 0 0]; [0 1 0]; [0 0 1]});
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
