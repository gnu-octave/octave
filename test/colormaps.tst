########################################################################
##
## Copyright (C) 2016-2023 The Octave Project Developers
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

## This is a test that should be used on all Octave colormaps.
## Because there is no function to get a list of all colormap
## functions, they should be added here manually.

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   all_colormaps = {@autumn, @bone, @cool, @copper, @cubehelix, ...
%!                    @flag, @gray, @hot, @hsv, @jet, @lines, @ocean, ...
%!                    @pink, @prism, @rainbow, @spring, @summer, ...
%!                    @viridis, @white, @winter};
%!
%!   for i = 1:numel (all_colormaps)
%!     f = all_colormaps{i};
%!
%!     assert (iscolormap (f (1)));
%!     assert (iscolormap (f (12)));
%!     assert (iscolormap (f (200)));
%!
%!     ## bug #44070
%!     assert (class (f (uint8 (12))), "double");
%!     assert (iscolormap (f (uint8 (12))));
%!
%!     assert (f (0), zeros (0, 3));
%!   endfor
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
