## Copyright (C) 2010-2012 David Bateman
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
## @deftypefn {Function File} {@var{rgb} =} __color_str_rgb__ (@var{str})
## Undocumented internal function.
## @end deftypefn

function rgb = __color_str_rgb__ (str)

  if (ischar (str))
    if (strncmpi (str, "black", 5))
      rgb = [0, 0, 0];
    elseif (strncmpi (str, "red", 3))
      rgb = [1, 0, 0];
    elseif (strncmpi (str, "green", 5))
      rgb = [0, 1, 0];
    elseif (strncmpi (str, "blue", 4))
      rgb = [0, 0, 1];

    elseif (strncmpi (str, "yellow", 6))
      rgb = [1, 1, 0];
    elseif (strncmpi (str, "magenta", 7))
      rgb = [1, 0, 1];
    elseif (strncmpi (str, "cyan", 4))
      rgb = [0, 1, 1];
    elseif (strncmpi (str, "white", 5))
      rgb = [1, 1, 1];
    else
      rgb = [0, 0, 0];
    endif
  else
    error ("__color_str_rgb__: expecting a string argument");
  endif
endfunction
