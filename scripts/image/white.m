########################################################################
##
## Copyright (C) 1999-2024 The Octave Project Developers
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
## @deftypefn  {} {@var{map} =} white ()
## @deftypefnx {} {@var{map} =} white (@var{n})
## Create color colormap.  This colormap is completely white.
##
## The argument @var{n} must be a scalar.
## If @var{n} is not specified the length of the current colormap is used.  If
## there is no current colormap the default value of 256 is used.
## @seealso{colormap}
## @end deftypefn

function map = white (n)

  if (nargin == 1)
    if (! isscalar (n))
      error ("white: N must be a scalar");
    endif
    n = double (n);
  else
    hf = get (0, "currentfigure");
    if (! isempty (hf))
      n = rows (get (hf, "colormap"));
    else
      n = 256;
    endif
  endif
  map = ones (n, 3);

endfunction


%!demo
%! ## Show the 'white' colormap profile and as an image
%! cmap = white (256);
%! subplot (2, 1, 1);
%!  rgbplot (cmap, "composite");
%! subplot (2, 1, 2);
%!  rgbplot (cmap);

%!assert (size (white ()), [256, 3])
%!assert (size (white (16)), [16, 3])
%!assert (all ((white ()(:)) == 1), true)

%!assert (white (1), [1, 1, 1])
%!assert (white (true), double ([1, 1, 1]))
%!assert (white (char (1)), double ([1, 1, 1]))
%!assert (white (int32 (1)), double ([1, 1, 1]))
%!assert (white (0), zeros (0, 3))
%!assert (white (-1), zeros (0, 3))


## Input validation
%!error <function called with too many inputs> white (1, 2)
%!error <N must be a scalar> white ("foo")
%!error <N must be a scalar> white ([1, 2, 3])
%!error <N must be a scalar> white ({1, 2, 3})
