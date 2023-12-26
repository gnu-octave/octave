########################################################################
##
## Copyright (C) 1994-2024 The Octave Project Developers
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
## @deftypefn  {} {@var{map} =} gray ()
## @deftypefnx {} {@var{map} =} gray (@var{n})
## Create gray colormap.  This colormap varies from black to white with shades
## of gray.
##
## The argument @var{n} must be a scalar.
## If @var{n} is not specified the length of the current colormap is used.  If
## there is no current colormap the default value of 256 is used.
## @seealso{colormap}
## @end deftypefn

function map = gray (n)

  if (nargin == 1)
    if (! isscalar (n))
      error ("gray: N must be a scalar");
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

  if (n == 1)
    map = [0, 0, 0];
  elseif (n > 1)
    gr = [0:(n-1)]' / (n - 1);
    map = [gr, gr, gr];
  else
    map = zeros (0, 3);
  endif

endfunction


%!demo
%! ## Show the 'gray' colormap profile and as an image
%! cmap = gray (16);
%! subplot (2, 1, 1);
%!  rgbplot (cmap, "composite");
%! subplot (2, 1, 2);
%!  rgbplot (cmap);


%!assert (size (gray ()), [256, 3])
%!assert (size (gray (16)), [16, 3])

%!assert (gray (1), [0, 0, 0])
%!assert (gray (true), double ([0, 0, 0]))
%!assert (gray (char (1)), double ([0, 0, 0]))
%!assert (gray (int32 (1)), double ([0, 0, 0]))

%!assert (gray (0), zeros (0, 3))
%!assert (gray (-1), zeros (0, 3))

%!assert (gray (11), [0:.1:1]' .* [1, 1, 1], eps)

## Input validation
%!error <N must be a scalar> gray ("foo")
%!error <N must be a scalar> gray ([1, 2, 3])
%!error <N must be a scalar> gray ({1, 2, 3})
