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
## @deftypefn  {} {@var{map} =} jet ()
## @deftypefnx {} {@var{map} =} jet (@var{n})
## Create color colormap.  This colormap ranges from dark blue through blue,
## cyan, green, yellow, red, to dark red.
##
## The argument @var{n} must be a scalar.
## If @var{n} is not specified the length of the current colormap is used.  If
## there is no current colormap the default value of 256 is used.
##
## Programming Note: The @code{jet} colormap is not perceptually uniform.
## Try the @code{viridis} colormap if that is important.  For a drop-in
## replacement for @code{jet} with better perceptual characteristics try
## the @code{turbo} colormap.
## @seealso{colormap, turbo}
## @end deftypefn

function map = jet (n)

  if (nargin == 1)
    if (! isscalar (n))
      error ("jet: N must be a scalar");
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
    map = [0, 1, 1];
  elseif (n == 2)
    map = [0, 0, 1
           0, 1, 1];
  elseif (n > 2)
    nel = ceil (n/4);           # number of elements
    idx1 = ceil (3/8 * n) + 1;  # ~3/8*n for large n
    if (mod (n, 8) == 2)
      idx1 += 1;
    endif
    idx2 = idx1 + nel - 1;      # ~5/8*n for large n
    idx3 = min (idx2 + nel, n); # ~7/8*n for large n

    r = zeros (n, 1);
    r(idx1:idx2, 1) = [1:nel] / nel;
    r(idx2:idx3, 1) = 1;
    nel2 = n - idx3;
    r(idx3:(idx3+nel2), 1) = [nel:-1:(nel - nel2)] / nel;

    idx1 = idx1 - nel;          # ~1/8*n for large n
    idx2 = idx1 + nel - 1;      # ~3/8*n for large n
    idx3 = min (idx2 + nel, n); # ~5/8*n for large n

    g = zeros (n, 1);
    g(idx1:idx2, 1) = [1:nel] / nel;
    g(idx2:idx3, 1) = 1;
    nel2 = min (nel, n - idx3);
    g(idx3:(idx3+nel2), 1) = [nel:-1:(nel - nel2)] / nel;

    idx1 = max (idx2 - nel, 1); # ~1/8*n for large n
    idx2 = idx2;                # ~3/8*n for large n
    idx3 = idx3;                # ~5/8*n for large n

    b = zeros (n, 1);
    nel2 = min (nel, idx1-1);
    b(1:idx1, 1) = [(nel - nel2):nel] / nel;
    b(idx1:idx2, 1) = 1;
    nel2 = min (nel, n - idx3);
    b(idx2:(idx2+nel2), 1) = [nel:-1:(nel - nel2)] / nel;

    map = [r, g, b];
  else
    map = zeros (0, 3);
  endif

endfunction


%!demo
%! ## Show the 'jet' colormap profile and as an image
%! cmap = jet (256);
%! subplot (2, 1, 1);
%!  rgbplot (cmap, "composite");
%! subplot (2, 1, 2);
%!  rgbplot (cmap);


%!assert (size (jet ()), [256, 3])
%!assert (size (jet (16)), [16, 3])

%!assert (jet (1), [0, 1, 1])
%!assert (jet (true), double ([0, 1, 1]))
%!assert (jet (char (1)), double ([0, 1, 1]))
%!assert (jet (int32 (1)), double ([0, 1, 1]))
%!assert (jet (2), [0, 0, 1; 0, 1, 1])

%!assert (jet (0), zeros (0, 3))
%!assert (jet (-1), zeros (0, 3))

%!test
%! a = zeros(15, 3);
%! a([3:13],2) = [0.25, 0.5, 0.75, 1, 1, 1, 1, 1, 0.75, 0.5, 0.25];
%! a([7:15], 1) = a([3:11], 2);
%! a(:,3) = flipud (a(:,1));
%! assert (jet (15), a, eps)

## Input validation
%!error <N must be a scalar> jet ("foo")
%!error <N must be a scalar> jet ([1, 2, 3])
%!error <N must be a scalar> jet ({1, 2, 3})
