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
## @deftypefn {} {@var{yi} =} __splinen__ (@var{x}, @var{y}, @var{xi}, @var{extrapval}, @var{f})
## Undocumented internal function.
## @end deftypefn

## FIXME: Allow arbitrary grids.

function yi = __splinen__ (x, y, xi, extrapval, f)

  ## ND function to check whether any object in cell array is *not* a vector.
  isnotvec = @(x) cellfun ("numel", x) != cellfun ("length", x);
  if (! iscell (x) || length (x) < ndims (y) || any (isnotvec (x))
      || ! iscell (xi) || length (xi) < ndims (y) || any (isnotvec (xi)))
    error ("__splinen__: %s: non-gridded data or dimensions inconsistent", f);
  endif

  yi = y;
  for i = length (x):-1:1
    yi = permute (spline (x{i}, yi, xi{i}(:)), [length(x),1:length(x)-1]);
  endfor

  if (! isempty (extrapval))
    [xi{:}] = ndgrid (cellfun (@(x) x(:), xi, "uniformoutput", false){:});
    idx = false (size (xi{1}));
    for i = 1 : length (x)
      idx |= xi{i} < min (x{i}(:)) | xi{i} > max (x{i}(:));
    endfor
    yi(idx) = extrapval;
  endif

endfunction
