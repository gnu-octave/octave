########################################################################
##
## Copyright (C) 2012-2023 The Octave Project Developers
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
## @deftypefn {} {@var{tf} =} iscolormap (@var{cmap})
## Return true if @var{cmap} is a colormap.
##
## A colormap is a real matrix, of class single or double, with 3 columns.
## Each row represents a single color.  The 3 columns contain red, green,
## and blue intensities respectively.
##
## All values in a colormap should be in the [0 1] range but this is not
## enforced.  Each function must decide what to do for values outside this
## range.
##
## @seealso{colormap, rgbplot}
## @end deftypefn

function tf = iscolormap (cmap)

  if (nargin < 1)
    print_usage ();
  endif

  tf = isnumeric (cmap) && isreal (cmap) && isfloat (cmap) ...
       && ndims (cmap) == 2 && columns (cmap) == 3;

endfunction


%!assert (iscolormap (jet (64)))
%!assert (iscolormap ({0 1 0}), false)
%!assert (iscolormap ([0 1i 0]), false)
%!assert (iscolormap (ones (3,3,3)), false)
%!assert (iscolormap (ones (3,4)), false)
