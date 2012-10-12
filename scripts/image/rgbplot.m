## Copyright (C) 2012 Rik Wehbring
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
## @deftypefn {Function File} {} rgbplot (@var{cmap})
## Plot the components of a colormap.
##
## The first column is plotted in red, the second column in green, and
## the third column in blue.  The values are between 0 and 1 and represent
## the intensity of the RGB components in the given indexed color.
## @seealso{colormap}
## @end deftypefn

function rgbplot (cmap)

  if (nargin != 1)
    print_usage ();
  endif

  if (! iscolormap (cmap))
    error ("rgbplot: CMAP must be a colormap");
  endif

  plot (cmap(:,1),"r", cmap(:,2),"g", cmap(:,3),"b");
  set (gca, 'ytick', 0:0.1:1);
  xlabel ("color index");

endfunction


%!demo
%! clf;
%! rgbplot (ocean);

%%test input validation
%!error rgbplot ()
%!error rgbplot (1,2)
%!error <CMAP must be a colormap> rgbplot ({0 1 0})
