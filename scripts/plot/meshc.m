## Copyright (C) 1996, 1997 John W. Eaton
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} meshc (@var{x}, @var{y}, @var{z})
## Plot a mesh and contour given matrices @var{x}, and @var{y} from 
## @code{meshgrid} and a matrix @var{z} corresponding to the @var{x} and 
## @var{y} coordinates of the mesh.  If @var{x} and @var{y} are vectors, 
## then a typical vertex is (@var{x}(j), @var{y}(i), @var{z}(i,j)).  Thus, 
## columns of @var{z} correspond to different @var{x} values and rows of 
## @var{z} correspond to different @var{y} values.
## @seealso{meshgrid, mesh, contour}
## @end deftypefn

function h = meshc (varargin)

  newplot ();

  if (nargin == 1)
    z = varargin{1};
    if (ismatrix (z))
      [nr, nc] = size (z);
      x = 1:nc;
      y = (1:nr)';
    else
      error ("meshc: argument must be a matrix");
    endif
  elseif (nargin == 3)
    x = varargin{1};
    y = varargin{2};
    z = varargin{3};

    if (isvector (x) && isvector (y) && ismatrix (z))
      if (rows (z) == length (y) && columns (z) == length (x))
        x = x(:)';
        y = y(:);
      else
        msg = "meshc: rows (z) must be the same as length (y) and";
        msg = sprintf ("%s\ncolumns (z) must be the same as length (x)", msg);
        error (msg);
      endif
    elseif (ismatrix (x) && ismatrix (y) && ismatrix (z))
      if (! (size_equal (x, y) && size_equal (x, z)))
        error ("meshc: x, y, and z must have same dimensions");
      endif
    else
      error ("meshc: x and y must be vectors and z must be a matrix");
    endif
  else
    print_usage ();
  endif

  ## make a default line object, and make it the current axes for the
  ## current figure.
  ca = gca ();

  tmp = __go_surface__ (ca, "xdata", x, "ydata", y, "zdata", z);

  set (ca, "view", [-37.5, 30]);

  hold on;

  [c, lev] = contourc (varargin{:});

  cmap = get (gcf(), "colormap");
  
  levx = linspace (min (lev), max (lev), size (cmap, 1));

  drawnow();
  ax = axis();
  zmin = 2 * ax(5) - ax(6);

  ## decode contourc output format
  i1 = 1;
  while (i1 < length (c))

    clev = c(1,i1);
    clen = c(2,i1);

    ccr = interp1 (levx, cmap(:,1), clev);
    ccg = interp1 (levx, cmap(:,2), clev);
    ccb = interp1 (levx, cmap(:,3), clev);

    ii = i1+1:i1+clen;
    line (c(1,ii), c(2,ii), zmin*ones(size(ii)), "color", [ccr, ccg, ccb]);

    i1 += c(2,i1)+1;
  endwhile
  
  if (nargout > 0)
    h = tmp;
  endif
endfunction
