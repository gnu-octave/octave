function [xx, yy] = meshdom (x, y)

# usage: [xx, yy] = meshdom (x, y)
#
# Given vectors of x and y coordinates, return two matrices
# corresponding to the x and y coordinates of the mesh.
#
# See the file sombrero.m for an example of using mesh and meshdom.
#
# See also: plot, semilogx, semilogy, loglog, polar, mesh, contour,
#           bar, stairs, gplot, gsplot, replot, xlabel, ylabel, title 

  if (nargin == 2)
    if (is_vector (x) && is_vector (y))
      xlen = length (x);
      ylen = length (y);
      xx = zeros (ylen, xlen);
      yy = zeros (ylen, xlen);
      y = y (ylen:-1:1);
      if (columns (x) == 1)
        x = x';
      endif
      if (rows (y) == 1)
        y = y';
      endif
      for i = 1:ylen
        xx(i,:) = x;
      endfor
      for i = 1:xlen
        yy(:,i) = y;
      endfor
    else
      error ("meshdom: arguments must be vectors");
    endif
  else
    error ("usage: [xx, yy] = meshdom (x, y)");
  endif

endfunction
