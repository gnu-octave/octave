function mesh (x, y, z)

# usage: mesh (x, y, z)
#
# See also: plot, semilogx, semilogy, loglog, polar, meshdom, contour,
#           bar, stairs, gplot, gsplot, replot, xlabel, ylabel, title 

  if (nargin == 1)
    z = x;
    if (is_matrix (z))
      set hidden3d;
      set data style lines;
      set surface;
      set nocontour;
      set noparametric;
      set view 60, 30, 1, 1
      gsplot (z);
    else
      error ("mesh: argument must be a matrix");
    endif
  elseif (nargin == 3)
    if (is_vector (x) && is_vector (y) && is_matrix (z))
      xlen = length (x);
      ylen = length (y);
      if (xlen == rows (z) && ylen == columns (z))
        if (rows (x) == 1)
          x = x';
        endif
        len = 3 * ylen;
        zz = zeros (xlen, ylen);
        k = 1;
        for i = 1:3:len
          zz(:,i)   = x;
          zz(:,i+1) = y(k) * ones (xlen, 1);
          zz(:,i+2) = z(:,k);
          k++;
        endfor
	set hidden3d;
	set data style lines;
        set surface;
        set nocontour;
	set parametric;
        set view 60, 30, 1, 1
	gsplot (zz);
      else
        disp ("mesh: rows (z) must be the same as length (x)");
        error ("      and columns (z) must be the same as length (y)");
      endif
    else
      error ("mesh: x and y must be vectors and z must be a matrix");
    endif    
  else
    error ("usage: mesh (z)");
  endif

endfunction
