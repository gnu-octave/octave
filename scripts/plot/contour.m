function contour (z, n, x, y)

# usage: contour (z, n, x, y)
#
# See also: plot, semilogx, semilogy, loglog, polar, mesh, contour,
#           bar, stairs, gplot, gsplot, replot, xlabel, ylabel, title 


  if (nargin == 1)
    n = 10;
  endif

  if (nargin == 1 || nargin == 2)
    if (is_matrix (z))
      set nosurface;
      set contour;
      set cntrparam bspline
      command = sprintf ("set cntrparam levels %d", n);
      eval (command);
      set noparametric;
      set view 0, 0, 1.9, 1
      gsplot z w l 1;
    else
      error ("mesh: argument must be a matrix");
    endif
  elseif (nargin == 4)
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
        set nosurface
        set contour
        set cntrparam bspline
        command = sprintf ("set cntrparam levels %d", n);
        eval (command);
	set parametric;
        set view 0, 0, 1.9, 1
	gsplot zz w l 1;
      else
        disp ("mesh: rows (z) must be the same as length (x)");
        error ("      and columns (z) must be the same as length (y)");
      endif
    else
      error ("mesh: x and y must be vectors and z must be a matrix");
    endif    
  else
    error ("usage: mesh (z, levels, x, y)");
  endif

endfunction
