function polar (x1, x2)

# usage: polar (theta, rho)
#
# Make a 2D plot given polar the coordinates theta and rho.
#
# See also: plot, semilogx, semilogy, loglog, mesh, contour, bar,
#           stairs, gplot, gsplot, replot, xlabel, ylabel, title 

  set nologscale;
  set nopolar;

  if (nargin == 1)
    polar_int (x1);
  elseif (nargin == 2)
    polar_int (x1, x2);
  else
    usage = sprintf ("usage: polar (x)\n");
    usage = sprintf ("%s       polar (x, y)", usage);
    error (usage);
  endif

endfunction
