function semilogx (x1, x2)

# usage: semilogx (x, y)
#
# Make a 2D plot of y versus x using a log scale for the x axis. 
#
# See the help message for the plot command for a description of how
# the arguments are interpreted. 
#
# See also: plot, semilogy, loglog, polar, mesh, contour, bar, stairs,
#           gplot, gsplot, replot, xlabel, ylabel, title 

  set logscale x;
  set nologscale y;
  set nopolar;

  if (nargin == 1)
    plot_int (x1);
  elseif (nargin == 2)
    plot_int (x1, x2);
  else
    usage = sprintf ("usage: semilogx (x)\n");
    usage = sprintf ("%s       semilogx (x, y)", usage);
    error (usage);
  endif

endfunction
