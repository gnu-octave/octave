function semilogy (x1, x2)

# usage: semilogy (x, y)
#
# Make a 2D plot of y versus x using a log scale for the y axis. 
#
# See the help message for the plot command for a description of how
# the arguments are interpreted. 
#
# See also: plot, semilogx, loglog, polar, mesh, contour, bar, stairs,
#           gplot, gsplot, replot, xlabel, ylabel, title 

  set nologscale x;
  set logscale y;
  set nopolar;

  if (nargin == 1)
    plot_int (x1);
  elseif (nargin == 2)
    plot_int (x1, x2);
  else
    usage = sprintf ("usage: semilogy (x)\n");
    usage = sprintf ("%s       semilogy (x, y)", usage);
    error (usage);
  endif

endfunction
