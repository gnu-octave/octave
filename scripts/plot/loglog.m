function loglog (x1, x2)

# usage: loglog (x, y)
#
# Make a 2D plot of y versus x using log scales for both axes.
#
# See the help message for the plot command for a description of how
# the arguments are interpreted. 
#
# See also: plot, semilogx, semilogy, polar, mesh, contour, bar, stairs,
#           gplot, gsplot, replot, xlabel, ylabel, title 


  set logscale x;
  set logscale y;
  set nopolar;

  if (nargin == 1)
    plot_int (x1);
  elseif (nargin == 2)
    plot_int (x1, x2);
  else
    usage = sprintf ("usage: loglog (x)\n");
    usage = sprintf ("%s       loglog (x, y)", usage);
    error (usage);
  endif

endfunction
