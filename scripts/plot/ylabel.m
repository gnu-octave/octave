function ylabel (text)

# usage: ylabel (text)
#
# Defines a label for the y-axis of a plot.  The label will appear the
# next time a plot is displayed.
#
# See also: plot, semilogx, semilogy, loglog, polar, mesh, contour,
#           bar, stairs, gplot, gsplot, replot, xlabel, title

  if (nargin != 1)
    error ("usage: ylabel (text)");
  endif

  if (isstr (text))
    command = sprintf ("set ylabel \"%s\"", text);
    eval (command);
  else
    error ("error: ylabel: text must be a string");
  endif

endfunction
