function xlabel (text)

# usage: xlabel (text)
#
# Defines a label for the x-axis of a plot.  The label will appear the
# next time a plot is displayed.
#
# See also: plot, semilogx, semilogy, loglog, polar, mesh, contour,
#           bar, stairs, gplot, gsplot, replot, ylabel, title

  if (nargin != 1)
    error ("usage: xlabel (text)");
  endif

  if (isstr (text))
    command = sprintf ("set xlabel \"%s\"", text);
    eval (command);
  else
    error ("error: xlabel: text must be a string");
  endif

endfunction
