function title (text)

# usage: title (text)
#
# Defines a title for a plot.  The title will appear the next time a
# plot is displayed. 
#
# See also: plot, semilogx, semilogy, loglog, polar, mesh, contour,
#           bar, stairs, gplot, gsplot, replot, xlabel, ylabel

  if (nargin != 1)
    error ("usage: title (text)");
  endif

  if (isstr (text))
    command = sprintf ("set title \"%s\"", text);
    eval (command);
  else
    error ("error: title: text must be a string");
  endif

endfunction
