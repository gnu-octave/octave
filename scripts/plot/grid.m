function grid (x)

# usage: grid ("on" | "off")
#
# Turn grid lines on or off for plotting.
#
# If the argument is omitted, "on" is assumed.
#
# See also: plot, semilogx, semilogy, loglog, polar, mesh, contour,
#           bar, stairs, gplot, gsplot, replot, xlabel, ylabel, title 

  if (nargin == 0)
    set grid;
  elseif (nargin == 1)
    if (isstr (x))
      if (strcmp ("off", x))
        set nogrid;
      elseif (strcmp ("on", x))
        set grid;
      else
        error ("usage: grid (\"on\" | \"off\")");
      endif
    else
      error ("error: grid: argument must be a string");
    endif
  else
    error ("usage: grid (\"on\" | \"off\")");
  endif

endfunction
