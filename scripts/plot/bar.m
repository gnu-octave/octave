function [xb, yb] = bar (x, y)

# usage: [xb, yb] = bar (x, y)
#
# Given two vectors of x-y data, bar produces a `bar' graph.
#
# If only one argument is given, it is taken as a vector of y-values
# and the x coordinates are taken to be the indices of the elements.
#
# If two output arguments are specified, the data are generated but
# not plotted.  For example,
#
#   bar (x, y);
#
# and
#
#   [xb, yb] = bar (x, y);
#   plot (xb, yb);
#
# are equivalent.
#
# See also: plot, semilogx, semilogy, loglog, polar, mesh, contour,
#           stairs, gplot, gsplot, replot, xlabel, ylabel, title 

  if (nargin == 1)
    if (is_vector (x))
      len = 3 * length (x) + 1;
      xb = yb = zeros (len, 1);
      xb(1) = 0.5;
      yb(1) = 0;
      k = 1;
      for i = 2:3:len
        xb(i) = k-0.5;
        xb(i+1) = k+0.5;
        xb(i+2) = k+0.5;
        yb(i) = x(k);
        yb(i+1) = x(k);
        yb(i+2) = 0.0;
        k++;
      endfor
    else
      error ("bar: argument must be a vector");
    endif
  elseif (nargin == 2)
    if (is_vector (x) && is_vector (y))
      xlen = length (x);
      ylen = length (y);
      if (xlen == ylen)
        len = 3 * xlen + 1;
        xb = yb = zeros (len, 1);
        delta = (x(2) - x(1)) / 2.0;
        xb(1) = x(1) - delta;
        yb(1) = 0.0;
	k = 1;
        for i = 2:3:len
          xb(i) = xb(i-1);
          xb(i+1) = xb(i) + 2.0 * delta;
          xb(i+2) = xb(i+1);
	  yb(i) = y(k);
	  yb(i+1) = y(k);
	  yb(i+2) = 0.0;
          if (k < xlen)
            delta = (x(k+1) - x(k)) / 2.0;
            if (x(k+1) < x(k))
              error ("bar: x vector values must be in ascending order");
            endif
          endif
          k++;
	endfor
      else
        error ("bar: arguments must be the same length");
      endif
    else
      error ("bar: arguments must be vectors");
    endif
  else
    error ("usage: [xb, yb] = bar (x, y)");
  endif

  if (nargout == 1)
    plot (xb, yb);
  endif

endfunction
