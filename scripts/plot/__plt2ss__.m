function plot_2_s_s (x, y)

  if (nargin != 2)
    error ("usage: plot_2_s_s (x, y)");
  endif

  [x_nr, x_nc] = size (x);
  [y_nr, y_nc] = size (y);

  if (x_nr == 1 && x_nr == y_nr && x_nc == 1 && x_nc == y_nc)
    tmp = [x, y];
    command = sprintf ("gplot tmp");
    eval ("gplot tmp");
  else
    error ("plot_2_s_s: arguments must be scalars");
  endif

endfunction
