function plot_2_v_v (x, y)

  if (nargin != 2)
    error ("usage: plot_2_m_m (x, y)");
  endif

  [x_nr, x_nc] = size (x);
  [y_nr, y_nc] = size (y);

  if (x_nr == 1)
    x = x';
    tmp = x_nr;
    x_nr = x_nc;
    x_nc = tmp;
  endif

  if (y_nr == 1)
    y = y';
    tmp = y_nr;
    y_nr = y_nc;
    y_nc = tmp;
  endif

  if (x_nr != y_nr)
    error ("plot_2_v_v: vector lengths must match");
  endif

  tmp = [x, y];
  eval ("gplot tmp");

endfunction
