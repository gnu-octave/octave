function plot_2_m_v (x, y)

  if (nargin != 2)
    error ("usage: plot_2_m_v (x, y)");
  endif

  [x_nr, x_nc] = size (x);
  [y_nr, y_nc] = size (y);

  if (y_nr == 1)
    y = y';
    tmp = y_nr;
    y_nr = y_nc;
    y_nc = tmp;
  endif

  if (x_nr == y_nr)
    1;
  elseif (x_nc == y_nr)
    x = x';
    tmp = x_nr;
    x_nr = x_nc;
    x_nc = tmp;
  else
    error ("plot_2_m_v: matrix dimensions must match");
  endif

  if (x_nc > 0)
    tmp = [x, y];
    command = sprintf ("gplot tmp(:,%d:%d:%d)", 1, x_nc, x_nc+1);
    for i = 2:x_nc
      command = sprintf ("%s, tmp(:,%d:%d:%d)", command, i, x_nc-i+1, x_nc+1);
    endfor
    eval (command);
  else
    error ("plot_2_m_v: arguments must be a matrices");
  endif

endfunction
