function plot_2_v_m (x, y)

  if (nargin != 2)
    error ("usage: plot_2_v_m (x, y)");
  endif

  [x_nr, x_nc] = size (x);
  [y_nr, y_nc] = size (y);

  if (x_nr == 1)
    x = x';
    tmp = x_nr;
    x_nr = x_nc;
    x_nc = tmp;
  endif

  if (x_nr == y_nr)
    1;
  elseif (x_nr == y_nc)
    y = y';
    tmp = y_nr;
    y_nr = y_nc;
    y_nc = tmp;
  else
    error ("plot_2_v_m: matrix dimensions must match");
  endif

  if (y_nc > 0)
    tmp = [x, y];
    command = sprintf ("gplot tmp(:,%d:%d:%d)", 1, x_nc, x_nc+1);
    for i = 2:y_nc
      command = sprintf ("%s, tmp(:,%d:%d:%d)", command, 1, i, i+1);
    endfor
    eval (command);
  else
    error ("plot_2_v_m: arguments must be a matrices");
  endif

endfunction
