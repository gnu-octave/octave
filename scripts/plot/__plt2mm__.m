function plot_2_m_m (x, y)

  if (nargin != 2)
    error ("usage: plot_2_m_m (x, y)");
  endif

  [x_nr, x_nc] = size (x);
  [y_nr, y_nc] = size (y);

  if (x_nr == y_nr && x_nc == y_nc)
    if (x_nc > 0)
      tmp = [x, y];
      command = sprintf ("gplot tmp(:,%d:%d:%d)", 1, x_nc, x_nc+1);
      for i = 2:x_nc
        command = sprintf ("%s, tmp(:,%d:%d:%d)", command, i, x_nc, x_nc+i);
      endfor
      eval (command);
    else
      error ("plot_2_m_m: arguments must be a matrices");
    endif
  else
    error ("plot_2_m_m: matrix dimensions must match");
  endif

endfunction
