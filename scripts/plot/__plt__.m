function plot_int (x1, x2)

  if (nargin == 1)
    [nr, nc] = size (x1);
    if (nr == 1)
      x1 = x1';
      tmp = nr;
      nr = nc;
      nc = tmp;
    endif
    x1_i = imag (x1);
    if (x1_i)
      x2 = x1_i;
      x1 = real (x1);
    else
      x2 = x1;
      x1 = (1:nr)';
    endif
  endif

  if (nargin <= 2)
    if (imag (x1))
      x1 = real (x1);
    endif
    if (imag (x2))
      x2 = real (x2);
    endif
    if (is_scalar (x1))
      if (is_scalar (x2))
        plot_2_s_s (x1, x2);
      endif
    elseif (is_vector (x1))
      if (is_vector (x2))
        plot_2_v_v (x1, x2);
      elseif (is_matrix (x2))
        plot_2_v_m (x1, x2);
      endif
    elseif (is_matrix (x1))
      if (is_vector (x2))
        plot_2_m_v (x1, x2);
      elseif (is_matrix (x2))
        plot_2_m_m (x1, x2);
      endif
    endif
  else
    usage = sprintf ("usage: plot_int (x)\n");
    usage = sprintf ("%s       plot_int (x, y)", usage);
    error (usage);
  endif

endfunction
