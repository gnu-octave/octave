function polar_int (theta, rho)

  if (nargin == 1)
    [nr, nc] = size (theta);
    if (nr == 1)
      theta = theta';
      tmp = nr;
      nr = nc;
      nc = tmp;
    endif
    theta_i = imag (theta);
    if (theta_i)
      rho = theta_i;
      theta = real (theta);
    else
      rho = theta;
      theta = (1:nr)';
    endif
  endif

  if (nargin <= 2)
    if (imag (theta))
      theta = real (theta);
    endif
    if (imag (rho))
      rho = real (rho);
    endif
    if (is_scalar (theta))
      if (is_scalar (rho))
        x = rho * cos (theta);
        y = rho * sin (theta);
        plot_2_s_s (x, y);
      endif
    elseif (is_vector (theta))
      if (is_vector (rho))
        if (length (theta) != length (rho))
          error ("error: polar: vector lengths must match");
        endif
        if (rows (rho) == 1)
          rho = rho';
        endif
        if (rows (theta) == 1)
          theta = theta';
        endif
        x = rho .* cos (theta);
        y = rho .* sin (theta);
        plot_2_v_v (x, y);
      elseif (is_matrix (rho))
        [t_nr, t_nc] = size (theta);
        if (t_nr == 1)
          theta = theta';
          tmp = t_nr;
          t_nr = t_nc;
          t_nc = tmp;
        endif
        [r_nr, r_nc] = size (rho);
        if (t_nr != r_nr)
          rho = rho'
          tmp = r_nr;
          r_nr = r_nc;
          r_nc = tmp;
        endif
        if (t_nr != r_nr)
          error ("error: polar: vector and matrix sizes must match");
        endif
        x = diag (cos (theta)) * rho;
        y = diag (sin (theta)) * rho;
        plot_2_v_m (x, y);
      endif
    elseif (is_matrix (theta))
      if (is_vector (rho))
        [r_nr, r_nc] = size (rho);
        if (r_nr == 1)
          rho = rho';
          tmp = r_nr;
          r_nr = r_nc;
          r_nc = tmp;
        endif
        [t_nr, t_nc] = size (theta);
        if (r_nr != t_nr)
          theta = rho'
          tmp = t_nr;
          t_nr = t_nc;
          t_nc = tmp;
        endif
        if (r_nr != t_nr)
          error ("error: polar: vector and matrix sizes must match");
        endif
        diag_r = diag (r);
        x = diag_r * cos (theta);
        y = diag_r * sin (theta);
        plot_2_m_v (x, y);
      elseif (is_matrix (rho))
        if (size (rho) != size (theta))
          error ("error: polar: matrix dimensions must match");
        endif
        x = rho .* cos (theta);
        y = rho .* sin (theta);
        plot_2_m_m (x, y);
      endif
    endif
  else
    usage = sprintf ("usage: polar_int (x)\n");
    usage = sprintf ("%s       polar_int (x, y)", usage);
    error (usage);
  endif

endfunction
