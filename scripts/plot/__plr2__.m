# Copyright (C) 1993, 1994 John W. Eaton
# 
# This file is part of Octave.
# 
# Octave is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 2, or (at your option) any
# later version.
# 
# Octave is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
# 
# You should have received a copy of the GNU General Public License
# along with Octave; see the file COPYING.  If not, write to the Free
# Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

function polar_int_2 (theta, rho, fmt)

  if (nargin != 3)
    usage ("polar_int_2 (theta, rho, fmt)");
  endif

  if (any (imag (theta)))
    theta = real (theta);
  endif

  if (any (imag (rho)))
    rho = real (rho);
  endif

  if (is_scalar (theta))
    if (is_scalar (rho))
      x = rho * cos (theta);
      y = rho * sin (theta);
      plot_2_s_s (x, y, fmt);
    endif
  elseif (is_vector (theta))
    if (is_vector (rho))
      if (length (theta) != length (rho))
	error ("polar: vector lengths must match");
      endif
      if (rows (rho) == 1)
	rho = rho';
      endif
      if (rows (theta) == 1)
	theta = theta';
      endif
      x = rho .* cos (theta);
      y = rho .* sin (theta);
      plot_2_v_v (x, y, fmt);
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
	error ("polar: vector and matrix sizes must match");
      endif
      x = diag (cos (theta)) * rho;
      y = diag (sin (theta)) * rho;
      plot_2_v_m (x, y, fmt);
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
	error ("polar: vector and matrix sizes must match");
      endif
      diag_r = diag (r);
      x = diag_r * cos (theta);
      y = diag_r * sin (theta);
      plot_2_m_v (x, y, fmt);
    elseif (is_matrix (rho))
      if (size (rho) != size (theta))
	error ("polar: matrix dimensions must match");
      endif
      x = rho .* cos (theta);
      y = rho .* sin (theta);
      plot_2_m_m (x, y, fmt);
    endif
  endif

endfunction
