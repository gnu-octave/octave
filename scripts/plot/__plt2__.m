# Copyright (C) 1994, 1995 John W. Eaton
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

function plot_int_2 (x1, x2, fmt)

  if (nargin < 2 || nargin > 3)
    usage ("plot_int_2 (x1, x2, fmt)");
  endif

  if (nargin == 2)
    fmt = "";
  endif

  if (! isstr (fmt))
    error ("plot_int_2: fmt must be a string");
  endif

  if (any (any (imag (x1))))
    x1 = real (x1);
  endif
  if (any (any (imag (x2))))
    x2 = real (x2);
  endif
  if (is_scalar (x1))
    if (is_scalar (x2))
      plot_2_s_s (x1, x2, fmt);
    endif
  elseif (is_vector (x1))
    if (is_vector (x2))
      plot_2_v_v (x1, x2, fmt);
    elseif (is_matrix (x2))
      plot_2_v_m (x1, x2, fmt);
    endif
  elseif (is_matrix (x1))
    if (is_vector (x2))
      plot_2_m_v (x1, x2, fmt);
    elseif (is_matrix (x2))
      plot_2_m_m (x1, x2, fmt);
    endif
  endif

endfunction
