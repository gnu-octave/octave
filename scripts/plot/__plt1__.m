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
# Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

function plot_int_1 (x1, fmt)

  if (nargin < 1 || nargin > 2)
    usage ("plot_int_1 (x1, fmt)");
  endif

  if (nargin == 1)
    fmt = "";
  endif

  if (! isstr (fmt))
    error ("plot_int_1: fmt must be a string");
  endif

  [nr, nc] = size (x1);
  if (nr == 1)
    x1 = x1.';
    tmp = nr;
    nr = nc;
    nc = tmp;
  endif
  x1_i = imag (x1);
  if (any (any (x1_i)))
    x2 = x1_i;
    x1 = real (x1);
  else
    x2 = x1;
    x1 = (1:nr)';
  endif

  plot_int_2 (x1, x2, fmt);

endfunction
