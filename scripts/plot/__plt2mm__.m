## Copyright (C) 1996 John W. Eaton
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, 59 Temple Place - Suite 330, Boston, MA
## 02111-1307, USA.

function plot_2_m_m (x, y, fmt)

  if (nargin < 2 || nargin > 3)
    msg = sprintf ("plot_2_m_m (x, y)\n");
    msg = sprintf ("%s              plot_2_m_m (x, y, fmt)", msg);
    usage (msg);
  elseif (nargin == 2)
    fmt = "";
  endif

  [x_nr, x_nc] = size (x);
  [y_nr, y_nc] = size (y);

  if (x_nr == y_nr && x_nc == y_nc)
    if (x_nc > 0)
      tmp = [x, y];
      cmd = sprintf ("gplot tmp(:,%d:%d:%d) %s", 1, x_nc, x_nc+1, fmt);
      for i = 2:x_nc
        cmd = sprintf ("%s, tmp(:,%d:%d:%d) %s", cmd, i, x_nc, x_nc+i, fmt);
      endfor
      eval (cmd);
    else
      error ("plot_2_m_m: arguments must be a matrices");
    endif
  else
    error ("plot_2_m_m: matrix dimensions must match");
  endif

endfunction
