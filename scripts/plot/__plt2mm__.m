## Copyright (C) 1996, 1997 John W. Eaton
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

## -*- texinfo -*-
## @deftypefn {Function File} {} __plt2mm__ (@var{x}, @var{y}, @var{fmt})
## @end deftypefn

## Author: jwe

function __plt2mm__ (x, y, fmt)

  if (nargin < 2 || nargin > 3)
    msg = sprintf ("__plt2mm__ (x, y)\n");
    msg = sprintf ("%s              __plt2mm__ (x, y, fmt)", msg);
    usage (msg);
  elseif (nargin == 2 || fmt == "")
    fmt = " ";  ## Yes, this is intentionally not an empty string!
  endif

  [x_nr, x_nc] = size (x);
  [y_nr, y_nc] = size (y);

  k = 1;
  fmt_nr = rows (fmt);
  if (x_nr == y_nr && x_nc == y_nc)
    if (x_nc > 0)
      tmp = [x, y];
      cmd = sprintf ("gplot tmp(:,%d:%d:%d) %s", 1, x_nc, x_nc+1,
                     undo_string_escapes (deblank (fmt (k, :))));
      if (k < fmt_nr)
        k++;
      endif
      for i = 2:x_nc
        cmd = sprintf ("%s, tmp(:,%d:%d:%d) %s", cmd, i, x_nc, x_nc+i,
                       undo_string_escapes (deblank (fmt (k, :))));
        if (k < fmt_nr)
          k++;
        endif
      endfor
      eval (cmd);
    else
      error ("__plt2mm__: arguments must be a matrices");
    endif
  else
    error ("__plt2mm__: matrix dimensions must match");
  endif

endfunction
