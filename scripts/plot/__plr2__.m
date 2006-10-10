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
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} __plr2__ (@var{theta}, @var{rho}, @var{fmt})
## @end deftypefn

## Author: jwe

function __plr2__ (theta, rho, fmt)

  if (nargin != 3)
    print_usage ();
  endif

  if (any (imag (theta)))
    theta = real (theta);
  endif

  if (any (imag (rho)))
    rho = real (rho);
  endif

  if (isscalar (theta))
    if (isscalar (rho))
      x = rho * cos (theta);
      y = rho * sin (theta);
      __plt__ ("polar", x, y, fmt);
    else
      error ("__plr2__: invalid data for plotting");
    endif
  elseif (isvector (theta))
    if (isvector (rho))
      if (length (theta) != length (rho))
        error ("__plr2__: vector lengths must match");
      endif
      if (rows (rho) == 1)
        rho = rho';
      endif
      if (rows (theta) == 1)
        theta = theta';
      endif
      x = rho .* cos (theta);
      y = rho .* sin (theta);
      __plt__ ("polar", x, y, fmt);
    elseif (ismatrix (rho))
      [t_nr, t_nc] = size (theta);
      if (t_nr == 1)
        theta = theta';
        tmp = t_nr;
        t_nr = t_nc;
        t_nc = tmp;
      endif
      [r_nr, r_nc] = size (rho);
      if (t_nr != r_nr)
        rho = rho';
        tmp = r_nr;
        r_nr = r_nc;
        r_nc = tmp;
      endif
      if (t_nr != r_nr)
        error ("__plr2__: vector and matrix sizes must match");
      endif
      x = diag (cos (theta)) * rho;
      y = diag (sin (theta)) * rho;
      __plt__ ("polar", x, y, fmt);
    else
      error ("__plr2__: invalid data for plotting")
    endif
  elseif (ismatrix (theta))
    if (isvector (rho))
      [r_nr, r_nc] = size (rho);
      if (r_nr == 1)
        rho = rho';
        tmp = r_nr;
        r_nr = r_nc;
        r_nc = tmp;
      endif
      [t_nr, t_nc] = size (theta);
      if (r_nr != t_nr)
        theta = theta';
        tmp = t_nr;
        t_nr = t_nc;
        t_nc = tmp;
      endif
      if (r_nr != t_nr)
        error ("__plr2__: vector and matrix sizes must match");
      endif
      diag_r = diag (rho);
      x = diag_r * cos (theta);
      y = diag_r * sin (theta);
      __plt__ ("polar", x, y, fmt);
    elseif (ismatrix (rho))
      if (size (rho) != size (theta))
        error ("__plr2__: matrix dimensions must match");
      endif
      x = rho .* cos (theta);
      y = rho .* sin (theta);
      __plt__ ("polar", x, y, fmt);
    else
      error ("__plr2__: invalid data for plotting")
    endif
  else
    error ("__plr2__: invalid data for plotting")
  endif

endfunction
