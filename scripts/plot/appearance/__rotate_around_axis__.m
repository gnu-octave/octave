########################################################################
##
## Copyright (C) 2014-2023 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.
##
########################################################################

## -*- texinfo -*-
## @deftypefn {} {[@var{xr}, @var{yr}, @var{zr}] =} __rotate_around_axis__ (@var{x}, @var{y}, @var{z}, @var{angle}, @var{dir}, @var{origin})
## Rotate the points given by X, Y, Z about an axis by ANGLE degrees.
## The axis is specified by the vector DIR and the point ORIGIN.
## @end deftypefn

function [xr, yr, zr] = __rotate_around_axis__ (x, y, z, angle, dir, origin)

  dir /= norm (dir);
  u = dir(1);
  v = dir(2);
  w = dir(3);

  a = origin(1);
  b = origin(2);
  c = origin(3);

  sa = sind (angle);
  ca = cosd (angle);

  if (a == 0 && b == 0 && c == 0)
    tmp = (u*x + v*y + w*z) * (1 - ca);

    xr = u*tmp + x*ca + (-w*y + v*z)*sa;
    yr = v*tmp + y*ca + (w*x - u*z)*sa;
    zr = w*tmp + z*ca + (-v*x + u*y)*sa;
  else
    one_m_ca = 1 - ca;
    tmp = u*x + v*y + w*z;

    xr = ((a*(v^2 + w^2) - u*(b*v + c*w - tmp))*one_m_ca
          + x*ca + (-c*v + b*w - w*y + v*z)*sa);
    yr = ((b*(u^2 + w^2) - v*(a*u + c*w - tmp))*one_m_ca
          + y*ca + (c*u - a*w + w*x - u*z)*sa);
    zr = ((c*(u^2 + v^2) - w*(a*u + b*v - tmp))*one_m_ca
          + z*ca + (-b*u + a*v - v*x + u*y)*sa);
  endif

endfunction
