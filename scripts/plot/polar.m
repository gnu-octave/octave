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
## @deftypefn {Function File} {} polar (@var{theta}, @var{rho}, @var{fmt})
## Make a two-dimensional plot given polar the coordinates @var{theta} and
## @var{rho}.
##
## The optional third argument specifies the line type.
## @end deftypefn
##
## @seealso{plot, semilogx, semilogy, loglog, mesh, contour, bar,
## stairs, replot, xlabel, ylabel, and title}

## Author: jwe

function polar (x1, x2, fmt)

  ## XXX FIXME XXX -- these plot states should really just be set
  ## temporarily, probably inside an unwind_protect block, but there is
  ## no way to determine their current values.

  __gnuplot_raw__ ("set nologscale;\n");
  __gnuplot_raw__ ("set nopolar;\n");

  if (nargin == 3)
    if (isstr (fmt))
      fmt = __pltopt__ ("polar", fmt);
    else
      error ("polar: third argument must be a string");
    endif
    __plr2__ (x1, x2, fmt);
  elseif (nargin == 2)
    if (isstr (x2))
      fmt = __pltopt__ ("polar", x2);
      __plr1__ (x1, fmt);
    else
      fmt = "";
      __plr2__ (x1, x2, fmt);
    endif
  elseif (nargin == 1)
    fmt = "";
    __plr1__ (x1, fmt);
  else
    usage ("polar (theta, rho, fmt)");
  endif

endfunction
