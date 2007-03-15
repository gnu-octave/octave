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
## @deftypefn {Function File} {} xlabel (@var{string})
## @deftypefnx {Function File} {} ylabel (@var{string})
## @deftypefnx {Function File} {} zlabel (@var{string})
## Specify x, y, and z axis labels for the plot.  If you already have a plot
## displayed, use the command @code{replot} to redisplay it with the new
## labels.
## @seealso{plot, semilogx, semilogy, loglog, polar, mesh, contour,
## bar, stairs, replot, ylabel, title}
## @end deftypefn

## Author: jwe

function h = xlabel (varargin)

  if (rem (nargin, 2) == 1)
    if (nargout > 0)
      h = __axis_label__ ("xlabel", varargin{:});
    else
      __axis_label__ ("xlabel", varargin{:});
    endif
  else
    print_usage ();
  endif

endfunction
