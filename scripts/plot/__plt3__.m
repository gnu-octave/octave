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
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} __plt3__ (@var{x}, @var{y}, @var{z}, @var{fmt})
## @end deftypefn

## Author: Paul Kienzle <kienzle.powernet.co.uk>
## 2001-04-06 Paul Kienzle <kienzle.powernet.co.uk>
##     * __gnuplot_set__ nohidden3d; vector X,Y, matrix Z => meshgrid(X,Y)

## Modified to use new gnuplot interface in octave > 2.9.0
## Dmitri A. Sergatskov <dasergatskov@gmail.com>
## April 18, 2005
## Modified to use NaN as seperator for gnuplot, so multiple calls
## aren't needed.
## David Bateman <dbateman@free.fr>
## May 25, 2006

function __plt3__ (x, y, z, fmt)

  if (isvector(x) && isvector(y))
    if (isvector(z))
      x = x(:); y=y(:); z=z(:);
    elseif (length(x) == rows(z) && length(y) == columns(z))
      error("plot3: [length(x), length(y)] must match size(z)");
    else
      [x,y] = meshgrid(x,y);
    endif
  endif

  if (any(size(x) != size(y)) || any(size(x) != size(z)))
    error("plot3: x, y, and z must have the same shape");
  endif

  unwind_protect
    __gnuplot_set__  parametric;
    __gnuplot_raw__ ("set nohidden3d;\n");

    tmp = [([x; NaN*ones(1,size(x,2))])(:), ...
	   ([y; NaN*ones(1,size(y,2))])(:), ...
	   ([z; NaN*ones(1,size(z,2))])(:)];

    cmd =  ["__gnuplot_splot__ tmp ", fmt, ";\n"];
    eval (cmd);
  unwind_protect_cleanup
    __gnuplot_set__ noparametric; 
  end_unwind_protect
endfunction
