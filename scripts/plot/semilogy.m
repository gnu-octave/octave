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
## @deftypefn {Function File} {} semilogy (@var{args})
## Make a two-dimensional plot using a log scale for the @var{y} axis.  See
## the description of @code{plot} for a description of the arguments
## that @code{semilogy} will accept.
## @end deftypefn
##
## @seealso{plot, semilogx, loglog, polar, mesh, contour, bar, stairs,
## gplot, gsplot, replot, xlabel, ylabel, and title}

## Author: jwe

function semilogy (varargin)

  ## XXX FIXME XXX -- these plot states should really just be set
  ## temporarily, probably inside an unwind_protect block, but there is
  ## no way to determine their current values.

  gset nologscale x;
  gset logscale y;
  gset nopolar;

  __plt__ ("semilogy", varargin{:});

endfunction
