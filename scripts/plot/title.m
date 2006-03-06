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
## @deftypefn {Function File} {} title (@var{string})
## Specify a title for a plot.
## @seealso{plot, semilogx, semilogy, loglog, polar, mesh, contour,
## bar, stairs, replot, xlabel, ylabel}
## @end deftypefn

## Author: jwe

function h = title (text)

  if (nargin != 1)
    usage ("title (text)");
  endif

  if (ischar (text))
    __gnuplot_raw__ (sprintf ("set title \"%s\";\n",
			      undo_string_escapes (text)));
    if (automatic_replot)
      replot ();
    endif
  else
    error ("title: text must be a string");
  endif

  ## XXX FIXME XXX -- eventually, we will return a graphics handle.  For
  ## now, return something, so that calls that expect a handle won't
  ## fail (at least immediately).

  if (nargout > 0)
    h = -1;
  endif

endfunction
