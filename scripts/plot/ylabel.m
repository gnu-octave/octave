# Copyright (C) 1993 John W. Eaton
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
# Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

function ylabel (text)

# usage: ylabel (text)
#
# Defines a label for the y-axis of a plot.  The label will appear the
# next time a plot is displayed.
#
# See also: plot, semilogx, semilogy, loglog, polar, mesh, contour,
#           bar, stairs, gplot, gsplot, replot, xlabel, title

  if (nargin != 1)
    error ("usage: ylabel (text)");
  endif

  if (isstr (text))
    command = sprintf ("set ylabel \"%s\"", text);
    eval (command);
  else
    error ("error: ylabel: text must be a string");
  endif

endfunction
