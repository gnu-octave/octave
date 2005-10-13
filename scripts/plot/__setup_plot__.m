## Copyright (C) 2005 John W. Eaton
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

if (ishold ())
  if (isempty (__plot_command__{__current_figure__}{__multiplot_xi__,__multiplot_yi__}))
    __plot_command__{__current_figure__}{__multiplot_xi__,__multiplot_yi__} = "__gnuplot_plot__";
    __plot_command_sep__ = "";
  else
    __plot_command_sep__ = ",\\\n";
  endif
else
  __plot_command__{__current_figure__}{__multiplot_xi__,__multiplot_yi__} = "__gnuplot_plot__";
  __plot_command_sep__ = "";
  __plot_data__{__current_figure__}{__multiplot_xi__,__multiplot_yi__} = [];
  __plot_data_offset__{__current_figure__}(__multiplot_xi__,__multiplot_yi__) = 1;
endif
