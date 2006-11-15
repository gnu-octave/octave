## Copyright (C) 2006 John W. Eaton
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

function __init_plot_vars__ (clear_data)

  __plot_globals__;

  cf = __current_figure__;
  mxi = __multiplot_xi__(cf);
  myi = __multiplot_yi__(cf);

  if (nargin < 1)
    clear_data = true;
  endif

  if (clear_data)
    __plot_data__{cf}{mxi,myi} = [];
    __plot_data_offset__{cf}(mxi,myi) = 1;
    __plot_data_type__{cf}{mxi,myi} = [];
    __plot_data_parametric__{cf}{mxi,myi} = [];
    __plot_image_colormap__{cf}{mxi,myi} = [];
    __plot_image_dims__{cf}{mxi,myi} = [];
    __plot_fmtstr__{cf}{mxi,myi} = [];
    __plot_usingstr__{cf}{mxi,myi} = [];
    __plot_withstr__{cf}{mxi,myi} = [];
  endif

endfunction
