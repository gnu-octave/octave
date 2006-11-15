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

global __current_figure__;
global __plot_data_offset__;
global __plot_line_offset__;
global __plot_command__;
global __plot_command_sep__;
global __plot_data__;
global __plot_key_labels__;
global __plot_key_properties__;
global __multiplot_mode__;
global __multiplot_xsize__;
global __multiplot_ysize__;
global __multiplot_xn__;
global __multiplot_yn__;
global __multiplot_xi__;
global __multiplot_yi__;

if (isempty (__current_figure__))
  __current_figure__ = 1;
endif

if (length (__multiplot_mode__) < __current_figure__)
  __multiplot_mode__(__current_figure__) = false;
endif

if (length (__multiplot_xsize__) < __current_figure__)
  __multiplot_xsize__(__current_figure__) = 1;
endif

if (length (__multiplot_ysize__) < __current_figure__)
  __multiplot_ysize__(__current_figure__) = 1;
endif

if (length (__multiplot_xn__) < __current_figure__)
  __multiplot_xn__(__current_figure__) = 1;
endif

if (length (__multiplot_yn__) < __current_figure__)
  __multiplot_yn__(__current_figure__) = 1;
endif

if (length (__multiplot_xi__) < __current_figure__)
  __multiplot_xi__(__current_figure__) = 1;
endif

if (length (__multiplot_yi__) < __current_figure__)
  __multiplot_yi__(__current_figure__) = 1;
endif

if (length (__plot_data_offset__) < __current_figure__
    || any (size (__plot_data_offset__{__current_figure__}) != [__multiplot_xi__(__current_figure__), __multiplot_yi__(__current_figure__)]))

  __plot_data_offset__{__current_figure__}(__multiplot_xi__(__current_figure__),__multiplot_yi__(__current_figure__)) = 1;
endif

if (length (__plot_line_offset__) < __current_figure__
    || any (size (__plot_line_offset__{__current_figure__}) != [__multiplot_xi__(__current_figure__), __multiplot_yi__(__current_figure__)]))

  __plot_line_offset__{__current_figure__}(__multiplot_xi__(__current_figure__),__multiplot_yi__(__current_figure__)) = 1;
endif

if (length (__plot_command__) < __current_figure__
    || any (size (__plot_command__{__current_figure__}) != [__multiplot_xi__(__current_figure__), __multiplot_yi__(__current_figure__)]))
  __plot_command__{__current_figure__}{__multiplot_xi__(__current_figure__),__multiplot_yi__(__current_figure__)} = "";
endif

if (length (__plot_data__) < __current_figure__
    || any (size (__plot_data__{__current_figure__}) != [__multiplot_xi__(__current_figure__), __multiplot_yi__(__current_figure__)]))
  __plot_data__{__current_figure__}{__multiplot_xi__(__current_figure__),__multiplot_yi__(__current_figure__)} = [];
endif

if (length (__plot_key_labels__) < __current_figure__
    || any (size (__plot_key_labels__{__current_figure__}) != [__multiplot_xi__(__current_figure__), __multiplot_yi__(__current_figure__)]))
  __plot_key_labels__{__current_figure__}{__multiplot_xi__(__current_figure__),__multiplot_yi__(__current_figure__)} = [];
endif

if (length (__plot_key_properties__) < __current_figure__
    || any (size (__plot_key_properties__{__current_figure__}) != [__multiplot_xi__(__current_figure__), __multiplot_yi__(__current_figure__)]))
  __plot_key_properties__{__current_figure__}{__multiplot_xi__(__current_figure__),__multiplot_yi__(__current_figure__)} = struct ("visible", true, "box", false, "position", 0);
endif
