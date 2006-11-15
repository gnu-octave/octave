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

function __do_legend__ ()

  __plot_globals__;

  cf = __current_figure__;
  mxi = __multiplot_xi__(cf);
  myi = __multiplot_yi__(cf);

  props = __plot_key_properties__{cf}{mxi,myi};

  if (isstruct (props))
    if (isfield (props, "visible"))
      visible = props.visible;
    else
      error ("__do_legend__: missing field \"visible\"");
    endif
    if (isfield (props, "box"))
      box = props.box;
    else
      error ("__do_legend__: missing field \"box\"");
    endif
    if (isfield (props, "position"))
      position = props.position;
    else
      error ("__do_legend__: missing field \"position\"");
    endif
    if (visible)
      switch (position)
	case 1
	  __gnuplot_raw__ ("set  key right top;\n")
	case 2
	  __gnuplot_raw__ ("set  key left top;\n")
	case 3
	  __gnuplot_raw__ ("set  key left bottom;\n")
	case 4
	  __gnuplot_raw__ ("set  key right bottom;\n")
	case -1
	  __gnuplot_raw__ ("set  key right top outside;\n")
	case -2
	  __gnuplot_raw__ ("set  key right bottom outside;\n")
	case -3
	  __gnuplot_raw__ ("set  key below;\n")
      endswitch
      if (box)
        __gnuplot_raw__ ("set key box;\n")
      else
        __gnuplot_raw__ ("set key nobox;\n")
      endif
    else
      __gnuplot_raw__ ("unset key;\n")
    endif
  else
    error ("__do_legend__: expecting properties to be a struct");
  endif

endfunction
