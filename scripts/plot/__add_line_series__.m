## Copyright (C) 2008 David Bateman
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## Undocumented internal function

function  __add_line_series__ (h, hg)

  obj = get(h);

  addproperty ("color", hg, "linecolor", obj.color);
  addproperty ("linewidth", hg, "linelinewidth", obj.linewidth);
  addproperty ("linestyle", hg, "linelinestyle", obj.linestyle);
  addproperty ("marker", hg, "linemarker", obj.marker);
  addproperty ("markeredgecolor", hg, "linemarkerfacecolor", 
	       obj.markeredgecolor);
  addproperty ("markerfacecolor", hg, "linemarkerfacecolor", 
	       obj.markerfacecolor);
  addproperty ("markersize", hg, "linemarkersize", obj.markersize);
      
  addlistener (hg, "color", @update_props);
  addlistener (hg, "linewidth", @update_props); 
  addlistener (hg, "linestyle", @update_props); 
  addlistener (hg, "marker", @update_props); 
  addlistener (hg, "markeredgecolor", @update_props); 
  addlistener (hg, "markerfacecolor", @update_props); 
  addlistener (hg, "markersize", @update_props);

  addproperty ("xdata", hg, "data", obj.xdata);
  addproperty ("ydata", hg, "data", obj.ydata);
  addproperty ("zdata", hg, "data", obj.zdata);

  addlistener (hg, "xdata", @update_props);
  addlistener (hg, "ydata", @update_props);
  addlistener (hg, "zdata", @update_props);
endfunction

function update_props (h, d)
  set (get (h, "children"), "color", get (h, "color"), 
       "linewidth", get (h, "linewidth"),
       "linestyle", get (h, "linestyle"),
       "marker", get (h, "marker"),
       "markerfacecolor", get (h, "markerfacecolor"),
       "markeredgecolor", get (h, "markeredgecolor"),
       "markersize", get (h, "markersize"),
       "xdata", get (h, "xdata"),
       "ydata", get (h, "ydata"),
       "zdata", get (h, "zdata"));
endfunction

