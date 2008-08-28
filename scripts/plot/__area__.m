## Copyright (C) 2007 David Bateman
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

## Undocumented internal function.

function retval = __area__ (ax, x, y, bv, varargin)

  y0 = bv * ones (1, rows (y));
  y0 = zeros (1, rows (y));
  retval = [];
  for i = 1: size (y, 2);
    hg = hggroup ();
    retval = [retval; hg];
    args = __add_datasource__ ("area", hg, {"x", "y"}, varargin{:});

    x1 = x(:, 1).';
    y1 = y (:, i).';
    addproperty ("xdata", hg, "data", x1);
    addproperty ("ydata", hg, "data", y1);

    addlistener (hg, "xdata", @update_data);
    addlistener (hg, "ydata", @update_data);

    if (i == 1)
      h = patch (ax, [x1(1), x1, fliplr(x1)], [bv, y1, bv*ones(1, length(y1))],
		 __next_line_color__ (), "parent", hg, args{:});
    else
      y1 = y0 + y1;
      h = patch (ax, [x1(1), x1, fliplr(x1)], [y0(1), y1, fliplr(y0)],
		 __next_line_color__ (), "parent", hg, args{:});
    endif

    y0 = y1;

    addproperty ("basevalue", hg, "data", bv);
    addlistener (hg, "basevalue", @move_baseline); 

    addproperty ("edgecolor", hg, "patchedgecolor", get (h, "edgecolor"));
    addproperty ("linewidth", hg, "patchlinewidth", get (h, "linewidth"));
    addproperty ("linestyle", hg, "patchlinestyle", get (h, "linestyle"));
    addproperty ("facecolor", hg, "patchfacecolor", get (h, "facecolor"));

    addlistener (hg, "edgecolor", @update_props);
    addlistener (hg, "linewidth", @update_props); 
    addlistener (hg, "linestyle", @update_props); 
    addlistener (hg, "facecolor", @update_props); 

    addproperty ("areagroup", hg, "data");
    set (retval, "areagroup", retval);
  endfor

endfunction

function update_props (h, d)
  kids = get (h, "children");
  set (kids, "edgecolor", get (h, "edgecolor"), 
       "linewidth", get (h, "linewidth"),
       "linestyle", get (h, "linestyle"),
       "facecolor", get (h, "facecolor"));
endfunction

function move_baseline (h, d)
  persistent recursion = false;

  ## Don't allow recursion
  if (! recursion)
    unwind_protect
      recursion = true;
      hlist = get (h, "areagroup");
      b0 = get (h, "basevalue");

      for hh = hlist(:)'
	if (hh != h )
	  b1 = get (hh, "basevalue");
	  if (b1 != b0)
	    set (hh, "basevalue", b0);
	  endif
	endif
      endfor
      update_data (h, d);
    unwind_protect_cleanup
      recursion = false;
    end_unwind_protect
  endif
endfunction

function update_data (h, d)
  hlist = get (h, "areagroup");
  bv = get (h, "basevalue");
  for i = 1 : length (hlist)
    hh = hlist(i);
    x1 = get (hh, "xdata")(:);
    y1 = get (hh, "ydata")(:);

    set (get (hh, "children"), "xdata", [x1(1); x1; flipud(x1)]);
    if (i == 1)
      set (get (hh, "children"), "ydata", [bv; y1; bv*ones(length(y1), 1)]);
    else
      y1 = y0 + y1;
      set (get (hh, "children"), "ydata", [y0(1); y1; flipud(y0)]);
    endif      

    y0 = y1;
  endfor
endfunction
