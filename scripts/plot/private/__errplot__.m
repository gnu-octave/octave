## Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009
##               Teemu Ikonen
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

## -*- texinfo -*-
## @deftypefn {Function File} {@var{h} =} __errplot__ (@var{fstr}, @var{p}, @dots{})
## Undocumented internal function.
## @end deftypefn

## Created: 18.7.2000
## Author: Teemu Ikonen <tpikonen@pcu.helsinki.fi>
## Keywords: errorbar, plotting

function h = __errplot__ (fstr, p, a1, a2, a3, a4, a5, a6)

  if (nargin < 4 || nargin > 8) # at least two data arguments needed
    print_usage ();
  endif

  [fmt, key] = __pltopt__ ("__errplot__", fstr);

  [len, nplots] = size (a1);
  h = [];

  for i = 1:nplots
    ## Set the plot type based on linestyle.

    if (strcmp (fmt.linestyle, "~"))
      ifmt = "yerr";
    elseif (strcmp (fmt.linestyle, ">"))
      ifmt = "xerr";
    elseif (strcmp (fmt.linestyle, "~>"))
      ifmt = "xyerr";
    elseif (strcmp (fmt.linestyle, "#"))
      ifmt = "box";
    elseif (strcmp (fmt.linestyle, "#~"))
      ifmt = "boxy";
    elseif (strcmp (fmt.linestyle, "#~>"))
      ifmt = "boxxy";
    else
      print_usage ();
    endif

    hg = hggroup ("parent", p);
    h = [h; hg];
    args = __add_datasource__ ("__errplot__", hg, 
			       {"x", "y", "l", "u", "xl", "xu"});

    if (isempty (fmt.color))
      hl = __line__ (hg, "color", __next_line_color__ ());
    else
      hl = __line__ (hg, "color", fmt.color);
    endif

    ## FIXME -- note the code below adds the errorbar data directly as
    ## ldata, etc properties of the line objects, as gnuplot can handle
    ## this.  Matlab has the errorbar part of the plot as a special line
    ## object with embedded NaNs that draws the three segments of the
    ## bar separately.  Should we duplicate Matlab's behavior and stop
    ## using the ldata, etc. properties of the line objects that are
    ## Octace specific?

    switch (nargin - 2)
      case 1
	error ("error plot requires 2, 3, 4 or 6 columns");
      case 2
	set (hl, "xdata", (1:len)');
	set (hl, "ydata", a1(:,i));
	set (hl, "ldata", a2(:,i));
	set (hl, "udata", a2(:,i));
      case 3
	set (hl, "xdata", a1(:,i));
	set (hl, "ydata", a2(:,i));
	set (hl, "ldata", a3(:,i));
	set (hl, "udata", a3(:,i));
      case 4
	set (hl, "xdata", a1(:,i));
	set (hl, "ydata", a2(:,i));

	if (index (ifmt, "boxxy") || index (ifmt, "xyerr"))
	  set (hl, "xldata", a3(:,i));
	  set (hl, "xudata", a3(:,i));
	  set (hl, "ldata", a4(:,i));
	  set (hl, "udata", a4(:,i));
	elseif (index (ifmt, "xerr"))
	  set (hl, "xldata", a3(:,i));
	  set (hl, "xudata", a4(:,i));
	else
	  set (hl, "ldata", a3(:,i));
	  set (hl, "udata", a4(:,i));
	endif
      case 5
	error ("error plot requires 2, 3, 4 or 6 columns");
      case 6
	set (hl, "xdata", a1(:,i));
	set (hl, "ydata", a2(:,i));
	set (hl, "xldata", a3(:,i));
	set (hl, "xudata", a4(:,i));
	set (hl, "ldata", a5(:,i));
	set (hl, "udata", a6(:,i));
    endswitch

    addproperty ("color", hg, "linecolor", get (hl, "color"));
    addproperty ("linewidth", hg, "linelinewidth", get (hl, "linewidth"));
    addproperty ("linestyle", hg, "linelinestyle", get (hl, "linestyle"));
    addproperty ("marker", hg, "linemarker", get (hl, "marker"));
    addproperty ("markerfacecolor", hg, "linemarkerfacecolor", 
		 get (hl, "markerfacecolor"));
    addproperty ("markeredgecolor", hg, "linemarkerfacecolor", 
		 get (hl, "markeredgecolor"));
    addproperty ("markersize", hg, "linemarkersize", 
		 get (hl, "markersize"));

    addlistener (hg, "color", @update_props);
    addlistener (hg, "linewidth", @update_props); 
    addlistener (hg, "linestyle", @update_props); 
    addlistener (hg, "marker", @update_props); 
    addlistener (hg, "markerfacecolor", @update_props); 
    addlistener (hg, "markersize", @update_props);

    addproperty ("xdata", hg, "data", get (hl, "xdata"));
    addproperty ("ydata", hg, "data", get (hl, "ydata"));
    addproperty ("ldata", hg, "data", get (hl, "ldata"));
    addproperty ("udata", hg, "data", get (hl, "udata"));
    addproperty ("xldata", hg, "data", get (hl, "xldata"));
    addproperty ("xudata", hg, "data", get (hl, "xudata"));

    addlistener (hg, "xdata", @update_data);
    addlistener (hg, "ydata", @update_data);
    addlistener (hg, "ldata", @update_data);
    addlistener (hg, "udata", @update_data);
    addlistener (hg, "xldata", @update_data);
    addlistener (hg, "xudata", @update_data);

    __line__ (hg, "xdata", get (hl, "xdata"), 
	      "ydata", get (hl, "ydata"), 
	      "color", get (hl, "color"),
	      "linewidth", get (hl, "linewidth"),
	      "linestyle", get (hl, "linestyle"), 
	      "marker", "none", "parent", hg);
  endfor

endfunction

function update_props (h, d)
  set (get (h, "children"), "color", get (h, "color"), 
       "linewidth", get (h, "linewidth"), "linestyle", get (h, "linestyle"), 
       "marker", get (h, "marker"), "markersize", get (h, "markersize"),
       "markerfacecolor", get (h, "markerfacecolor"),
       "markeredgecolor", get (h, "markeredgecolor"));
endfunction

function update_data (h, d)
  x = get (h, "xdata");
  y = get (h, "ydata");
  l = get (h, "ldata");
  u = get (h, "udata");
  xl = get (h, "xldata");
  xu = get (h, "xudata");

  kids = get (h, "children");
  set (kids(1), "xdata", x, "ydata", y);
  set (kids(2), "xdata", x, "ydata", y, "ldata", l, "udata", u, 
       "xldata", xl, "xudata", xu);
endfunction
