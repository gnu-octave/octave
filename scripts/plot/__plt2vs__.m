## Copyright (C) 1993, 1994, 1995, 1996, 1997, 1998, 2000, 2003, 2005,
##               2006, 2007 John W. Eaton
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

## Author: jwe

function retval = __plt2vs__ (h, x, y, options, properties)

  if (nargin < 3 || nargin > 5)
    print_usage ();
  endif

  if (nargin < 4 || isempty (options))
    options = __default_plot_options__ ();
  endif

  if (nargin < 5)
    properties = {};
  endif

  if (isvector (x) && isscalar (y))
    len = numel (x);
    if (numel (options) == 1)
      options = repmat (options(:), len, 1);
    endif
    retval = zeros (len, 1);
    for i = 1:len
      tkey = options(i).key;
      if (! isempty (tkey))
	set (h, "key", "on");
      endif
      color = options(i).color;
      if (isempty (color))
	color = __next_line_color__ ();
      endif

      hg = hggroup ();
      retval(i) = hg;
      args = __add_datasource__ ("__plt2vs__", hg, {"x", "y", "z"}, 
				 properties{:});

      h = line (x(i), y, "keylabel", tkey, "color", color,
		"linestyle", options(i).linestyle,
		"marker", options(i).marker, "parent", hg);

      __add_line_series__ (h, hg);
      if (! isempty (args))
        set (hg, args{:});
      endif
    endfor
  else
    error ("__plt2vs__: first arg must be vector, second arg must be scalar");
  endif

endfunction
