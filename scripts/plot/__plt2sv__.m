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

## -*- texinfo -*-
## @deftypefn {Function File} {} __plt2sv__ (@var{h}, @var{x}, @var{y}, @var{options}, @var{properties})
## Undocumented internal function.
## @end deftypefn

## Author: jwe

function retval = __plt2sv__ (h, x, y, options, properties)

  if (nargin < 3 || nargin > 5)
    print_usage ();
  endif

  if (nargin < 4 || isempty (options))
    options = __default_plot_options__ ();
  endif

  if (nargin < 5)
    properties = {};
  endif

  if (isscalar (x) && isvector (y))
    len = numel (y);
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

      retval(i) = line (x, y(i), "keylabel", tkey, "color", color,
			"linestyle", options(i).linestyle,
			"marker", options(i).marker, properties{:});
    endfor
  else
    error ("__plt2sv__: first arg must be scalar, second arg must be vector");
  endif

endfunction
