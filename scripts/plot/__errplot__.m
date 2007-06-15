## Copyright (C) 2000, 2001, 2002 Teemu Ikonen
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

## -*- texinfo -*-
## @deftypefn {Function File} {} __errplot__ (@var{args})
## Really plot errorbar plots. User interface in function errorbar.
##
## @example
## __errplot__ (@var{arg1}, @var{arg2}, ..., @var{fmt})
## @end example
##
## @seealso{semilogx, semilogy, loglog, polar, mesh, contour, __pltopt__,
## bar, stairs, errorbar, xlabel, ylabel, title}
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

  for i = 1:nplots
    ## Set the plot type based on linestyle.
    if (fmt.linestyle == "~")
      ifmt = "yerr";
    elseif (fmt.linestyle == ">")
      ifmt = "xerr";
    elseif (fmt.linestyle == "~>")
      ifmt = "xyerr";
    elseif (fmt.linestyle == "#")
      ifmt = "box";
    elseif (fmt.linestyle == "#~")
      ifmt = "boxy";
    elseif (fmt.linestyle == "#~>")
      ifmt = "boxxy";
    else
      print_usage ();
    endif

    h = __line__ (p);

    switch (nargin - 2)
      case 2
	set (h, "xdata", (1:len)');
	set (h, "ydata", a1(:,i));
	set (h, "ldata", a2(:,i));
	set (h, "udata", a2(:,i));
      case 3
	set (h, "xdata", a1(:,i));
	set (h, "ydata", a2(:,i));
	set (h, "ldata", a3(:,i));
	set (h, "udata", a3(:,i));
      case 4
	set (h, "xdata", a1(:,i));
	set (h, "ydata", a2(:,i));

	if (index (ifmt, "boxxy") || index (ifmt, "xyerr"))
	  set (h, "xldata", a3(:,i));
	  set (h, "xudata", a3(:,i));
	  set (h, "ldata", a4(:,i));
	  set (h, "udata", a4(:,i));
	elseif (index (ifmt, "xerr"))
	  set (h, "xldata", a3(:,i));
	  set (h, "xudata", a4(:,i));
	else
	  set (h, "ldata", a3(:,i));
	  set (h, "udata", a4(:,i));
	endif
      case 5
	error ("error plot requires 2, 3, 4 or 6 columns");
      case 6
	set (h, "xdata", a1(:,i));
	set (h, "ydata", a2(:,i));
	set (h, "xldata", a3(:,i));
	set (h, "xudata", a4(:,i));
	set (h, "ldata", a5(:,i));
	set (h, "udata", a6(:,i));
    endswitch
  endfor

endfunction
