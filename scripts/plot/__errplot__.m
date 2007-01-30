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
## bar, stairs, errorbar, replot, xlabel, ylabel, title}
## @end deftypefn

## Created: 18.7.2000
## Author: Teemu Ikonen <tpikonen@pcu.helsinki.fi>
## Keywords: errorbar, plotting

function __errplot__ (fstr, h, a1, a2, a3, a4, a5, a6)

  if (nargin < 4 || nargin > 8) # at least two data arguments needed
    print_usage ();
  endif

  [fmt, key] = __pltopt__ ("__errplot__", fstr);

  [len, nplots] = size (a1);

  for i = 1:nplots
    ifmt = fmt{1+mod(i-1,numel(fmt))};
    s = __uiobject_line_ctor__ (h);
    switch (nargin - 2)
      case 2
	s.xdata = (1:len)';
	s.ydata = a1(:,i);
	s.ldata = a2(:,i);
	s.udata = a2(:,i);
      case 3
	s.xdata = a1(:,i);
	s.ydata = a2(:,i);
	s.ldata = a3(:,i);
	s.udata = a3(:,i);
      case 4
	s.xdata = a1(:,i);
	s.ydata = a2(:,i);

	if (index (ifmt, "boxxy") || index (ifmt, "xyerr"))
	  s.xldata = a3(:,i);
	  s.xudata = a3(:,i);
	  s.ldata = a4(:,i);
	  s.udata = a4(:,i);
	elseif (index (ifmt, "xerr"))
	  s.xldata = a3(:,i);
	  s.xudata = a4(:,i);
	else
	  s.ldata = a3(:,i);
	  s.udata = a4(:,i);
	endif
      case 5
	error ("error plot requires 2, 3, 4 or 6 columns");
      case 6
	s.xdata = a1(:,i);
	s.ydata = a2(:,i);
	s.xldata = a3(:,i);
	s.xudata = a4(:,i);
	s.ldata = a5(:,i);
	s.udata = a6(:,i);
    endswitch

    __uiobject_adopt__ (h, __uiobject_make_handle__ (s));

  endfor

endfunction
