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
## Software Foundation, 59 Temple Place - Suite 330, Boston, MA
## 02111-1307, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} __errplot__ (@var{args})
## Really plot errorbar plots. User interface in function errorbar.
##
## @example
## __errplot__ (@var{arg1}, @var{arg2}, ..., @var{fmt})
## @end example
##
## @end deftypefn
##
## @seealso{semilogx, semilogy, loglog, polar, mesh, contour, __pltopt__
## bar, stairs, errorbar, gplot, gsplot, replot, xlabel, ylabel, and title}

## Created: 18.7.2000
## Author: Teemu Ikonen <tpikonen@pcu.helsinki.fi>
## Keywords: errorbar, plotting

function __errplot__ (fstr,a1,a2,a3,a4,a5,a6)

  if (nargin < 3 || nargin > 7) # at least three data arguments needed
    usage ("__errplot__ (fmt, arg1, ...)");
  endif

  fmt = __pltopt__ ("__errplot__", fstr);

  nplots = size (a1, 2);
  len = size (a1, 1);
  for i = 1:nplots
    ifmt = fmt(1+mod(i,size(fmt,1)), :);
    switch (nargin - 1)
      case 2
	tmp = [(1:len)', a1(:,i), a2(:,i)];
      case 3
	tmp = [a1(:,i), a2(:,i), a3(:,i)];
      case 4
	if (index (ifmt, "boxxy") || index (ifmt, "xyerr"))
	  tmp = [a1(:,i), a2(:,i), a3(:,i), a4(:,i)];
	elseif (index (ifmt, "xerr"))
	  tmp = [a1(:,i), a2(:,i), a1(:,i)-a3(:,i), a1(:,i)+a4(:,i)];
	else
	  tmp = [a1(:,i), a2(:,i), a2(:,i)-a3(:,i), a2(:,i)+a4(:,i)];
	endif
      case 5
	error ("error plot requires 2, 3, 4 or 6 columns");
	## tmp = [a1(:,i), a2(:,i), a3(:,i), a4(:,i), a5(:,i)];
      case 6
	tmp = [a1(:,i), a2(:,i), ...
	       a1(:,i)-a3(:,i), a1(:,i)+a4(:,i), ...
	       a2(:,i)-a5(:,i), a2(:,i)+a6(:,i)];
    endswitch
    cmd = sprintf ("gplot tmp %s", ifmt);
    eval (cmd);
endfor

endfunction
