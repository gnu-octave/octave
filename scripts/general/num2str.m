## Copyright (C) 1996, 1997 John W. Eaton
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
## @deftypefn {Function File} {} int2str (@var{n})
## @deftypefnx {Function File} {} num2str (@var{x}, @var{precision})
## @deftypefnx {Function File} {} num2str (@var{x}, @var{format})
## Convert a number to a string.  These functions are not very flexible,
## but are provided for compatibility with @sc{Matlab}.  For better control
## over the results, use @code{sprintf} (@pxref{Formatted Output}).
## @end deftypefn
## @seealso{sprintf and int2str}

## Author: jwe

function retval = num2str (x, arg)

  if (nargin == 1 || nargin == 2)
    if (nargin == 2)
      if (isstr (arg))
	fmt = arg;
      else
	fmt = sprintf ("%%.%dg", arg);
      endif
    else
      fmt = "%11.4g";
    endif
    if (iscomplex (x))
      error ("num2str: sorry, can't handle complex numbers yet");
    else
      fmt = strcat (repmat (fmt, 1, columns (x)), "\n");
      tmp = sprintf (fmt, x.');
      tmp(length (tmp)) = "";
      retval = split (tmp, "\n");
    endif
  else
    usage ("num2str (x) or num2str (x, precision) or num2str (x, fmt)");
  endif

endfunction
