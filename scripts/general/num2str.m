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
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} int2str (@var{n})
## @deftypefnx {Function File} {} num2str (@var{x}, @var{precision})
## @deftypefnx {Function File} {} num2str (@var{x}, @var{format})
## Convert a number to a string.  These functions are not very flexible,
## but are provided for compatibility with @sc{Matlab}.  For better control
## over the results, use @code{sprintf} (@pxref{Formatted Output}).
## @end deftypefn
##
## @seealso{sprintf and int2str}

## Author: jwe

function retval = num2str (x, arg)

  if (nargin != 1 && nargin != 2)
    usage ("num2str (x) or num2str (x, precision) or num2str (x, fmt)");
  endif

  if (isstr (x))
    retval = x;
  endif

  if (iscomplex (x))
    if (nargin == 2)
      if (isstr (arg))
	fmt = strcat (arg, "%-+", arg(2:end), "i");
      else
	if (isnumeric (x) && round (x) == x && abs (x) < (10 .^ arg))
	  fmt = sprintf ("%%%dd%%-+%ddi  ", arg, arg);
	else
	  fmt = sprintf ("%%%d.%dg%%-+%d.%dgi", arg+7, arg, arg+7, arg);
	endif
      endif
    else
      ## Setup a suitable format string
      if (isnumeric (x) && round (x) == x && abs (x) < 1e10)
	dgt1 = ceil (log10 (max (max (abs (real (x(:)))),
				 max (abs (imag (x(:))))))) + 1;
	dgt2 = dgt1 - (min (real (x(:))) >= 0);
	fmt = sprintf("%%%dd%%+-%ddi  ", dgt2, dgt1);
      elseif (isscalar (x))
	fmt = "%.4g%-+.4gi";
      else
	fmt = "%11.4g%-+11.4gi";
      endif
    endif

    ## Manipulate the complex value to have real values in the odd
    ## columns and imaginary values in the even columns.
    sz = size (x);
    nc = sz(2);
    nd = ndims (x);
    perm = fix ([1:0.5:nc+0.5]);
    perm(2:2:2*nc) = perm(2:2:2*nc) + nc;
    idx = cell ();
    for i = 1:nd
      idx {i} = 1:sz(i);
    endfor
    idx{2} = perm;
    x = horzcat (real(x), imag(x));
    x = x(idx{:});
    
    fmt = strcat (deblank (repmat (fmt, 1, nc)), "\n");
    tmp = sprintf (fmt, permute (x, [2, 1, 3:nd]));

    ## Put the "i"'s where they are supposed to be.
    while (true)
      tmp2 = strrep (tmp, " i\n", "i\n");
      if (length (tmp) == length (tmp2))
	break;
      else
	tmp = tmp2;
      endif
    endwhile
    while (true)
      tmp2 = strrep (tmp, " i", "i ");
      if (tmp == tmp2)
	break;
      else
	tmp = tmp2;
      endif
    endwhile

    tmp(length (tmp)) = "";
    retval = split (tmp, "\n");
  else
    if (nargin == 2)
      if (isstr (arg))
	fmt = arg;
      else
	if (isnumeric (x) && round (x) == x && abs (x) < (10 .^ arg))
	  fmt = sprintf ("%%%dd  ", arg);
	else
	  fmt = sprintf ("%%%d.%dg", arg+7, arg);
	endif
      endif
    else
      if (isnumeric (x) && round (x) == x && abs (x) < 1e10)
	if (max (abs (x(:))) == 0)
	  dgt = 1;
	else
	  dgt = floor (log10 (max (abs(x(:))))) + (min (real (x(:))) < 0) + 1;
	endif
	fmt = sprintf ("%%%dd  ", dgt);
      elseif (isscalar (x))
	fmt = "%.4g";
      else
	fmt = "%11.4g";
      endif
    endif
    fmt = strcat (deblank (repmat (fmt, 1, columns (x))), "\n");
    nd = ndims (x);
    tmp = sprintf (fmt, permute (x, [2, 1, 3:nd]));
    tmp(length (tmp)) = "";
    retval = split (tmp, "\n");
  endif

endfunction
