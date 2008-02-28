## Copyright (C) 1993, 1994, 1995, 1996, 1997, 1999, 2000, 2002, 2003,
##               2004, 2005, 2006, 2007 John W. Eaton
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
## @deftypefn {Function File} {} num2str (@var{n})
## @deftypefnx {Function File} {} num2str (@var{x}, @var{precision})
## @deftypefnx {Function File} {} num2str (@var{x}, @var{format})
## Convert a number to a string.  This function is not very flexible.
## For better control over the results, use @code{sprintf}
## (@pxref{Formatted Output}).
## @seealso{sprintf, int2str}
## @end deftypefn

## Author: jwe

function retval = num2str (x, arg)

  if (nargin != 1 && nargin != 2)
    print_usage ();
  endif

  if (ischar (x))
    retval = x;
  elseif (isempty (x))
    retval = "";
  elseif (iscomplex (x))
    if (nargin == 2)
      if (ischar (arg))
	fmt = cstrcat (arg, "%-+", arg(2:end), "i");
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
	if (max (abs (real (x(:)))) == 0)
	  dgt1 = 2;
	else
	  dgt1 = ceil (log10 (max (max (abs (real (x(:)))),
				   max (abs (imag (x(:))))))) + 2;
	endif
	dgt2 = dgt1 - (min (real (x(:))) >= 0);
	
	if (length (abs (x) == x) > 0)
	  fmt = sprintf("%%%dg%%+-%dgi  ", dgt2, dgt1);
	else
	  fmt = sprintf("%%%dd%%+-%ddi  ", dgt2, dgt1);
	endif
      elseif (isscalar (x))
	fmt = "%.6g%-+.6gi";
      else
	fmt = "%11.6g%-+11.6gi";
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
      idx{i} = 1:sz(i);
    endfor
    idx{2} = perm;
    x = horzcat (real (x), imag (x));
    x = x(idx{:});

    fmt = cstrcat (deblank (repmat (fmt, 1, nc)), "\n");
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
    retval = strtrim (split (tmp, "\n"));
  else
    if (nargin == 2)
      if (ischar (arg))
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
	  dgt = 2;
	else
	  dgt = floor (log10 (max (abs(x(:))))) + (min (real (x(:))) < 0) + 2;
	endif
	if (length (abs (x) == x) > 0)
	  fmt = sprintf ("%%%dg  ", dgt);
	else
	  fmt = sprintf ("%%%dd  ", dgt);
	endif
      elseif (isscalar (x))
	fmt = "%11.5g";
      else
	fmt = "%11.5g";
      endif
    endif
    fmt = cstrcat (deblank (repmat (fmt, 1, columns (x))), "\n");
    nd = ndims (x);
    tmp = sprintf (fmt, permute (x, [2, 1, 3:nd]));
    tmp(length (tmp)) = "";
    retval = strtrim (split (tmp, "\n"));
  endif

endfunction

%!assert((strcmp (num2str (123), "123") && strcmp (num2str (1.23), "1.23")));

%!error num2str ();

%!error num2str (1, 2, 3);

