## Copyright (C) 1995, 1998 Auburn University.  All rights reserved.
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by the
## Free Software Foundation; either version 2, or (at your option) any
## later version.
##
## Octave is distributed in the hope that it will be useful, but WITHOUT
## ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
## FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
## for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, 59 Temple Place, Suite 330, Boston, MA 02111 USA.

## -*- texinfo -*-
## @deftypefn {Function File} polyout (@var{c}, @var{x})
## Write formatted polynomial
## @example
##    c(x) = c(1) * x^n + ... + c(n) x + c(n+1)
## @end example
##  and return it as a string or write it to the screen (if
##  @var{nargout} is zero).
##  @var{x} defaults to the string @code{"s"}
## @end deftypefn
## @seealso{polyval, polyvalm, poly, roots, conv, deconv, residue,
## filter, polyderiv, and polyinteg}

## Author: A. S. Hodel <a.s.hodel@eng.auburn.edu>
## Created: May 1995
## Nov 1998: Correctly handles complex coefficients

function y = polyout (c, x)

  if (nargin < 1) || (nargin > 2) || (nargout < 0) || (nargout > 1)
    usage("polyout (c, x)");
  endif

  if (! is_vector (c))
    error("polyout: first argument must be a vector");
  endif

  if (nargin == 1)
    x = "s";
  elseif (! isstr(x))
    error("polyout: second argument must be a string");
  endif

  n = length(c);
  if(n > 0)
    n1 = n+1;

    if (imag (c(1)))
      tmp = com2str(c(1))
    else
      tmp = num2str(c(1));
    endif

    for ii = 2:n
      if (real (c(ii)) < 0)
	ns = " - ";
	c(ii) = -c(ii);
      else
        ns = " + ";
      endif

      if (imag (c(ii)))
	nstr = sprintf ("(%s)", com2str (c(ii)));
      else
        nstr = num2str (c(ii));
      endif

      tmp = sprintf ("%s*%s^%d%s%s", tmp, x, n1-ii, ns, nstr);

    endfor
  else
    tmp = " ";
  endif

  if(nargout == 0)
    disp(tmp)
  else
    y = tmp;
  endif

endfunction
