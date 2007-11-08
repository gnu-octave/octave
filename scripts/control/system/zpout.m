## Copyright (C) 1996, 2000, 2002, 2003, 2004, 2005, 2006, 2007
##               Auburn University.  All rights reserved.
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
## @deftypefn {Function File} {} zpout (@var{zer}, @var{pol}, @var{k}, @var{x})
## print formatted zero-pole form to the screen.
## @var{x} defaults to the string @code{"s"}
## @seealso{polyval, polyvalm, poly, roots, conv, deconv, residue,
## filter, polyderiv, polyinteg, polyout}
## @end deftypefn

## Author: A. S. Hodel <a.s.hodel@eng.auburn.edu>
## Created: June 1995

function zpout (zer, pol, k, x)

  if (nargin < 3 || nargin > 4)
    print_usage ();
  endif

  if (! (isvector (zer) || isempty (zer))
      || ! (isvector (pol) || isempty(pol)))
    error ("zer, pol must be vectors or empty");
  endif

  if (! isscalar(k))
    error("zpout: argument k must be a scalar.")
  endif

  if (nargin == 3)
    x = "s";
  elseif (! ischar (x))
    error ("zpout: third argument must be a string");
  endif

  numstring = num2str (k);

  if (length (zer))
    ## find roots at z,s = 0
    nzr = sum (zer == 0);
    if (nzr)
      if (nzr > 1)
	numstring = sprintf ("%s %s^%d", numstring, x, nzr);
      else
	numstring = strcat (numstring, x);
      endif
    endif
    zer = sortcom (-zer);
    for ii = 1:length(zer)
      if (zer(ii) != 0)
	numstring = sprintf ("%s (%s %s)", numstring, x, com2str (zer(ii), 1));
      endif
    endfor
  endif

  if (length (pol))
    ## find roots at z,s = 0
    nzr = sum (pol == 0);
    if (nzr)
      if (nzr > 1)
	denomstring = sprintf("%s^%d", x, nzr);
      else
	denomstring = sprintf ("%s", x);
      endif
    else
      denomstring = " ";
    endif
    pol = sortcom (-pol);
    for ii = 1:length(pol)
      if (pol(ii) != 0)
	denomstring = sprintf ("%s (%s %s)", denomstring, x,
			       com2str (pol(ii), 1));
      endif
    endfor
  endif

  len = max (length (numstring), length (denomstring));
  if(len > 0)
    y = strrep (blanks (len), " ", "-");
    disp (numstring)
    if (length (denomstring))
	disp (y)
	disp (denomstring)
    endif
  else
    error ("zpout: empty transfer function")
  endif

endfunction
