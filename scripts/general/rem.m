## Copyright (C) 1993, 1994, 1995, 1996, 1997, 1999, 2000, 2002, 2004,
##               2005, 2006, 2007, 2008, 2009 John W. Eaton
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
## @deftypefn {Mapping Function} {} rem (@var{x}, @var{y})
## Return the remainder of the division @code{@var{x} / @var{y}}, computed 
## using the expression
##
## @example
## x - y .* fix (x ./ y)
## @end example
##
## An error message is printed if the dimensions of the arguments do not
## agree, or if either of the arguments is complex.
## @seealso{mod, fmod}
## @end deftypefn

## Author: jwe

function r = rem (x, y)

  if (nargin != 2)
    print_usage ();
  endif

  if (! size_equal (x, y) && ! (isscalar (x) || isscalar (y)))
    error ("rem: argument sizes must agree");
  endif

  if (isreal (x) && isreal (y))
      if (isinteger(x) || isinteger(y))
	if (isinteger (x))
	  typ = class (x);
	else
	  typ = class (y);
	endif
	r = x - y .* cast (fix (double (x) ./ double (y)), typ);
      else
	r = x - y .* fix (x ./ y);
      endif
  else
    error ("rem: complex arguments are not allowed");
  endif

endfunction

%!assert(rem ([1, 2, 3; -1, -2, -3], 2), [1, 0, 1; -1, 0, -1]);

%!assert(rem ([1, 2, 3; -1, -2, -3], 2 * ones (2, 3)),[1, 0, 1; -1, 0, -1]);

%!error rem ();

%!error rem (1, 2, 3);

%!error rem ([1, 2], [3, 4, 5]);

%!error rem (i, 1);

%!assert(rem (uint8([1, 2, 3; -1, -2, -3]), uint8 (2)), uint8([1, 0, 1; -1, 0, -1]));

%!assert(uint8(rem ([1, 2, 3; -1, -2, -3], 2 * ones (2, 3))),uint8([1, 0, 1; -1, 0, -1]));

%!error rem (uint(8),int8(5));

%!error rem (uint8([1, 2]), uint8([3, 4, 5]));
