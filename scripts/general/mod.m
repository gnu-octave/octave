## Copyright (C) 1999, 2000, 2002, 2004, 2005, 2006, 2007, 2008 Paul Kienzle
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
## @deftypefn {Mapping Function} {} mod (@var{x}, @var{y})
## Compute modulo function. Conceptually this is given by
##
## @example
## x - y .* floor (x ./ y)
## @end example
##
## and is written in a manner that the correct modulus is returned for
##integer types. This function handles negative values correctly. That
##is @code{mod (-1, 3)} is 2, not -1 as @code{rem (-1, 3)} returns.
## Also, @code{mod (@var{x}, 0)} returns @var{x}.
##
## An error message is printed if the dimensions of the arguments do not
## agree, or if either of the arguments is complex.
## @seealso{rem, round}
## @end deftypefn

## Author: Paul Kienzle <pkienzle@kienzle.powernet.co.uk>
## Modified by: Teemu Ikonen <tpikonen@pcu.helsinki.fi>
## Adapted by: jwe

function r = mod (x, y)

  if (nargin != 2)
    print_usage ();
  endif

  if (! size_equal (x, y) && ! (isscalar (x) || isscalar (y)))
    error ("mod: argument sizes must agree");
  endif

  if (isreal (x) && isreal (y))
    nz = y != 0.0;
    if (all (nz(:)))
      ## No elements of y are zero.
      if (isinteger(x) || isinteger(y))
	if (isinteger (x))
	  typ = class (x);
	else
	  typ = class (y);
	endif
	r = x - y .* cast (floor (double(x) ./ double(y)), typ);
      else
	r = x - y .* floor (x ./ y);
      endif
    elseif (isscalar (y))
      ## y must be zero.
      r = x;
    else
      ## Some elements of y are zero.
      if (isscalar (x))
	r = x * ones (size(y), class(y));
      else
	r = x;
	x = x(nz);
      endif
      y = y(nz);
      if (isinteger(x) || isinteger(y))
	if (isinteger (x))
	  typ = class (x);
	else
	  typ = class (y);
	endif
	r(nz) = x - y .* floor (double(x) ./ double(y));
      else
	r(nz) = x - y .* floor (x ./ y);
      endif
    endif
  else
    error ("mod: complex arguments are not allowed");
  endif

endfunction
  
## empty input test
%!assert (isempty(mod([], [])));

## x mod y, y != 0 tests
%!assert (mod(5, 3), 2);
%!assert (mod(-5, 3), 1);
%!assert (mod(0, 3), 0);
%!assert (mod([-5, 5, 0], [3, 3, 3]), [1, 2, 0]);
%!assert (mod([-5; 5; 0], [3; 3; 3]), [1; 2; 0]);
%!assert (mod([-5, 5; 0, 3], [3, 3 ; 3, 1]), [1, 2 ; 0, 0]);

## x mod 0 tests
%!assert (mod(5, 0), 5);
%!assert (mod(-5, 0), -5);
%!assert (mod([-5, 5, 0], [3, 0, 3]), [1, 5, 0]);
%!assert (mod([-5; 5; 0], [3; 0; 3]), [1; 5; 0]);
%!assert (mod([-5, 5; 0, 3], [3, 0 ; 3, 1]), [1, 5 ; 0, 0]);
%!assert (mod([-5, 5; 0, 3], [0, 0 ; 0, 0]), [-5, 5; 0, 3]);

## mixed scalar/matrix tests
%!assert (mod([-5, 5; 0, 3], 0), [-5, 5; 0, 3]); 
%!assert (mod([-5, 5; 0, 3], 3), [1, 2; 0, 0]);
%!assert (mod(-5,[0,0; 0,0]), [-5, -5; -5, -5]);
%!assert (mod(-5,[3,0; 3,1]), [1, -5; 1, 0]);
%!assert (mod(-5,[3,2; 3,1]), [1, 1; 1, 0]);

## integer types
%!assert (mod(uint8(5),uint8(4)),uint8(1))
%!assert (mod(uint8([1:5]),uint8(4)),uint8([1,2,3,0,1]))
%!assert (mod(uint8([1:5]),uint8(0)),uint8([1:5]))
%!error (mod(uint8(5),int8(4)))

## mixed integer/real types
%!assert (mod(uint8(5),4),uint8(1))
%!assert (mod(5,uint8(4)),uint8(1))
%!assert (mod(uint8([1:5]),4),uint8([1,2,3,0,1]))
