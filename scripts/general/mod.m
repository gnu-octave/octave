## Copyright (C) 1999, 2000 Paul Kienzle
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
## @deftypefn {Mapping Function} {} mod (@var{x}, @var{y})
## Compute modulo function, using
##
## @example
## x - y .* floor (x ./ y)
## @end example
##
## Note that this handles negative numbers correctly:
## @code{mod (-1, 3)} is 2, not -1 as @code{rem (-1, 3)} returns.
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

  if (((ndims (x) != ndims (y)) || any (size (x) != size (y))) &&
	 ! (isscalar (x) || isscalar (y)))
    error ("mod: argument sizes must agree");
  endif

  ## Matlab allows complex arguments, but as far as I can tell, that's a
  ## bunch of hooey.

  if (isreal (x) && isreal (y))
    nz = y != 0.0;
    if (all (nz(:)))
      ## No elements of y are zero.
      r = x - y .* floor (x ./ y);
    elseif (isscalar (y))
      ## y must be zero.
      r = x;
    else
      ## Some elements of y are zero.
      if (isscalar (x))
	r = x * ones (size (y));
      else
	r = x;
	x = x(nz);
      endif
      y = y(nz);
      r(nz) = x - y .* floor (x ./ y);
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

