## Copyright (C) 1993, 1994, 1995, 1996, 1997, 1999, 2000, 2004, 2005,
##               2006, 2007, 2008, 2009 John W. Eaton
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
## @deftypefn {Function File} {} rank (@var{a}, @var{tol})
## Compute the rank of @var{a}, using the singular value decomposition.
## The rank is taken to be the number of singular values of @var{a} that
## are greater than the specified tolerance @var{tol}.  If the second
## argument is omitted, it is taken to be
##
## @example
## tol = max (size (@var{a})) * sigma(1) * eps;
## @end example
##
## @noindent
## where @code{eps} is machine precision and @code{sigma(1)} is the largest
## singular value of @var{a}.
## @end deftypefn

## Author: jwe

function retval = rank (A, tol)

  if (nargin == 1)
    sigma = svd (A);
    if (isempty (sigma))
      tolerance = 0;
    else
      if (isa (A, "single"))
        tolerance = max (size (A)) * sigma (1) * eps ("single");
      else
        tolerance = max (size (A)) * sigma (1) * eps;
      endif
    endif
  elseif (nargin == 2)
    sigma = svd (A);
    tolerance = tol;
  else
    print_usage ();
  endif

  retval = sum (sigma > tolerance);

endfunction
