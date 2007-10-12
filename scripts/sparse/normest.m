## Copyright (C) 2006, 2007 David Bateman and Marco Caliari
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
## @deftypefn {Function File} {[@var{n}, @var{c}] =} normest (@var{a}, @var{tol})
## Estimate the 2-norm of the matrix @var{a} using a power series
## analysis. This is typically used for large matrices, where the cost
## of calculating the @code{norm (@var{a})} is prohibitive and an approximation
## to the 2-norm is acceptable.
##
## @var{tol} is the tolerance to which the 2-norm is calculated. By default
## @var{tol} is 1e-6. @var{c} returns the number of iterations needed for
## @code{normest} to converge.
## @end deftypefn

function [e1, c] = normest (A, tol)
  if (nargin < 2)
    tol = 1e-6;
  endif
  if (tol < eps)
    tol = eps
  endif
  if (ndims(A) != 2)
    error ("normest: A must be a matrix");
  endif 
  maxA = max (max (abs (A)));
  c = 0;
  if (maxA == 0)
    e1 = 0
  else
    [m, n] = size (A);
    B = A / maxA;
    Bt = B';
    if (m > n)
      tmp = B;
      B = Bt;
      Bt = tmp;
    endif
    e0 = 0;
    x = randn (min (m, n), 1);
    e1 = norm (x);
    x = x / e1;
    e1 = sqrt (e1);
    if (issparse (A))
      while (abs (e1 - e0) > tol * e1)
	e0 = e1;
	x = B * (Bt * x);
	e1 = norm (x);
	x = x / e1;
	e1 = sqrt (e1);
	c = c + 1;
      endwhile
    else
      B = B * Bt;
      while (abs (e1 - e0) > tol * e1)
	e0 = e1;
	x = B * x;
	e1 = norm (x);
	x = x / e1;
	e1 = sqrt (e1);
	c = c + 1;
      endwhile
    endif
    e1 = e1 * maxA;
  endif
endfunction
