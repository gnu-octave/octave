## Copyright (C) 2006, 2007, 2008, 2009 David Bateman and Marco Caliari
## Copyright (C) 2009 VZLU Prague
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
## analysis.  This is typically used for large matrices, where the cost
## of calculating the @code{norm (@var{a})} is prohibitive and an approximation
## to the 2-norm is acceptable.
##
## @var{tol} is the tolerance to which the 2-norm is calculated.  By default
## @var{tol} is 1e-6.  @var{c} returns the number of iterations needed for
## @code{normest} to converge.
## @end deftypefn

function [e, c] = normest (A, tol = 1e-6)
  if (! ismatrix (A) || ndims (A) != 2)
    error ("normest: A must be a matrix");
  endif 
  if (! isfloat (A))
    A = double (A);
  endif
  tol = max (tol, eps (class (A)));
  c = 0;
  x = norm (A, "columns").';
  e = norm (x);
  if (e > 0)
    [m, n] = size (A);
    x /= e;
    e0 = 0;
    while (abs (e - e0) > tol * e)
      e0 = e;
      y = A*x;
      e = norm (y);
      if (e == 0)
        x = rand (n, 1);
      else
        x = A'*(y / e);
      endif
      x /= norm (x);
      c += 1;
    endwhile
  endif
endfunction
