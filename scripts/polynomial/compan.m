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
## Software Foundation, 59 Temple Place - Suite 330, Boston, MA
## 02111-1307, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} compan (@var{c})
## Compute the companion matrix corresponding to polynomial coefficient
## vector @var{c}.
##
## The companion matrix is
## @iftex
## @tex
## $$
## A = \left[\matrix{
##  -c_2/c_1 & -c_3/c_1 & \cdots & -c_N/c_1 & -c_{N+1}/c_1\cr
##      1    &     0    & \cdots &     0    &         0   \cr
##      0    &     1    & \cdots &     0    &         0   \cr
##   \vdots  &   \vdots & \ddots &  \vdots  &      \vdots \cr
##      0    &     0    & \cdots &     1    &         0}\right].
## $$
## @end tex
## @end iftex
## @ifinfo
##
## @smallexample
##      _                                                        _
##     |  -c(2)/c(1)   -c(3)/c(1)  ...  -c(N)/c(1)  -c(N+1)/c(1)  |
##     |       1            0      ...       0             0      |
##     |       0            1      ...       0             0      |
## A = |       .            .   .            .             .      |
##     |       .            .       .        .             .      |
##     |       .            .           .    .             .      |
##     |_      0            0      ...       1             0     _|
## @end smallexample
## @end ifinfo
##
## The eigenvalues of the companion matrix are equal to the roots of the
## polynomial.
## @end deftypefn
##
## @seealso{poly, roots, residue, conv, deconv, polyval, polyderiv, and
## polyinteg}

## Author: Tony Richardson <arichard@stark.cc.oh.us>
## Created: June 1994
## Adapted-By: jwe

function A = compan (c)

  if (nargin != 1)
    usage ("compan (vector)");
  endif

  if (! isvector (c))
    error ("compan: expecting a vector argument");
  endif

  ## Ensure that c is a row vector.

  if (rows (c) > 1)
    c = c.';
  endif

  n = length (c);

  if (n == 1)
    A = [];
  else
    A = diag (ones (n-2, 1), -1);
    A(1,:) = -c(2:n) / c(1);
  endif

endfunction
