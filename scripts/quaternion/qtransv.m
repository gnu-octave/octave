## Copyright (C) 1998, 2000, 2002, 2005, 2007
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
## @deftypefn {Function File} {} qtransv (@var{v}, @var{q})
## Transform the 3-D vector @var{v} by the unit quaternion @var{q}.
## Return a column vector.
##
## @example
## vi = (2*real(q)^2 - 1)*vb + 2*imag(q)*(imag(q)'*vb) 
##    + 2*real(q)*cross(imag(q),vb)
## @end example
##
## @noindent
## Where imag(q) is a column vector of length 3.
## @end deftypefn

## Author: A. S. Hodel <a.s.hodel@eng.auburn.edu>
## Adapted-By: jwe

function vi = qtransv (vb, qib)

  if (! isvector (vb) || length (vb) != 3)
    error ("qtransv: v(%d,%d) must be a 3-D vector", rows (vb), columns (vb));
  elseif (! isvector (qib) || length (qib) != 4)
    error ("qtransv: q(%d,%d) must be a quaternion", rows (qib), columns (qib));
  elseif (max (abs (imag (vb))) + max (abs (imag (qib))) != 0)
    error ("qtransv: input values must be real");
  endif

  qr = qib(4);
  qimag = vec (qib(1:3));
  vb = vec (vb);
  vi = (2*qr^2 - 1)*vb + 2*qimag*(qimag'*vb) + 2*qr*cross (qimag, vb);

endfunction
