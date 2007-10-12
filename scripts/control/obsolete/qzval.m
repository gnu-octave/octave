## Copyright (C) 1998, 2000, 2004, 2005, 2007
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
## @deftypefn {Function File} {} qzval (@var{a}, @var{b})
## Compute generalized eigenvalues of the matrix pencil
## @ifinfo
## @example
## (A - lambda B).
## @end example
## @end ifinfo
## @iftex
## @tex
## $(A - \lambda B)$.
## @end tex
## @end iftex
##
## @var{a} and @var{b} must be real matrices.
##
## @code{qzval} is obsolete; use @code{qz} instead.
## @end deftypefn

## Author: A. S. Hodel <a.s.hodel@eng.auburn.edu>
## Created: July 1998

function lam = qzval (A, B)

  warning("qzval is obsolete; calling qz instead")
  lam = qz(A,B);

endfunction
