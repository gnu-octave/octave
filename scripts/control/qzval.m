## Copyright (C) 1998 Auburn University.  All rights reserved.
##
## This file is part of Octave. 
##
## Octave is free software; you can redistribute it and/or modify it 
## under the terms of the GNU General Public License as published by the 
## Free Software Foundation; either version 2, or (at your option) any 
## later version. 
## 
## Octave is distributed in the hope that it will be useful, but WITHOUT 
## ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or 
## FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License 
## for more details.
## 
## You should have received a copy of the GNU General Public License 
## along with Octave; see the file COPYING.  If not, write to the Free 
## Software Foundation, 59 Temple Place, Suite 330, Boston, MA 02111 USA. 

## -*- texinfo -*-
## @deftypefn {Function File } { @var{x} =} qzval (@var{A}, @var{B})
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
## @var{A} and @var{B} must be real matrices.
##  
## @strong{Note} @code{qzval} is obsolete; use @code{qz} instead.
## @end deftypefn
 
## A. S. Hodel July 1998

function lam = qzval (A, B)

  warning("qzval is obsolete; calling qz instead")
  lam = qz(A,B);
endfunction

