## Copyright (C) 1996 Auburn University.  All Rights Reserved
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
## @deftypefn {Function File } { @var{x} =} zgshsr( @var{y})
## apply householder vector based on @math{e^(m)} to 
## (column vector) y.
## Called by zgfslv
## @end deftypefn

function x = zgshsr(y)
  ## A. S. Hodel July 24, 1992
  ## Conversion to Octave by R. Bruce Tenison July 3, 1994

  if(!is_vector(y))
    error(sprintf("y(%dx%d) must be a vector",rows(y),columns(y)));
  endif
  x = vec(y);
  m = length(x);
  if (m>1)
    beta = (1 + sqrt(m))*x(1) + sum(x(2:m));
    beta = beta/(m+sqrt(m));
    x(1) = x(1) - beta*(1.0d0+sqrt(m));
    x(2:m) = x(2:m) - beta*ones(m-1,1);
  else
    x = -x;
  endif
endfunction
