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

## Usage: x = kron (a, b)
##
## Form the Kronecker product of two matrices, defined block by block
## as
##
##   x = [a(i,j) b]

## Author: A. S. Hodel <scotte@eng.auburn.edu>
## Created: August 1993
## Adapted-By: jwe

function x = kron (a, b)

  if (nargin == 2)

    [m, n] = size (b);
    [ma, na] = size (a);

    x = zeros (ma*m, na*n);	
    i_vec = 1:m;
    j_vec = 1:n;

    for jj = 1:na
      for ii = 1:ma
	x(i_vec+(ii-1)*m,j_vec) = a(ii,jj) * b;
      endfor
      j_vec = jvec + n;
    endfor
    
  else
    usage ("kron (a, b)");
  endif

endfunction
