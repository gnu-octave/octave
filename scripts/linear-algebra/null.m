# Copyright (C) 1995 John W. Eaton
# 
# This file is part of Octave.
# 
# Octave is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 2, or (at your option) any
# later version.
# 
# Octave is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
# 
# You should have received a copy of the GNU General Public License
# along with Octave; see the file COPYING.  If not, write to the Free
# Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

function retval = null (A, tol)

# usage: null (A, tol)
#        null (A)
#
# Returns an orthonormal basis of the null space of A.
#
# The dimension of the null space is taken as the number of singular
# values of A not greater than tol;  the default for tol is
# max (size (A)) * sigma_max (A) * eps, where sigma_max (A) is the
# maximal singular value of A. 

# Written by KH (Kurt.Hornik@ci.tuwien.ac.at) on Dec 24, 1993.

  [U, S, V] = svd (A);

  [rows, cols] = size (A);

  s = diag (S);

  if (nargin == 1)
    tol = max (size (A)) * s (1) * eps;
  else (nargin != 2)
    usage ("null (A [, tol])"); 
  endif

  rank = sum (s > tol);

  if (rank < cols)
    retval = V (:, rank+1:cols);
  else
    retval = zeros (cols, 0);
  endif

endfunction
