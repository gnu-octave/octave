# Copyright (C) 1998 Auburn University.  All Rights Reserved.
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
# Software Foundation, 59 Temple Place, Suite 330, Boston, MA 02111 USA. 
 
function lam = qzval(A,B)
# X = qzval (A, B)
# 
# compute generalized eigenvalues of the matrix pencil (A - lambda B).
# A and B must be real matrices.
# 
# This function is superseded by qz.  You should use qz instead.
#

# A. S. Hodel July 1998

  warning("qzval is obsolete; calling qz instead")
  lam = qz(A,B);
endfunction

