# Copyright (C) 1993, 1998, 1999 A. Scottedward Hodel
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

function [Uret,Ucols] = krylovb(A,V,k,eps1,pflg);
  # function [U,Ucols] = krylovb(A,V,k{,eps1,pflg});
  # construct orthogonal basis U of block Krylov subspace;
  # 	[V AV A^2*V ... A^(k+1)*V];
  # method used: householder reflections to guard against loss of
  # orthogonality
  # eps1: threshhold for 0 (default: 1e-12)
  # pflg: permutation flag
  # outputs:
  #   returned basis U is orthogonal matrix; due to "zeroed"
  #   columns of product, may not satisfy A U = U H identity
  # Ucols: dimension of span of krylov subspace (based on eps1)
  # if k > m-1, krylov returns the Hessenberg decompostion of A.
  #
  # Note: krylovb directly calls and is superseded by krylov.

  switch(nargin)
  case(3),
    [Uret,H,Ucols] = krylov(A,V,k);
  case(4),
    [Uret,H,Ucols] = krylov(A,V,k,eps1);
  case(5),
    [Uret,H,Ucols] = krylov(A,V,k,eps1,pflg);
  otherwise,
    usage("[Uret,Ucols] = krylovb(A,V,k{,eps1,pflg}); %d arguments passed", ...
      nargin);
  endswitch

endfunction
