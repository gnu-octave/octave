# Copyright (C) 1993, 1998 A. Scottedward Hodel
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

function [Uret,Ucols] = krylovb(A,V,k,eps1);
  # function [U,Ucols] = krylovb(A,V,k[,eps1]);
  # construct orthogonal basis U of block Krylov subspace;
  # 	[V AV A^2*V ... A^(k+1)*V];
  # method used: householder reflections to guard against loss of
  # orthogonality
  # eps1: threshhold for 0 (default: 1e-12)
  #
  # outputs:
  #   returned basis U is orthogonal matrix; due to "zeroed"
  #   columns of product, may not satisfy A U = U H identity
  # Ucols: dimension of span of krylov subspace (based on eps1)
  # if k > m-1, krylov returns the Hessenberg decompostion of A.
#
# A. S. Hodel Feb 1993
# $Revision$
# $Log$

  if(nargin == 3)
    eps1 = 1e-12;
  endif
  na = is_square(A);
  if( !na )
    error("krylov: first argument must be a square matrix")
  endif
 
  [m,kb] = size(V); 
  if(m != na);
    error("krylov: argument dimensions do not match")
  endif

  if( !is_scalar(k) )
    error("krylov: third argument must be a scalar integer")
  endif

  Vnrm = norm(V,Inf);

  # compute factored QR
  [U,alpha,kb] = qrhouse(V,eps1*Vnrm);
  Q = krygetq(U,alpha,kb);
  Uret = Q;
  Ucols = kb;

  iter = 0;
  while (Ucols < na) & (kb > 0) & (iter < k)
    iter++;
    # multiply to get new vector;
    V = A*Q;

    # project off of previous vectors
    nzv = [];    # set of reflection indices used so far
    nj = length(alpha);
    for i=1:nj
     hv = U(:,i);
     nzidx = find(hv != 0);	# extract nonzero entries
     nzv = union(nzv,nzidx(1)); # save for call to qrhouse
     hv = hv(nzidx);
     vr = V(nzidx,:); 
     av = alpha(i);
     V(nzidx,:) = vr - (av*hv)*(hv'*vr);
    end
    V(nzv,:) = 0;		# clear entries covered by earlier reflections

    [hvk,alphk,kb] = qrhouse(V,eps1*Vnrm);	# now get new QR factorization
    if(kb > 0)
      U = [U,hvk];		# append new data to Householder data structure
      alpha = [alpha;alphk];
      Q2 = krygetq(U,alpha,kb);
      Uret = [Uret,Q2];

      Ucols = Ucols + kb;
      Q = Q2;
    end
  end
endfunction
