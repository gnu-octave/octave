# Copyright (C) 1996,1998 A. Scottedward Hodel 
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
 
function [U,H,k1] = krylov(A,v,k,eps1);
  # function [U,H,k1] = krylov(A,v,k{,eps1});
  # construct orthogonal basis U of Krylov subspace;
  # 	span([v Av A^2*v ... A^(k+1)*v]);
  # via householder reflections; reflections are multiplied to obtain U
  # eps1: threshhold for 0 relative to norm of current column (default: 1e-12)
  # method used: householder reflections to guard against loss of
  # orthogonality
  #
  # outputs:
  # returned basis U is n x k+1; A*U(:,1:k) = U*H
  # k1: dimension of span of krylov subspace (based on eps1)
  # if k > m-1, krylov returns the Hessenberg decompostion of A.
  
  # Written by A. S. Hodel 1992
  # $Revision: 1.2 $
  # $Log$

  if(nargin == 3)
    eps1 = 1e-12;
  endif

  if( !is_square(A) )
    error("first argument must be a square matrix")
  else
    [m,n] = size(v); 
    if(m != is_square(A))
      error("krylov: argument dimensions do not match")
    elseif( !is_sample(k) )
      error("krylov: third argument must be a scalar integer")
    elseif( k > m)
      warning(["krylov: k is too large; reducing to ",num2str(m)]);
      k = m-1;
    endif
  endif

  if(norm(v) == 0)
    U = [];
    H = [];
    k1 = 0;
    return
  endif

  k1 = k+1;		# Assume subspace basis has full rank
  [m,n] = size(A);
  [hv,alpha(1),z] = housh(v,1,0);

  # initial orthogonal vector
  q = zeros(n,1);
  q(1) = 1;
  q = q - alpha*hv*hv'*q;
  normq = norm(q);
  normres = normq;

  U(:,1) = hv;
  j = 1;
  while(j <= k & normres > eps1*normq)
    # multiply to get new vector;
    q = A*q;
    normq = norm(q);

    # multiply by householder reflections to obtain projected vector and the
    # next column of H
    for i=1:j
     hv = U(i:n,i);
     av = alpha(i);
     q(i:n,1) = q(i:n,1)-av*hv*(hv'*q(i:n,1));
    endfor

    i = j+1;
    # compute and apply next householder vector;
    if(i <= n)
      [hv,av,z] = housh(q(i:n,1),1,0);
      alpha(i) = av;
      q(i:n,1) = q(i:n,1)-av*hv*(hv'*q(i:n,1));
      U(i:n,i) = hv;
      H(1:i,j) = q(1:i);
    else
      av = 0;
      H(:,j) = q;	# complete Hessenberg decomposition
    endif

    # see if done yet
    normres = norm(q(i:n));
    if(normres > eps1*normq)
      j = j+1;
    else   
      k1 = min(k1,j);	# time to quit; norm of residual is small
    endif
  
    # back out new q vector for next pass;
    j1 = columns(U);
    q = zeros(n,1);
    q(j1) = 1;
    for i=j1:-1:1;
      hv = U(i:n,i);
      av = alpha(i);
      q(i:n,1) = q(i:n,1)-av*hv*(hv'*q(i:n,1));
    endfor
  endwhile

  # back out U matrix ;
  j1 = columns(U);
  for i=j1:-1:1;
   hv = U(i:n,i);
   av = alpha(i);
   U(:,i) = zeros(n,1);
   U(i,i) = 1;
   U(i:n,i:j1) = U(i:n,i:j1)-av*hv*(hv'*U(i:n,i:j1));
  endfor

endfunction
