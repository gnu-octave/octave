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
 
function [U,H,k1] = krylov(A,v,k,eps1,pflg);
  # function [U,H,k1] = krylov(A,v,k{,eps1});
  # construct orthogonal basis U of Krylov subspace;
  #     span([v Av A^2*v ... A^(k+1)*v]);
  # via householder reflections; reflections are multiplied to obtain U
  # inputs:
  #   A: square matrix
  #   v: column vector
  #   k: desired krylov subspace dimension (as above)
  #   eps1: threshhold for 0 relative to norm of current column (default: 1e-12)
  #   pflg: permutation flag (default 0): avoid using zero rows of
  #      [A,v] as householder pivots; this avoids spurious non-zero entries
  #      in returned orthogonal matrix U, but destroys the Householder matrix
  #      structure of H.
  # outputs:
  #   U: (n x k+1) orthogonal basis of Krylov subspace. A*U(:,1:k) = U*H
  #   H: (pflg=0): Hessenberg matrix satisfying A*U(:,1:k) = U*H
  #      (pflg=1): Workspace; does not satisfy above equation.
  # k1: dimension of span of krylov subspace (based on eps1)
  # if k > m-1, krylov returns the Hessenberg decompostion of A.

  # Written by A. S. Hodel 1992
  # $Revision: 1.2 $
  # $Log$

  save_empty_list_elements_ok = empty_list_elements_ok;
  empty_list_elements_ok = 1;

  if    (nargin > 5)   usage("[U,H,k1] = krylov(A,v,k{,eps1,pflg})");
  elseif(nargin < 5)   pflg = 0;
  elseif(nargin < 4)   eps1 = 1e-12; endif
  na = is_square(A);
  if(!na) error("krylov: A(%dx%d) must be square",na,na); endif
  [m,n] = size(v);
  if(!is_vector(v)) error("krylov: v(%dx%d) must be a vector",m,n);
  elseif(length(v) != na)
    error("krylov: A(%dx%d), v(%dx1); mismatch",na,na,length(v));
  endif
  v = vec(v);    # make sure it's a column vector
  if(!is_scalar(k))
    error("krylov: k(%dx%d) must be a scalar",rows(k), columns(k));
  elseif( k > m)
    warning("krylov: k is too large; reducing to %d",m-1);
    k = m-1;
  endif

  # check for zero input vector
  if(norm(v) == 0) U = []; H = []; k1 = 0; return endif

  # indices of legal pivot points (identifies trivial null space).
  abm = max(abs([A,v]')); nzidx = find(abm != 0); zidx = find(abm == 0);

  # check permutation flag
  if(pflg)
    # permute zero rows of [A,v] to trailing rows
    permvec = [vec(nzidx); vec(zidx)];
    pmat = eye(na); pmat = pmat(permvec,:);
    ipermvec = pmat'*vec(1:na);
    A = A(permvec,permvec);
    v = v(permvec);
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

  # check permutation flag
  if(pflg)
    # permute rows of U back to original coordinates
    U = U(ipermvec,:);
  endif

  # check for spurious nonzero entries
  if( max(max( abs(U(zidx,:)) )) > eps1 )
    warning("krylov: trivial null space corrupted; set pflg=1 or eps1>%e",eps1);
  endif

  empty_list_elements_ok = save_empty_list_elements_ok;

endfunction
