# Copyright (C) 1993, 1998, 1999 Auburn University.  All rights reserved.
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

function [Uret,H,nu] = krylov(A,V,k,eps1,pflg);
  # function [U,H,nu] = krylov(A,V,k{,eps1,pflg});
  # construct orthogonal basis U of block Krylov subspace;
  #  [V AV A^2*V ... A^(k+1)*V];
  # method used: householder reflections to guard against loss of
  # orthogonality
  # eps1: threshhold for 0 (default: 1e-12)
  # pflg: flag to use row pivoting  (improves numerical behavior)
  #   0 [default]: no pivoting; prints a warning message if trivial
  #                null space is corrupted
  #   1          : pivoting performed
  #
  # outputs:
  #   Uret: orthogonal basis of block krylov subspace
  #   H: Hessenberg matrix; if V is a vector then A U = U H
  #      otherwise H is meaningless
  # nu: dimension of span of krylov subspace (based on eps1)
  # if B is a vector and k > m-1, krylov returns H = the Hessenberg
  # decompostion of A.
  #
  # Reference: Hodel and Misra, "Partial Pivoting in the Computation of
  #     Krylov Subspaces", to be submitted to Linear Algebra and its
  #     Applications
  # written by A. Scottedward Hodel a.s.hodel@eng.auburn.edu

  defeps = 1e-12;
  if(nargin < 3 | nargin > 5)
    usage("[U,nu] = krylov(A,V,k{,eps1,pflg})")
  elseif(nargin < 5)
    pflg = 0;        # default permutation flag
  endif
  if(nargin < 4)
    eps1 = defeps;    # default tolerance parameter
  endif
  if(isempty(eps1)) eps1 = defeps; endif

  na = is_square(A);
  if( !na ) error("A(%d x %d) must be square",rows(A),columns(A)); endif

  [m,kb] = size(V);
  if(m != na);
    error("A(%d x %d), V(%d x %d): argument dimensions do not match", ...
      na,na,m,kb)
  endif

  if( !is_scalar(k) )
    error("krylov: third argument must be a scalar integer")
  endif

  Vnrm = norm(V,Inf);

  # check for trivial solution
  if(Vnrm == 0)
    Uret = []; nu = 0;  return;
  endif

  # identify trivial null space
  abm = max(abs([A,V]'));  nzidx = find(abm != 0);  zidx = find(abm == 0);

  # set up vector of pivot points
  pivot_vec = 1:na;

  iter = 0;
  alpha = [];
  nh = 0;
  while (length(alpha) < na) & (columns(V) > 0) & (iter < k)
    iter++;

    # get orthogonal basis of V
    jj = 1;
    while(jj <= columns(V) & length(alpha) < na)
      nu = length(alpha)+1;   # index of next Householder reflection

      short_pv = pivot_vec(nu:na);
      q = V(:,jj);
      short_q = q(short_pv);

      if(norm(short_q) < eps1)
        # insignificant column; delete
        nv = columns(V);
        if(jj != nv)
          [V(:,jj),V(:,nv)] = swap(V(:,jj),V(:,nv));
          # FIX ME: H columns should be swapped too.  Not done since
          # Block Hessenberg structure is lost anyway.
        endif
        V = V(:,1:(nv-1));
        nu = nu - 1;    # one less reflection

      else
        # new householder reflection
        if(pflg)
          # locate max magnitude element in short_q
          asq = abs(short_q);
	  maxv = max(asq);
          maxidx = find(asq == maxv);
          pivot_idx = short_pv(maxidx(1));

          # see if need to change the pivot list
          if(pivot_idx != pivot_vec(nu))
            swapidx = maxidx(1) + (nu-1);
            [pivot_vec(nu),pivot_vec(swapidx)] = ...
              swap(pivot_vec(nu),pivot_vec(swapidx));
          endif
        endif

        # isolate portion of vector for reflection
        idx = pivot_vec(nu:na);
        jdx = pivot_vec(1:nu);

        [hv,av,z] = housh(q(idx),1,0);
        alpha(nu) = av;
        U(idx,nu) = hv;

        # reduce V per the reflection
        V(idx,:) = V(idx,:) - av*hv*(hv' * V(idx,:));
        if(iter > 1)
          # FIX ME: not done correctly for block case
          H(nu,nu-1) = V(pivot_vec(nu),jj);
        endif

        # advance to next column of V
        jj=jj+1;
      endif
    endwhile

    # check for oversize V (due to full rank)
    if( ( columns(V) > na ) & ( length(alpha) == na ) )
      # trim to size
      V = V(:,1:na);
    elseif( columns(V) > na )
      krylov_V = V
      krylov_na = na
      krylov_length_alpha = length(alpha)
      error("This case should never happen; submit bug report.");
    endif

    if(columns(V) > 0)
      # construct next Q and multiply
      Q = zeros(size(V));
      for kk=1:columns(Q)
        Q(pivot_vec(nu-columns(Q)+kk),kk) = 1;
      endfor

      # apply Householder reflections
      for ii = nu:-1:1
        idx = pivot_vec(ii:na);
        hv = U(idx,ii);
        av = alpha(ii);
        Q(idx,:) = Q(idx,:) - av*hv*(hv'*Q(idx,:));
      endfor
    endif

    # multiply to get new vector;
    V = A*Q;
    # project off of previous vectors
    nu = length(alpha);
    for i=1:nu
      hv = U(:,i);  av = alpha(i);
      V = V - av*hv*(hv'*V);
      H(i,nu-columns(V)+(1:columns(V))) = V(pivot_vec(i),:);
    end

  endwhile

  # Back out complete U matrix
  # back out U matrix ;
  j1 = columns(U);
  for i=j1:-1:1;
    idx = pivot_vec(i:na);
    hv = U(idx,i);
    av = alpha(i);
    U(:,i) = zeros(na,1);
    U(idx(1),i) = 1;
    U(idx,i:j1) = U(idx,i:j1)-av*hv*(hv'*U(idx,i:j1));
  endfor

  nu = length(alpha);
  Uret = U;
  if( max(max( abs(Uret(zidx,:)) )) > 0)
    warning("krylov: trivial null space corrupted; set pflg=1 or eps1>%e",eps1);
  endif

endfunction
