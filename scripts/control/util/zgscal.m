## Copyright (C) 1996, 1998 Auburn University.  All rights reserved.
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
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301 USA.

## -*- texinfo -*-
## @deftypefn {Function File} {@var{x} =} zgscal (@var{f}, @var{z}, @var{n}, @var{m}, @var{p})
## Generalized conjugate gradient iteration to
## solve zero-computation generalized eigenvalue problem balancing equation
## @math{fx=z}; called by @command{zgepbal}.
## @end deftypefn

## References:
## ZGEP: Hodel, "Computation of Zeros with Balancing," 1992, submitted to  LAA
## Generalized CG: Golub and Van Loan, "Matrix Computations, 2nd ed" 1989

## Author: A. S. Hodel <a.s.hodel@eng.auburn.edu>
## Created: July 24, 1992
## Conversion to Octave R. Bruce Tenison July 3, 1994

function x = zgscal (a, b, c, d, z, n, m, p)

  ## initialize parameters:
  ## Givens rotations, diagonalized 2x2 block of F, gcg vector initialization

  nmp = n+m+p;

  ## x_0 = x_{-1} = 0, r_0 = z
  x = zeros(nmp,1);
  xk1 = x;
  xk2 = x;
  rk1 = z;
  k = 0;

  ## construct balancing least squares problem
  F = eye(nmp);
  for kk=1:nmp
    F(1:nmp,kk) = zgfmul(a,b,c,d,F(:,kk));
  endfor

  [U,H,k1] = krylov(F,z,nmp,1e-12,1);
  if(!issquare(H))
    if(columns(H) != k1)
      error("zgscal(tzero): k1=%d, columns(H)=%d",k1,columns(H));
    elseif(rows(H) != k1+1)
      error("zgscal: k1=%d, rows(H) = %d",k1,rows(H));
    elseif ( norm(H(k1+1,:)) > 1e-12*norm(H,"inf") )
      zgscal_last_row_of_H = H(k1+1,:)
      error("zgscal: last row of H nonzero (norm(H)=%e)",norm(H,"inf"))
    endif
    H = H(1:k1,1:k1);
    U = U(:,1:k1);
  endif

  ## tridiagonal H can still be rank deficient, so do permuted qr
  ## factorization
  [qq,rr,pp] = qr(H);   # H = qq*rr*pp'
  nn = rank(rr);
  qq = qq(:,1:nn);
  rr = rr(1:nn,:);            # rr may not be square, but "\" does least
  xx = U*pp*(rr\qq'*(U'*z));  # squares solution, so this works
  ## xx1 = pinv(F)*z;
  ## zgscal_x_xx1_err = [xx,xx1,xx-xx1]
  return;

  ## the rest of this is left from the original zgscal;
  ## I've had some numerical problems with the GCG algorithm,
  ## so for now I'm solving it with the krylov routine.

  ## initialize residual error norm
  rnorm = norm(rk1,1);

  xnorm = 0;
  fnorm = 1e-12 * norm([a,b;c,d],1);

  ## dummy defines for MATHTOOLS compiler
  gamk2 = 0;      omega1 = 0;      ztmz2 = 0;

  ## do until small changes to x
  len_x = length(x);
  while ((k < 2*len_x) & (xnorm> 0.5) & (rnorm>fnorm))|(k == 0)
    k = k+1;

    ## solve F_d z_{k-1} = r_{k-1}
    zk1= zgfslv(n,m,p,rk1);

    ## Generalized CG iteration
    ## gamk1 = (zk1'*F_d*zk1)/(zk1'*F*zk1);
    ztMz1 = zk1'*rk1;
    gamk1 = ztMz1/(zk1'*zgfmul(a,b,c,d,zk1));

    if(rem(k,len_x) == 1) omega = 1;
    else                  omega = 1/(1-gamk1*ztMz1/(gamk2*omega1*ztmz2));
    endif

    ## store x in xk2 to save space
    xk2 = xk2 + omega*(gamk1*zk1 + xk1 - xk2);

    ## compute new residual error: rk = z - F xk, check end conditions
    rk1 = z - zgfmul(a,b,c,d,xk2);
    rnorm = norm(rk1);
    xnorm = max(abs(xk1 - xk2));

    ## printf("zgscal: k=%d, gamk1=%e, gamk2=%e, \nztMz1=%e ztmz2=%e\n", ...
    ##   k,gamk1, gamk2, ztMz1, ztmz2);
    ## xk2_1_zk1 = [xk2 xk1 zk1]
    ## ABCD = [a,b;c,d]
    ## prompt

    ## get ready for next iteration
    gamk2 = gamk1;
    omega1 = omega;
    ztmz2 = ztMz1;
    [xk1,xk2] = swap(xk1,xk2);
  endwhile
  x = xk2;

  ## check convergence
  if (xnorm> 0.5 & rnorm>fnorm)
    warning("zgscal(tzero): GCG iteration failed; solving with pinv");

    ## perform brute force least squares; construct F
    Am = eye(nmp);
    for ii=1:nmp
      Am(:,ii) = zgfmul(a,b,c,d,Am(:,ii));
    endfor

    ## now solve with qr factorization
    x = pinv(Am)*z;
  endif
endfunction
