## Copyright (C) 1996, 2000, 2002, 2004, 2005, 2007
##               Auburn University. All rights reserved.
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

##  O B S O L E T E * * * D O   N O T   U S E!
##
##  Use lqg instead.
##
## function [K,Q,P,Ee,Er] = dlqg(A,B,C,G,Sigw,Sigv,Q,R)
## function [K,Q,P,Ee,Er] = dlqg(Sys,Sigw,Sigv,Q,R)
##
## design a discrete-time linear quadratic gaussian optimal controller
## for the system
##
##  x(k+1) = A x(k) + B u(k) + G w(k)       [w]=N(0,[Sigw 0    ])
##    y(k) = C x(k) + v(k)                  [v]  (    0   Sigv ])
##
## Outputs:
##    K: system data structure format LQG optimal controller
##    P: Solution of control (state feedback) algebraic Riccati equation
##    Q: Solution of estimation algebraic Riccati equation
##    Ee: estimator poles
##    Es: controller poles
## inputs:
##  A,B,C,G, or Sys: state space representation of system.
##  Sigw, Sigv: covariance matrices of independent Gaussian noise processes
##      (as above)
##  Q, R: state, control weighting matrices for dlqr call respectively.
##
## See also: lqg, dlqe, dlqr

## Author: A. S. Hodel <a.s.hodel@eng.auburn.edu>
## Created: August 1995

function [K, Q, P, Ee, Er] = dlqg (A, B, C, G, Sigw, Sigv, Q, R)

  warning("dlqg: obsolete. use lqg instead (system data structure format)");

  if (nargin == 5)
    ## system data structure format

    ## check that it really is system data structure
    if(! isstruct(A) )
      error("dlqg: 5 arguments, first argument is not a system data structure structure")
    endif

    sys = sysupdate(sys,"ss");    # make sure in proper form
    [ncstates,ndstates,nin,nout] = sysdimensions(sys);
    if(ndstates == -1)
      error("this message should never appear: bad system dimensions");
    endif

    if(ncstates)
      error("dlqg: system has continuous-time states (try lqg?)")
    elseif(ndstates < 1)
      error("dlqg: system has no discrete time states")
    elseif(nin <= columns(Sigw))
      error(["dlqg: ",num2str(nin)," inputs provided, noise dimension is ", ...
          num2str(columns(Sigw))])
    elseif(nout != columns(Sigv))
      error(["dlqg: number of outputs (",num2str(nout),") incompatible with ", ...
          "dimension of Sigv (",num2str(columns(Sigv)),")"])
    endif

    ## put parameters into correct variables
    R = Sigw;
    Q = G;
    Sigv = C;
    Sigw = B;
    [A,B,C,D] = sys2ss(Sys)
    [n,m] = size(B)
    m1 = columns(Sigw);
    m2 = m1+1;
    G = B(:,1:m1);
    B = B(:,m2:m);

  elseif (nargin == 8)
    ## state-space format
    m = columns(B);
    m1 = columns(G);
    p = rows(C);
    n = abcddim(A,B,C,zeros(p,m));
    n1 = abcddim(A,G,C,zeros(p,m1));
    if( (n == -1) || (n1 == -1))
      error("dlqg: A,B,C,G incompatibly dimensioned");
    elseif(p != columns(Sigv))
      error("dlqg: C, Sigv incompatibly dimensioned");
    elseif(m1 != columns(Sigw))
      error("dlqg: G, Sigw incompatibly dimensioned");
    endif
  else
    error ("dlqg: invalid number of arguments")
  endif

  if (! (issquare(Sigw) && issquare(Sigv) ) )
    error("dlqg: Sigw, Sigv must be square");
  endif

  ## now we can just do the design; call dlqr and dlqe, since all matrices
  ## are not given in Cholesky factor form (as in h2syn case)
  [Ks, P, Er] = dlqr(A,B,Q,R);
  [Ke, Q, jnk, Ee] = dlqe(A,G,C,Sigw,Sigv);
  Ac = A - Ke*C - B*Ks;
  Bc = Ke;
  Cc = -Ks;
  Dc = zeros(rows(Cc),columns(Bc));
  K = ss(Ac,Bc,Cc,Dc,1);
  disp("HODEL: need to add names to this guy!")

endfunction
