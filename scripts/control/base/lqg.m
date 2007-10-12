## Copyright (C) 1996, 1997, 2000, 2002, 2004, 2005, 2006, 2007
##               Auburn University.  All rights reserved.
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

## -*- texinfo -*-
## @deftypefn {Function File} {[@var{k}, @var{q1}, @var{p1}, @var{ee}, @var{er}] =} lqg (@var{sys}, @var{sigw}, @var{sigv}, @var{q}, @var{r}, @var{in_idx})
## Design a linear-quadratic-gaussian optimal controller for the system
## @example
## dx/dt = A x + B u + G w       [w]=N(0,[Sigw 0    ])
##     y = C x + v               [v]  (    0   Sigv ])
## @end example
## or
## @example
## x(k+1) = A x(k) + B u(k) + G w(k)       [w]=N(0,[Sigw 0    ])
##   y(k) = C x(k) + v(k)                  [v]  (    0   Sigv ])
## @end example
##
## @strong{Inputs}
## @table @var
## @item  sys
## system data structure
## @item  sigw
## @itemx  sigv
## intensities of independent Gaussian noise processes (as above)
## @item  q
## @itemx  r
## state, control weighting respectively.  Control @acronym{ARE} is
## @item  in_idx
## names or indices of controlled inputs (see @command{sysidx}, @command{cellidx})
##
## default: last dim(R) inputs are assumed to be controlled inputs, all
## others are assumed to be noise inputs.
## @end table
## @strong{Outputs}
## @table @var
## @item    k
## system data structure format @acronym{LQG} optimal controller (Obtain A, B, C
## matrices with @command{sys2ss}, @command{sys2tf}, or @command{sys2zp} as
## appropriate).
## @item    p1
## Solution of control (state feedback) algebraic Riccati equation.
## @item    q1
## Solution of estimation algebraic Riccati equation.
## @item    ee
## Estimator poles.
## @item    es
## Controller poles.
## @end table
## @seealso{h2syn, lqe, lqr}
## @end deftypefn

## Author: A. S. Hodel <a.s.hodel@eng.auburn.edu>
## Created: August 1995
## revised for new system format August 1996

function [K, Q1, P1, Ee, Er] = lqg (sys, Sigw, Sigv, Q, R, input_list)

  if ( (nargin < 5) | (nargin > 6))
    print_usage ();

  elseif(!isstruct(sys) )
    error("sys must be in system data structure");
  endif

  DIG = is_digital(sys);
  [A,B,C,D,tsam,n,nz,stname,inname,outname] = sys2ss(sys);
  [n,nz,nin,nout] = sysdimensions(sys);
  if(nargin == 5)
    ## construct default input_list
    input_list = (columns(Sigw)+1):nin;
  endif

  if( !(n+nz) )
      error(["lqg: 0 states in system"]);

  elseif(nin != columns(Sigw)+ columns(R))
    error(["lqg: sys has ",num2str(nin)," inputs, dim(Sigw)=", ...
          num2str(columns(Sigw)),", dim(u)=",num2str(columns(R))])

  elseif(nout != columns(Sigv))
    error(["lqg: sys has ",num2str(nout)," outputs, dim(Sigv)=", ...
          num2str(columns(Sigv)),")"])
  endif

  ## check for names of signals
  if(is_signal_list(input_list) | ischar(input_list))
    input_list = sysidx(sys,"in",input_list);
  endif

  if(length(input_list) != columns(R))
    error(["lqg: length(input_list)=",num2str(length(input_list)), ...
          ", columns(R)=", num2str(columns(R))]);
  endif

  varname = {"Sigw","Sigv","Q","R"};
  for kk=1:length(varname);
    eval(sprintf("chk = issquare(%s);",varname{kk}));
    if(! chk ) error("lqg: %s is not square",varname{kk}); endif
  endfor

  ## permute (if need be)
  if(nargin == 6)
    all_inputs = sysreorder(nin,input_list);
    B = B(:,all_inputs);
    inname = inname(all_inputs);
  endif

  ## put parameters into correct variables
  m1 = columns(Sigw);
  m2 = m1+1;
  G = B(:,1:m1);
  B = B(:,m2:nin);

  ## now we can just do the design; call dlqr and dlqe, since all matrices
  ## are not given in Cholesky factor form (as in h2syn case)
  if(DIG)
    [Ks, P1, Er] = dlqr(A,B,Q,R);
    [Ke, Q1, jnk, Ee] = dlqe(A,G,C,Sigw,Sigv);
  else
    [Ks, P1, Er] = lqr(A,B,Q,R);
    [Ke, Q1, Ee] = lqe(A,G,C,Sigw,Sigv);
  endif
  Ac = A - Ke*C - B*Ks;
  Bc = Ke;
  Cc = -Ks;
  Dc = zeros(rows(Cc),columns(Bc));

  ## fix state names
  stname1 = strappend(stname,"_e");

  ## fix controller output names
  outname1 = strappend(inname(m2:nin),"_K");

  ## fix controller input names
  inname1 = strappend(outname,"_K");

  if(DIG)
    K = ss(Ac,Bc,Cc,Dc,tsam,n,nz,stname1,inname1,outname1,1:rows(Cc));
  else
    K = ss(Ac,Bc,Cc,Dc,tsam,n,nz,stname,inname1,outname1);
  endif

endfunction
