# Copyright (C) 1996, 1997 A. Scottedward Hodel 
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
 
function [K,Q1,P1,Ee,Er] = lqg(sys,Sigw,Sigv,Q,R,input_list)
#
# function [K,Q,P,Ee,Er] = lqg(sys,Sigw,Sigv,Q,R,input_list)
# design a linear-quadratic-gaussian optimal controller for the system
#
#  dx/dt = A x + B u + G w       [w]=N(0,[Sigw 0    ])
#      y = C x + v               [v]  (    0   Sigv ])
#
# or
# 
#  x(k+1) = A x(k) + B u(k) + G w(k)       [w]=N(0,[Sigw 0    ])
#    y(k) = C x(k) + v(k)                  [v]  (    0   Sigv ])
#
# inputs:
#  sys: system data structure
#  Sigw, Sigv: intensities of independent Gaussian noise processes (as above)
#  Q, R: state, control weighting respectively.  Control ARE is
#  input_list: indices of controlled inputs
#     default: last dim(R) inputs are assumed to be controlled inputs, all
#              others are assumed to be noise inputs.
# Outputs:
#    K: system data structure LQG optimal controller
#       (Obtain A,B,C matrices with sys2ss, sys2tf, or sys2zp as appropriate)
#    P: Solution of control (state feedback) algebraic Riccati equation
#    Q: Solution of estimation algebraic Riccati equation
#    Ee: estimator poles
#    Es: controller poles
#
# See also: h2syn, lqe, lqr

# Written by A. S. Hodel August 1995; revised for new system format
# August 1996
# $Revision: 1.1.1.1 $
# $Log: lqg.m,v $
# Revision 1.1.1.1  1998/05/19 20:24:07  jwe
#
# Revision 1.2  1997/03/01 00:21:33  hodel
# fixed some string manipulation problems.
#

sav_val = implicit_str_to_num_ok;
implicit_str_to_num_ok = 1;

if ( (nargin < 5) | (nargin > 6))
  usage("[K,Q1,P1,Ee,Er] = lqg(sys,Sigw, Sigv,Q,R{,input_list})");

elseif(!is_struct(sys) )
  error("sys must be in system data structure");
endif

DIG = is_digital(sys);
[A,B,C,D,tsam,n,nz,stname,inname,outname] = sys2ss(sys);
nout = rows(outname);
nin = rows(inname);
if(nargin == 5)
  #construct default input_list
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
elseif(length(input_list) != columns(R))
  error(["lqg: length(input_list)=",num2str(length(input_list)), ...
	", columns(R)=", num2str(columns(R))]);
endif

varname = ["Sigw";"Sigv";"Q   ";"R   "];
for kk=1:rows(varname);
  stval = dezero(varname(kk,:));
  cmd = ["chk = is_square(",stval,");"];
  eval(cmd);
  if(! chk )
    error(["lqg: ",stval," is not square"]);
  endif
endfor

# permute (if need be)
if(nargin == 6)
  all_inputs = sysreorder(nin,input_list);
  B = B(:,all_inputs);
  inname = inname(all_inputs,:);
endif

# put parameters into correct variables
m1 = columns(Sigw);
m2 = m1+1;
G = B(:,1:m1);
B = B(:,m2:nin);

# now we can just do the design; call dlqr and dlqe, since all matrices
# are not given in Cholesky factor form (as in h2syn case)
if(DIG)
  [Ks P1 Er] = dlqr(A,B,Q,R);
  [Ke Q1 jnk Ee] = dlqe(A,G,C,Sigw,Sigv);
else
  [Ks P1 Er] = lqr(A,B,Q,R);
  [Ke Q1 Ee] = lqe(A,G,C,Sigw,Sigv);
endif
Ac = A - Ke*C - B*Ks;
Bc = Ke;
Cc = -Ks;
Dc = zeros(rows(Cc),columns(Bc));

# fix state names
for ii=1:rows(stname)
  newst = [dezero(stname(ii,:)),"\\e"];
  stname1(ii,1:length(newst)) = newst;
endfor

# fix controller output names
inname = inname(m2:nin,:);
for ii=1:rows(inname)
  newst = [dezero(inname(ii,:)),"\\K"];
  outname1(ii,1:length(newst)) = newst;
endfor

# fix controller input names
for ii=1:rows(outname)
  newst = [dezero(outname(ii,:)),"\\K"];
  inname1(ii,1:length(newst)) = newst;
endfor

if(DIG)
  K = ss2sys(Ac,Bc,Cc,Dc,tsam,n,nz,stname1,inname1,outname1,1:rows(Cc));
else
  K = ss2sys(Ac,Bc,Cc,Dc,tsam,n,nz,stname,inname1,outname1);
endif

implicit_str_to_num_ok = sav_val;
endfunction
