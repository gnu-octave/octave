# Copyright (C) 1993, 1994, 1995 John W. Eaton
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

function dsys = c2d (sys, opt, T)

# Usage: dsys = c2d (sys[, T])
# Usage: dsys = c2d (sys[, opt[, T]])
# inputs:
#   sys: system data structure (may be mixed discrete/continiuous time)
#   optional arguments:
#     opt: conversion option: 
#          "ex" - use the matrix exponential (default)
#          "bi" - use the bilinear transformation
#                   2(z-1)
#               s = -----
#                   T(z+1)
#               FIXME: This option exits with an error if sys is not purely 
#               continuous. (The ex can handle mixed systems.)
#
#     T: sampling time; required if sys is purely continuous.
# outputs: 
#   dsys: discrete time equivalent via zero-order hold, sample each T sec.
#
# converts the system described by:
#   .
#   x = Ac x + Bc u
#
# into a discrete time equivalent model via the matrix exponential
#
#   x[n+1] = Ad x[n] + Bd u[n]
#
# Note: This function adds _d to the names of the new discrete states.   

# Written by R.B. Tenison (btenison@eng.auburn.edu)
# October 1993
# Updated by John Ingram for system data structure August 1996
# SYS_INTERNAL accesses members of system data structure
# $Log: c2d.m,v $
# Revision 1.13  1998-11-06 16:15:36  jwe
# *** empty log message ***
#
# Revision 1.3  1998/07/21 14:53:08  hodelas
# use isempty instead of size tests; use sys calls to reduce direct
# access to system structure elements
#
# Revision 1.2  1998/07/01 16:23:35  hodelas
# Updated c2d, d2c to perform bilinear transforms.
# Updated several files per bug updates from users.
#
# $Revision: 1.13 $

  save_val = implicit_str_to_num_ok;	# save for later
  implicit_str_to_num_ok = 1;

# parse input arguments
  if(nargin < 1 | nargin > 3)
    usage("dsys=c2d(sys[,T])");
  elseif (!is_struct(sys))
    error("sys must be a system data structure");
  elseif (nargin == 1)
    opt = "ex";
  elseif (nargin == 2 & !isstr(opt) )
    T = opt;
    opt = "ex";
  endif

  # check if sampling period T was passed.
  Ts = sysgettsam(sys);
  if(!exist("T"))
    T = Ts;
    if(T == 0)
      error("sys is purely continuous; no sampling period T provided");
    endif
  elseif (T != Ts & Ts > 0)
    warning(["c2d: T=",num2str(T),", system tsam==",num2str(Ts), ...
      ": using T=", num2str(min(T,Ts))]);
    T = min(T,Ts);
  endif

  if (!is_sample(T))
    error("sampling period T must be a postive, real scalar");
  elseif( ! (strcmp(opt,"ex") | strcmp(opt,"bi") ) )
    error(["illegal option passed: ",opt])
  endif

  sys = sysupdate(sys,"ss");
  [n,nz,m,p] = sysdimensions(sys);

  if (n == 0)
    warning("c2d: sys has no continuous states; setting outputs to discrete");
    dsys = syschnames(sys,"yd",1:p,ones(1:p));
  elseif(strcmp(opt,"ex"))
    # construct new state-space (a,b,c,d) for continuous subsystem
    [csys,Acd] = syscont(sys);   	# extract continuous subsystem
    [csys_a, csys_b, csys_c, csys_d] = sys2ss(csys);
    [ sys_a,  sys_b,  sys_c,  sys_d] = sys2ss( sys);
    if(isempty(Acd))
      Bmat = sys_b;
    elseif(isempty(csys_b))
      Bmat = Acd;
    else
      Bmat = [Acd csys_b];
    endif
    
    row_zer = columns(Bmat);
    col_zer = csys.n + row_zer;

    if(isempty(Bmat) )
      warning("c2d: no inputs to continuous subsystem.");
      mat = csys.a;
    else
      mat = [csys.a Bmat ; zeros( row_zer,col_zer) ];
    endif

    matexp = expm(mat * T);
  
    Abar = matexp( 1:csys.n , 1:(csys.n + columns(Acd)) );  
    Bbar = matexp( 1:csys.n , (columns(Abar) + 1):columns(matexp) );

    dsys = sys;

    dsys.a(1:csys.n , : ) = Abar;
    dsys.b(1:csys.n , : ) = Bbar;

    dsys.sys = [2 0 0 1];

    dsys.tsam = T;
    dsys.n = 0;
    dsys.nz = rows(dsys.a);

    dsys.yd = ones(1,rows(dsys.c));

    for ii = 1:csys.n
      strval = [dezero((dsys.stname(ii,:))),"_d"];
      dsys.stname(ii,(1:length(strval))) = [strval];
    endfor

  elseif(strcmp(opt,"bi"))
    if(is_digital(sys))
      error("c2d: system is already digital")
    else
      # convert with bilinear transform
      [a,b,c,d,tsam,n,nz,stname,inname,outname,yd] = sys2ss(sys);
      IT = (2/T)*eye(size(a));
      A = (IT+a)/(IT-a);
      iab = (IT-a)\b;
      tk=2/sqrt(T);
      B = tk*iab;
      C = tk*(c/(IT-a));
      D = d + (c*iab);
      stnamed="";
      for kk=1:rows(stname)
        tmp =  [dezero(stname(kk,:)),"_d"];
        stnamed(kk,1:length(tmp)) = tmp;
      endfor
      dsys = ss2sys(A,B,C,D,T,0,rows(A),stnamed,inname,outname);
    endif
  else
    error(["Bad option=",opt])
  endif
  
  implicit_str_to_num_ok = save_val;	# restore value

endfunction
