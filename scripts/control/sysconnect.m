# Copyright (C) 1996 A. Scottedward Hodel 
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
 
function sys = sysconnect(sys,output_list,input_list,order,tol)
# function retsys = sysconnect(sys,output_list,input_list[,order,tol])
# Close the loop from specified outputs to respective specified inputs
# 
# inputs:
#   sys: system data structure
#   output_list,input_list: list of connections indices; y(output_list(ii))
#       is connected to u(input_list(ii)).
#   order: logical flag (default = 0)
#	0: leave inputs and outputs in their original order
#	1: permute inputs and outputs to the order shown in the diagram below
#     tol: tolerance for singularities in algebraic loops
#	    default: 200*eps
# output: sys: resulting closed loop system:
#
# Operation: sysconnect internally permutes selected inputs, outputs as shown
# below, closes the loop, and then permutes inputs and outputs back to their
# original order
#                      ____________________
#                      |                  |
#    u_1         ----->|                  |----> y_1
#                      |        sys       |
#              old u_2 |                  |
#   u_2* ------>(+)--->|                  |----->y_2 
#   (input_list) ^     |                  |    | (output_list)
#                |     --------------------    |
#                |                             |
#                -------------------------------
#
# The input that has the summing junction added to it has an * added to the end 
# of the input name.

# A. S. Hodel August 1995
# modified by John Ingram July 1996
# $Revision: 1.1.1.1 $
# $Log: sysconnect.m,v $
# Revision 1.1.1.1  1998/05/19 20:24:09  jwe
#
# Revision 1.3  1997/03/03 19:21:06  hodel
# removed calls to packsys: a.s.hodel@eng.auburn.edu
#
# Revision 1.2  1997/02/13 14:23:18  hodel
# fixed bug in continuous<->discrete loop connection check.
# a.s.hodel@eng.auburn.edu
#

  save_val = implicit_str_to_num_ok;	# save for later
  implicit_str_to_num_ok = 1;

  if( (nargin < 3) | (nargin > 5) )
    usage("retsys = sysconnect(sys,output_list,input_list[,order,tol])");
  endif

  # check order
  if(nargin <= 3)
    order = 0;
  elseif( (order != 0) & (order != 1) )
    error("sysconnect: order must be either 0 or 1")
  endif

  if (nargin <= 4)
    tol = 200*eps;
  elseif( !is_sample(tol) )
    error("sysconnect: tol must be a positive scalar");
  elseif(tol > 1e2*sqrt(eps))
    warning(["sysconnect: tol set to large value=",num2str(tol), ...
	", eps=",num2str(eps)])
  endif

  # verify sizes,format of input, output lists
  if( min(size(output_list))*min(size(input_list)) != 1)
    error("output_list and input_list must be vectors");
  else
    lo = length(output_list);
    li = length(input_list);
    if(lo != li)
      error("output_list and input_list must be of the same length")
    endif
    
    if(is_duplicate_entry(output_list) | is_duplicate_entry(input_list) )
      error("duplicate entry in input_list and/or output_list");
    endif
  endif
  
  mm = rows(sys.inname);
  pp = rows(sys.outname);
  nn = rows(sys.stname);

  if( !is_struct(sys))
    error("sys must be in structured system form")
  elseif(pp < li)
    error(["length(output_list)=",num2str(li),", sys has only ", ...
	num2str(pp),"system outputs"])
  elseif(mm < li)
    error(["length(input_list)=",num2str(li),", sys has only ", ...
	num2str(mm),"system inputs"])
  endif

  # check that there are enough inputs/outputs in the system for the lists
  if(max(input_list) > mm) 
    error("max(input_list) exceeds the number of inputs");
  elseif(max(output_list) > pp)
    error("max(output_list) exceeds the number of outputs");
  endif

  output_list = reshape(output_list,1,length(output_list));

  # make sure we're in state space form
  sys = sysupdate(sys,'ss');

  # permute rows and columns of B,C,D matrices into pseudo-dgkf form...
  all_inputs = sysreorder(mm,input_list);
  all_outputs = sysreorder(pp,output_list);

  sys.b = sys.b(:,all_inputs);
  sys.c = sys.c(all_outputs,:);
  sys.d = sys.d(all_outputs,all_inputs);
  sys.yd = sys.yd(all_outputs);

  # m1, p1 = number of inputs, outputs that are not being connected
  m1 = mm-li;
  p1 = pp-li;

  # m2, p2: 1st column, row of B, C that is being connected
  m2 = m1+1;
  p2 = p1+1;

  # partition system into a DGKF-like form; the loop is closed around
  # B2, C2
  if(m1 > 0)
    B1 = sys.b(:,1:m1);
    D21= sys.d(p2:pp,1:m1);
  endif
  B2 = sys.b(:,m2:mm);
  if(p1 > 0)
    C1 = sys.c(1:p1,:);
    D12= sys.d(1:p1,m2:mm);
  endif
  C2 = sys.c(p2:pp,:);
  if(m1*p1 > 0)
    D11= sys.d(1:p1,1:m1);
  endif
  D22= sys.d(p2:pp,m2:mm);

  if(norm(D22))
    warning("sysconnect: possible algebraic loop, D22 non-zero");
    D22i = (eye(size(D22))-D22);
    C2h = D22i\C2;
    if(m1 > 0)
      D21h = D22i\D21;
    endif
    D22h = D22i\D22;
  else
    C2h = C2;
    if(m1 > 0)
      D21h = D21;
    endif
    D22h = D22;

  endif

  # check cont state -> disc output -> cont state
  dyi = find(sys.yd(p2:pp));

  #disp("sysconnect: dyi=")
  #dyi
  #sys.n
  #disp("/sysconnect");

  if( (sys.n > 0) & find(dyi > 0) )
    B2con = B2(1:sys.n,dyi);	# connection to cont states
    C2hd = C2h(dyi,1:sys.n);	# cont states -> outputs
  else
    B2con = C2hd = [];
  endif

  if(max(size(B2con)) & max(size(C2hd)) )
    if(norm(B2con*C2hd))
      warning("sysconnect: cont-state -> disc output -> cont state derivative");
      warning("    connection made; resulting system may not be meaningful");
    endif
  endif

  Ac = sys.a+B2*C2h;
  if(m1 > 0)
    B1c = B1 + B2*D21h;
  endif
  B2c = B2*(eye(size(D22h)) + D22h);
  if(p1*m1 > 0)
    D11c = D11 + D12*D21h;
  endif
  if(p1 > 0)
    C1c  = C1+D12*C2h;
    D12c = D12*(eye(size(D22h))+D22h);
  endif

  # construct system data structure
  if(m1 > 0)
   Bc = [B1c B2c];
  else
   Bc = B2c;
  endif

  if(p1 > 0)
    Cc = [C1c;C2h];
  else
    Cc = C2h;
  endif

  if(m1*p1 > 0)
    Dc = [D11c,D12c; D21h,D22h];
  elseif(m1 > 0)
    Dc = [D21h, D22h];
  elseif(p1 > 0)
    Dc = [D12c; D22h];
  else
    Dc = D22h;
  endif 

  # permute rows and columns of Bc, Cc, Dc back into original order
  Im = eye(mm,mm);
  Pi = Im(:,all_inputs);
  back_inputs = Pi*[1:mm]';

  Ip = eye(pp,pp);
  Po = Ip(:,all_outputs);
  back_outputs = Po*[1:pp]';

  Bc = Bc(:,back_inputs);
  Cc = Cc(back_outputs,:);
  Dc = Dc(back_outputs,back_inputs);
  sys.yd = sys.yd(back_outputs);

  sys.a = Ac;
  sys.b = Bc;
  sys.c = Cc;
  sys.d = Dc;

  for ii = 1:length(input_list)
    strval = [dezero(sys.inname(input_list(ii),:)),"*"];
    sys.inname(input_list(ii),(1:length(strval))) = [strval];
  endfor
  
  if (sys.sys(1) == 0)
    sysupdate(sys,'tf');
  elseif (sys.sys(1) == 1)
    sysupdate(sys,'zp');
  endif

  implicit_str_to_num_ok = save_val;	# restore value  

endfunction
