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
 
function sys = sysgroup(Asys,Bsys)
# function sys = sysgroup(Asys,Bsys)
# Combines two system data structures into a single system
#
# input: Asys, Bsys: system data structures
# output: sys: Asys and Bsys are combined into a single system:
#
#              __________________
#              |    ________    |
#     u1 ----->|--> | Asys |--->|----> y1
#              |    --------    |
#              |    ________    |
#     u2 ----->|--> | Bsys |--->|----> y2
#              |    --------    |
#              ------------------
#                   Ksys
# 
# The function also rearranges the A,B,C matrices so that the 
# continuous states come first and the discrete states come last.
# If there are duplicate names, the second name has a unique suffix appended
# on to the end of the name.

# A. S. Hodel August 1995
# modified by John Ingram July 1996

  save_val = implicit_str_to_num_ok;	# save for later
  implicit_str_to_num_ok = 1;

  save_emp = empty_list_elements_ok;
  empty_list_elements_ok = 1;

  if(nargin ~= 2)
    usage("sys = sysgroup(Asys,Bsys)");
  elseif( !is_struct(Asys) | !is_struct(Bsys) )
    error("sysgroup: input arguments must both be structured systems");
  endif

  # extract information from Asys, Bsys to consruct sys
  Asys = sysupdate(Asys,"ss");
  Bsys = sysupdate(Bsys,"ss");
  [n1,nz1,m1,p1] = sysdimensions(Asys);
  [n2,nz2,m2,p2] = sysdimensions(Bsys);
  [Aa,Ab,Ac,Ad,Atsam,An,Anz,Ast,Ain,Aout,Ayd] = sys2ss(Asys);
  [Ba,Bb,Bc,Bd,Btsam,Bn,Bnz,Bst,Bin,Bout,Byd] = sys2ss(Bsys);
  nA = An + Anz;
  nB = Bn + Bnz;

  if(p1*m1*p2*m2 == 0)
    error("sysgroup: argument lacks inputs and/or outputs");

  elseif((Atsam + Btsam > 0) & (Atsam * Btsam == 0) )
    warning("sysgroup: creating combination of continuous and discrete systems")

  elseif(Atsam != Btsam)
    error("sysgroup: Asys.tsam=%e, Bsys.tsam =%e", Atsam, Btsam);
  endif

  A = [Aa,zeros(nA,nB); zeros(nB,nA),Ba];
  B = [Ab,zeros(nA,m2); zeros(nB,m1),Bb];
  C = [Ac,zeros(p1,nB); zeros(p2,nA),Bc];
  D = [Ad,zeros(p1,m2); zeros(p2,m1),Bd];
  tsam = max(Atsam,Btsam);

  # construct combined signal names; stnames must check for pure gain blocks
  if(isempty(Ast))
    stname = Bst;
  elseif(isempty(Bst))
    stname = Ast;
  else
    stname  = append(Ast, Bst);
  endif
  inname  = append(Ain, Bin);
  outname = append(Aout,Bout);

  # Sort states into continous first, then discrete
  dstates = ones(1,(nA+nB));
  if(An)
    dstates(1:(An)) = zeros(1,An);
  endif
  if(Bn)
    dstates((nA+1):(nA+Bn)) = zeros(1,Bn);
  endif
  [tmp,pv] = sort(dstates);
  A = A(pv,pv);
  B = B(pv,:);
  C = C(:,pv);
  stname = stname(pv);

  # check for duplicate signal names
  inname = sysgroupn(inname,"input");
  stname = sysgroupn(stname,"state");
  outname = sysgroupn(outname,"output");

  # mark discrete outputs
  outlist = find([Ayd, Byd]);

  # build new system
  sys = ss2sys(A,B,C,D,tsam,An+Bn,Anz+Bnz,stname,inname,outname);

  implicit_str_to_num_ok = save_val;	# restore value  
  empty_list_elements_ok = save_emp;

endfunction
