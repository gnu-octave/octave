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
 
function [sys] = sysmult(Asys,Bsys)
#
# [sys] = sysmult(Asys,Bsys)
#
# returns sys = Asys*Bsys
#
# This function takes two systems, Asys and Bsys, and multiplies them together.
# This has the effect of connecting the outputs of Bsys to the inputs of Asys.
#
#
#     u   ----------     ----------
#     --->|  Bsys  |---->|  Asys  |--->
#         ----------     ----------
#
# A warning occurs if there is direct feed-through
# from an input of Bsys or a continuous state of Bsys through a discrete 
# output of Bsys to a continuous state or output in Asys (system data structure form 
# does not recognize discrete inputs)

# Written by John Ingram July 1996

  save_val = implicit_str_to_num_ok;	# save for later
  implicit_str_to_num_ok = 1;

  if(nargin != 2)
    usage("sysmult:  [sys] = sysmult(Asys,Bsys)");
  endif

  # check inputs
  if(!is_struct(Asys) | !is_struct(Bsys))
    error("Both Asys and Bsys must be in system data structure form")
  endif

  # check for compatibility
  [An,Anz,Am,Ap] = sysdimensions(Asys);
  [Bn,Bnz,Bm,Bp] = sysdimensions(Bsys);
  if(Bp != Am)
    error(["Bsys has ",num2str(Bp)," outputs, Asys has ",num2str(Am), ...
	" inputs; mismatch."]);
  endif

  [Aa,Ab,Ac,Ad,Atsam,An,Anz,Astname,Ainname,Aoutname,Ayd] = sys2ss(Asys);
  [Ba,Bb,Bc,Bd,Btsam,Bn,Bnz,Bstname,Binname,Boutname,Byd] = sys2ss(Bsys);

  if(Byd)
    # check direct feed-through of inputs through discrete outputs
    alist = find(Byd);
    if(An)
      bd = Ab(1:An)* Bd(alist,:);	
      if(norm(bd,1))
        warning("sysmult: inputs -> Bsys discrete outputs -> continous states of Asys");
      endif
    endif
    # check direct feed-through of continuous state through discrete outputs
    if(Bn)
      bc = Ab(1:An)* Bc(alist,1:(Bn));	
      if( norm(bc,1) )
        warning("sysmult: Bsys states -> Bsys discrete outputs -> continuous states of Asys");
      endif
    endif
  endif

  # change signal names to avoid spurious warnings from sysgroup
  Asys = syssetsignals(Asys,"in",sysdefioname(Am,"A_sysmult_tmp_name"));
  Bsys = syssetsignals(Bsys,"out",sysdefioname(Bp,"B_sysmult_tmp_name"));

  sys = sysgroup(Asys,Bsys);

  # connect outputs of B to inputs of A
  sys = sysconnect(sys,Ap+(1:Bp),1:Am);
 
  # now keep only  outputs of A and inputs of B
  sys = sysprune(sys,1:Ap,Am+(1:Bm));

  implicit_str_to_num_ok = save_val;	# restore value  
endfunction  
  
