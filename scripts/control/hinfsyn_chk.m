# Copyright (C) 1996 Auburn University.  All Rights Reserved
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
# Software Foundation, 59 Temple Place, Suite 330, Boston, MA 02111 USA. 
 
function [retval,Pc,Pf] = hinfsyn_chk(A,B1,B2,C1,C2,D12,D21,g,ptol)
  # [retval,Pc,Pf] = hinfsyn_chk(A,B1,B2,C1,C2,D12,D21,g,ptol)
  #
  # Called by hinfsyn to see if gain g satisfies conditions in Theorem 3 of
  # Doyle, Glover, Khargonekar, Francis, "State Space Solutions to Standard
  # H2 and Hinf Control Problems", IEEE TAC August 1989
  # 
  # inputs:
  #   g = candidate gain level
  #   ptol: as in hinfsyn
  #   remaining parameters as returned by is_dgkf
  # outputs: 
  #   retval: = 1 if g exceeds optimal Hinf closed loop gain, else 0
  #   Pc: solution of "regulator" H-inf ARE
  #   Pf: solution of "filter" H-inf ARE
  #
  # Do not attempt to use this at home; no argument checking performed.

  # A. S. Hodel August 1995

  Pc = Pf = [];

  # Construct the two Hamiltonians
  g2 = 1/(g*g);
  Hc = [ A ,  g2*B1*B1' - B2*B2'; -C1'*C1 , -A'];
  Hf = [ A' , g2*C1'*C1 - C2'*C2; -B1*B1' , -A];

  # check if Hc, Hf are in dom(Ric)
  Hcminval = min(abs(real(eig(Hc))));
  Hfminval = min(abs(real(eig(Hf))));
  if(Hcminval < ptol);
    disp("hinfsyn_chk: Hc is not in dom(Ric)");
    retval = 0;
    return
  endif
  if(Hfminval < ptol)
    disp("hinfsyn_chk: Hf is not in dom(Ric)");
    retval = 0;
    return
  endif

  #Solve ARE's
  Pc = are(A, B2*B2'-g2*B1*B1',C1'*C1);
  Pf = are(A',C2'*C2-g2*C1'*C1,B1*B1');

  Pceig = eig(Pc);
  Pfeig = eig(Pf);
  Pcfeig = eig(Pc*Pf);

  if(min(Pceig) < -ptol)
    disp("hinfsyn_chk: Pc is not >= 0");
    retval = 0;
    return
  endif
  if(min(Pfeig) < -ptol)
    disp("hinfsyn_chk: Pf is not >= 0");
    retval = 0;
    return
  endif
  if(max(abs(Pcfeig)) >= g*g)
    disp("hinfsyn_chk: rho(Pf*Pc) is not < g^2");
    retval = 0;
    return
  endif
 
  # all conditions met.
  retval = 1;

endfunction
