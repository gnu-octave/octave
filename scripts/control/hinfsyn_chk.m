## Copyright (C) 1996 Auburn University.  All Rights Reserved
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
## Software Foundation, 59 Temple Place, Suite 330, Boston, MA 02111 USA. 

## -*- texinfo -*-
## @deftypefn {Function File} {[@var{retval}, @var{Pc}, @var{Pf}] =} hinfsyn_chk(@var{A}, @var{B1}, @var{B2}, @var{C1}, @var{C2}, @var{D12}, @var{D21}, @var{g}, @var{ptol})
##  Called by @code{hinfsyn} to see if gain @var{g} satisfies conditions in 
## Theorem 3 of
##  Doyle, Glover, Khargonekar, Francis, "State Space Solutions to Standard
##  H2 and Hinf Control Problems", IEEE TAC August 1989
##  
## @strong{Warning} Do not attempt to use this at home; no argument checking performed.
## 
## @strong{Inputs} as returned by @code{is_dgkf}, except for:
## @table @var
## @item g 
## candidate gain level
## @item ptol
##  as in @code{hinfsyn}
## @end table
## 
## @strong{Outputs}
## @table @var
## @item retval
##  1 if g exceeds optimal Hinf closed loop gain, else 0
## @item Pc
##  solution of "regulator" H-inf ARE
## @item Pf
##  solution of "filter" H-inf ARE
## @end table
## Do not attempt to use this at home; no argument checking performed.
## @end deftypefn 

function [retval,Pc,Pf] = hinfsyn_chk(A,B1,B2,C1,C2,D12,D21,g,ptol)

  ## A. S. Hodel August 1995

  Pc = Pf = [];

  ## Construct the two Hamiltonians
  g2 = 1/(g*g);
  Hc = [ A ,  g2*B1*B1' - B2*B2'; -C1'*C1 , -A'];
  Hf = [ A' , g2*C1'*C1 - C2'*C2; -B1*B1' , -A];

  ## check if Hc, Hf are in dom(Ric)
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

  ## Solve ARE's
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
 
  ## all conditions met.
  retval = 1;

endfunction
