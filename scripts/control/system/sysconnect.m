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
## Software Foundation, 59 Temple Place, Suite 330, Boston, MA 02111 USA.

## -*- texinfo -*-
## @deftypefn {Function File} {@var{clsys} =} sysconnect (@var{sys}, @var{out_idx}, @var{in_idx}, @var{order}, @var{tol})
## Close the loop from specified outputs to respective specified inputs
##
## @strong{Inputs}
## @table @var
## @item   sys
## System data structure.
## @item   out_idx
## @itemx  in_idx
## Names or indices of signals to connect (see @code{sysidx}).
## The output specified by @math{out_idx(ii)} is connected to the input
## specified by @math{in_idx(ii)}.
## @item   order
## logical flag (default = 0)
## @table @code
## @item        0
## Leave inputs and outputs in their original order.
## @item        1
## Permute inputs and outputs to the order shown in the diagram below.
## @end table
## @item     tol
## Tolerance for singularities in algebraic loops, default: 200@code{eps}.
## @end table
##
## @strong{Outputs}
## @table @var
## @item clsys
## Resulting closed loop system.
## @end table
##
## @strong{Method}
##
## @code{sysconnect} internally permutes selected inputs, outputs as shown
## below, closes the loop, and then permutes inputs and outputs back to their
## original order
## @example
## @group
##                  --------------------
##  u_1       ----->|                  |----> y_1
##                  |        sys       |
##          old u_2 |                  |
## u_2* ---->(+)--->|                  |----->y_2
## (in_idx)   ^     --------------------    | (out_idx)
##            |                             |
##            -------------------------------
## @end group
## @end example
## The input that has the summing junction added to it has an * added to
## the end  of the input name.
## @end deftypefn

## Author: A. S. Hodel <a.s.hodel@eng.auburn.edu>
## Created: August 1995
## modified by John Ingram July 1996

function sys = sysconnect (sys, output_list, input_list, order, tol)

  if( (nargin < 3) | (nargin > 5) )
    usage("retsys = sysconnect(sys,output_list,input_list[,order,tol])");
  endif

  ## check order
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

  ## convert signal names to indices
  if(is_signal_list(input_list) | isstr(input_list))
    input_list = sysidx(sys,"in",input_list);
  endif
  if(is_signal_list(output_list) | isstr(output_list))
    output_list = sysidx(sys,"out",output_list);
  endif

  ## verify sizes,format of input, output lists
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

  [nc,nz,mm,pp] = sysdimensions(sys);
  nn = nc+nz;

  if( !isstruct(sys))
    error("sys must be in structured system form")
  elseif(pp < li)
    error(["length(output_list)=",num2str(li),", sys has only ", ...
        num2str(pp),"system outputs"])
  elseif(mm < li)
    error(["length(input_list)=",num2str(li),", sys has only ", ...
        num2str(mm),"system inputs"])
  endif

  ## check that there are enough inputs/outputs in the system for the lists
  if(max(input_list) > mm)
    error("max(input_list) exceeds the number of inputs");
  elseif(max(output_list) > pp)
    error("max(output_list) exceeds the number of outputs");
  endif

  output_list = reshape(output_list,1,length(output_list));

  ## make sure we're in state space form
  sys = sysupdate (sys, "ss");

  ## permute rows and columns of B,C,D matrices into pseudo-dgkf form...
  all_inputs = sysreorder(mm,input_list);
  all_outputs = sysreorder(pp,output_list);

  [aa,bb,cc,dd] = sys2ss(sys);
  bb = bb(:,all_inputs);
  cc = cc(all_outputs,:);
  dd = dd(all_outputs,all_inputs);

  yd = sysgetsignals(sys,"yd");
  yd = yd(all_outputs);

  ## m1, p1 = number of inputs, outputs that are not being connected
  m1 = mm-li;
  p1 = pp-li;

  ## m2, p2: 1st column, row of B, C that is being connected
  m2 = m1+1;
  p2 = p1+1;

  ## partition system into a DGKF-like form; the loop is closed around
  ## B2, C2
  if(m1 > 0)
    B1 = bb(:,1:m1);
    D21= dd(p2:pp,1:m1);
  endif
  B2 = bb(:,m2:mm);
  if(p1 > 0)
    C1 = cc(1:p1,:);
    D12= dd(1:p1,m2:mm);
  endif
  C2 = cc(p2:pp,:);
  if(m1*p1 > 0)
    D11= dd(1:p1,1:m1);
  endif
  D22= dd(p2:pp,m2:mm);

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

  ## check cont state -> disc output -> cont state
  dyi = find(yd(p2:pp));

  ## disp("sysconnect: dyi=")
  ## dyi
  ## nc
  ## disp("/sysconnect");

  if( (nc > 0) & find(dyi > 0) )
    B2con = B2(1:nc,dyi);       # connection to cont states
    C2hd = C2h(dyi,1:nc);       # cont states -> outputs
  else
    B2con = C2hd = [];
  endif

  if(max(size(B2con)) & max(size(C2hd)) )
    if(norm(B2con*C2hd))
      warning("sysconnect: cont-state -> disc output -> cont state derivative");
      warning("    connection made; resulting system may not be meaningful");
    endif
  endif

  Ac = aa+B2*C2h;
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

  ## construct system data structure
  if(m1 > 0)
   Bc = [B1c, B2c];
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

  ## permute rows and columns of Bc, Cc, Dc back into original order
  Im = eye(mm,mm);
  Pi = Im(:,all_inputs);
  back_inputs = Pi*[1:mm]';

  Ip = eye(pp,pp);
  Po = Ip(:,all_outputs);
  back_outputs = Po*[1:pp]';

  Bc = Bc(:,back_inputs);
  Cc = Cc(back_outputs,:);
  Dc = Dc(back_outputs,back_inputs);
  yd = yd(back_outputs);

  ## rebuild system
  Ts = sysgettsam(sys);
  [stnam,innam,outnam] = sysgetsignals(sys);
  sys = ss(Ac,Bc,Cc,Dc,Ts,nc,nz,stnam,innam,outnam,find(yd));

  ## update connected input names
  for ii = 1:length(input_list)
    idx = input_list(ii);
    tmpval = sysgetsignals(sys,"in",idx);
    strval = sprintf("%s*",tmpval{1} );
    sys = syssetsignals(sys,"in",strval,idx);
  endfor

  ## maintain original system type if it was SISO
  if (strcmp (sysgettype (sys), "tf"))
    sysupdate (sys, "tf");
  elseif (strcmp (sysgettype (sys),"zp"))
    sysupdate (sys, "zp");
  endif

endfunction
