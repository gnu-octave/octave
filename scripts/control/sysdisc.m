## Copyright (C) 1996 Auburn University.  All Rights Reserved.
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
## @deftypefn {Function File } { [@var{dsys}, @var{Adc}, @var{Cdc}] =} sysdisc (@var{sys})
## 
## @strong{Inputs}
## @var{sys} = system data structure
## 
## @strong{Outputs}
## @table @var
## @item dsys
##  purely discrete portion of sys (returned empty if there is
##           no purely discrete path from inputs to outputs)
## @item    Adc, Cdc
##  connections from continuous states to discrete states and discrete
##     outputs, respectively.
## @end table
## 
## @end deftypefn
 
function [dsys,Adc,Cdc] = sysdisc(sys)

  ## function [dsys,Adc,Cdc] = sysdisc(sys)
  ## inputs: sys = system data structure
  ## outputs:
  ##    dsys: purely discrete portion of sys (returned empty if there is
  ##          no purely discrete path from inputs to outputs)
  ##    Adc, Cdc: connections from continuous states to discrete states/discrete
  ##    outputs, respectively.
  ##

  save_val = implicit_str_to_num_ok;	# save for later
  save_empty = empty_list_elements_ok;
  empty_list_elements_ok = implicit_str_to_num_ok = 1;


  if (nargin != 1)
    usage("[dsys,Adc,Cdc] = sysdisc(sys)");
  elseif (!is_struct(sys))
    error("sys must be in system data structure form");
  endif

  sys = sysupdate(sys,"ss");
  [n_tot,st_c,st_d,y_c,y_d] = syscont_disc(sys);	# get ranges

  ## assume there's nothing there; build partitions as appropriate
  Add = Adc = Bdd = Cdd = Cdc = Ddd = [];
  
  if(isempty(st_d) & isempty(y_d))
    error("sysdisc: expecting discrete states and/or continous outputs");
  elseif (isempty(st_d))
    warning("sysdisc: no discrete states");
  elseif(isempty(y_d))
    warning("sysdisc: no discrete outputs");
  endif

  [aa,bb,cc,dd] = sys2ss(sys);
  if(!isempty(st_d) )
    Add = aa( st_d , st_d);
    stname = sysgetsignals(sys,"st",st_d);
    Bdd = bb( st_d , :);
    if(!isempty(st_c))
      Adc = aa( st_d , st_c);
    endif
    if(!isempty(y_d))
      Cdd = cc(y_d , st_d);
    endif
  else
    stname = [];
  endif
  if(!isempty(y_d))
    Ddd = dd(y_d , :);
    outname = sysgetsignals(sys,"out",y_d);
    if(!isempty(st_c))
      Cdc = cc(y_d , st_c);
    endif
  else
    outname=[];
  endif
  inname = sysgetsignals(sys,"in");
  outlist = 1:rows(outname);

  if(!isempty(outname))
    tsam = sysgettsam(sys);
    [nc,nz] = sysdimensions(sys);
    dsys = ss2sys(Add,Bdd,Cdd,Ddd,tsam,0,nz,stname,inname,outname,outlist);
  else
    dsys=[];
  endif
  implicit_str_to_num_ok = save_val;	# restore value
  empty_list_elements_ok = save_empty;

endfunction
