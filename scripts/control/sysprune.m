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
## @deftypefn {Function File } { @var{retsys} =} sysprune ( @var{Asys}, @var{out_idx}, @var{in_idx})
## Extract specified inputs/outputs from a system
## 
## @strong{Inputs}
## @table @var
## @item   Asys
## system data structure
## @item out_idx,in_idx
##  list of connections indices; the new
##        system has outputs y(out_idx(ii)) and inputs u(in_idx(ii)).
##        May select as [] (empty matrix) to specify all outputs/inputs.
## @end table
## 
## @strong{Outputs}
## @var{retsys}: resulting system
## @example
## @group
##            ____________________
## u1 ------->|                  |----> y1
##  (in_idx)  |       Asys       | (out_idx)
## u2 ------->|                  |----| y2
##   (deleted)-------------------- (deleted)   
## @end group
## @end example
## 
## @end deftypefn
## 
 
function sys = sysprune (sys, output_idx, input_idx, state_idx)

  ## A. S. Hodel August 1995
  ## Updated by John Ingram 7-15-96

  if( nargin < 3 | nargin > 4  )
    usage("retsys = sysprune(sys,output_idx,input_idx{,state_idx})");
  elseif(nargin < 4)
    state_idx = [];
  endif

  ## default: no action
  [nn,nz,mm,pp] = sysdimensions(sys);
  if(isempty(output_idx)) output_idx = 1:pp; endif
  if(isempty(input_idx)) input_idx = 1:mm; endif
  if(isempty(state_idx)) state_idx = 1:(nn+nz); endif

  ## check dimensions
  if( !(is_vector(output_idx) | isempty(output_idx) )  )
    if(!is_matrix(output_idx)) 
      error("sysprune: bad argument passed for output_idx");
    else 
      error("sysprune: output_idx (%d x %d) must be a vector or empty", ...
        rows(output_idx),columns(output_idx));
    endif
  elseif(is_duplicate_entry(output_idx))
     error("sysprune: duplicate entries found in output_idx");
  endif

  if( !(is_vector(input_idx) | isempty(input_idx) )  )
    if(!is_matrix(input_idx)) 
      error("sysprune: bad argument passed for input_idx");
    else 
      error("sysprune: input_idx (%d x %d) must be a vector or empty", ...
        rows(input_idx),columns(input_idx));
    endif
  elseif(is_duplicate_entry(input_idx))
     error("sysprune: duplicate entries found in input_idx");
  endif

  if( !(is_vector(state_idx) | isempty(state_idx) )  )
    if(!is_matrix(state_idx)) 
      error("sysprune: bad argument passed for state_idx");
    else 
      error("sysprune: state_idx (%d x %d) must be a vector or empty", ...
        rows(state_idx),columns(state_idx));
    endif
  elseif(nn+nz > 0)
    if(is_duplicate_entry(state_idx))
      error("sysprune: duplicate entries found in state_idx");
    endif
  endif

  lo = length(output_idx);
  li = length(input_idx);
  lst = length(state_idx);
  
  if( !is_struct(sys))
    error("Asys must be a system data structure (see ss2sys, tf2sys, or zp2sys)")
  elseif(pp < lo)
    error([num2str(lo)," output_idx entries, system has only ", ...
	num2str(pp)," outputs"]);
  elseif(mm < li)
    error([num2str(li)," input_idx entries, system has only ", ...
	num2str(mm)," inputs"]);
  elseif(nn+nz < lst)
    error([num2str(lst)," state_idx entries, system has only ", ...
	num2str(nn+nz)," states"]);
  endif

  [aa,bb,cc,dd,tsam,nn,nz,stnam,innam,outnam,yd] = sys2ss(sys);

  ## check for legal state permutation
  if(nn & nz)
    c_idx = find(state_idx <= nn);
    if(!isempty(c_idx)) max_c = max(c_idx);
    else	        max_c = 0;            endif
    d_idx = find(state_idx > nn);
    if(!isempty(d_idx)) min_d = min(d_idx);
    else	        min_d = nn+nz;            endif
    if(max_c > min_d)
      warning("sysprune: state_idx(%d)=%d (discrete) preceeds", ...
	min_d,state_idx(min_d));
      warning("          state_idx(%d)=%d (continuous)",...
	max_c,state_idx(max_c));
      warning("sysprune: sys has %d continuous states, %d discrete states", ...
	nn,nz);
      error("continuous/discrete state partition not preserved ; see ss2sys");
    endif
  endif

  idx = input_idx;
  odx = output_idx;
  if(isempty(state_idx))
    idx = [];
    odx = [];
  endif
  aa = aa(state_idx,state_idx);
  bb = bb(state_idx,idx);
  cc = cc(odx,state_idx);
  dd = dd(output_idx,input_idx);
  yd = yd(output_idx); 

  innam  = innam(input_idx);
  outnam = outnam(output_idx); 
  stnam = stnam(state_idx);
  nn1 = length(find(state_idx <= nn));
  nz1 = length(find(state_idx > nn));
  sys = ss2sys(aa,bb,cc,dd,tsam,nn1,nz1,stnam,innam,outnam,find(yd));
endfunction
