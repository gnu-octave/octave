# Copyright (C) 1996,1998 Auburn University.  All Rights Reserved.
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
 
function retsys = sysappend(sys,b,c,d,outname,inname,yd)
  # 
  # function retsys = sysappend(sys,b[,c,d,outname,inname,yd])
  #
  # This function appends new inputs and/or outputs to a system
  # Inputs:
  #	sys:  system data structure
  #	b: matrix to be appended to sys "B" matrix (empty if none)
  #	c: matrix to be appended to sys "C" matrix (empty if none)
  #     d: revised sys d matrix (can be passed as [] if the revised d is all 
  #        zeros)
  #     outname: names for new outputs
  #     inname: names for new inputs
  #     yd: indicator for which new outputs are continuous/discrete 
  #         (yd(i) = 0 or , respectively)
  # result:
  #   sys.b := [sys.b , b]
  #   sys.c := [sys.c  ]
  #            [ c     ]
  #   sys.d := [sys.d | D12 ]
  #            [D21   | D22 ]
  #         where D12, D21, and D22 are the appropriate dimensioned blocks
  #         of the input parameter d.  The leading block D11 of d is ignored.
  # If inname and outname are not given as arguments, the new inputs and 
  # outputs are be assigned default names.  
  # yd is a vector of length rows(c), and indicates which new outputs are
  # discrete (yd(ii) = 1) and which are continuous (yd(ii) = 0).
  # Default value for yd is:
  #     sys = continuous or mixed: yd = zeros(1,rows(c))
  #     sys = discrete:            yd = ones(1,rows(c))
  
  # written by John Ingram August 1996
  
  sav_implicit_str_to_num_ok = implicit_str_to_num_ok;	# save for later
  sav_empty_list_elements_ok = empty_list_elements_ok;

  empty_list_elements_ok = 1;                implicit_str_to_num_ok = 1;
  
  # check input arguments
  if ( (nargin < 2) | (nargin > 7) | (!is_struct(sys)))
    usage("retsys = sysappend(sys,b,c[,d,outname,inname,yd]) ");
  elseif(!is_struct(sys))
    error("sys must be a system data structure");
  endif
  
  # default system type must be state space form
  [Aa,Ab,Ac,Ad,Ats,Ann,Anz,Ast,Ain,Aout,Ayd] = sys2ss(sys);
  [Ann,Anz,Am,Ap] = sysdimensions(sys);

  #default c
  if(nargin < 3)      c = [];                                endif
  
  #default d
  if(nargin < 4)     make_d = 1;
  elseif(isempty(d)) make_d = 1;
  else               make_d = 0;                             endif
  if(make_d)         d = zeros(rows(c)+Ap,columns(b) + Am);  endif

  #
  # Append new input(s) if any
  Bm = max(columns(d),columns(b)+Am);
  if(Bm != Am)    
    # construct new signal names
    if(nargin >= 6)   # new names were passed
      if(!isstr(inname))
        error("inname must be a string");
      elseif(rows(inname) != (Bm - Am))
        error(sprintf("%d new inputs requested; inname(%dx%d)", ...
	  (Bm-Am),rows(inname),columns(inname)));
      endif
    else
      inname = sysdefioname(Bm,"u",(Am+1));
    endif
    if(Am)   Ain = append(Ain,inname);
    else     Ain = inname;		endif

    # default b matrix
    if(isempty(b))     b  = zeros(Ann+Anz,(Bm-Am));          
    elseif(rows(b) != Ann+Anz | columns(b) != (Bm-Am))
        error(sprintf("b(%dx%d); should be (%dx%d)", rows(b), columns(b), ...
          (Ann+Anz), (Bm-Am)));
    endif

    # append new b matrix
    Ab = [Ab,b];    # empty_list_elements_ok=1 makes this ok
  endif

  #
  # Append new output(s) if any
  Bp = max(rows(d),rows(c)+Ap);
  if(Bp != Ap)  

    # construct new signal names, output classification
    if(nargin >= 5)  # new names were passed
      if(!isstr(outname))
        error("outname must be a string");
      elseif(rows(outname) != (Bp - Ap))
        error(sprintf("%d new outputs requested; outname(%dx%d)", ...
          (Bp-Ap),rows(outname),columns(outname)));
      endif
    else
      outname = sysdefioname(Bp,"y",(Ap+1));
    endif
    if(Ap)   Aout = append(Aout,outname);
    else     Aout = outname;                endif

    # construct new yd entries
    if(nargin == 7)
      if(!is_vector(yd))
        error(sprintf("yd(%dx%d) must be a vector",rows(yd),columns(yd)))
      elseif(rows(c) != length(yd) & rows(d) != length(yd))
        error(sprintf("length(yd) = %d; c(%dx%d), d(%dx%d); mismatch", ...
	  length(yd), rows(c), columns(c),rows(d),columns(d)));
      endif
    else
      # default yd values
      yd = ones(1,Bp)*( (Ats > 0) & (Ann == 0)  & isempty(find(Ayd == 0)) ) ;
    endif
    Ayd = [vec(Ayd);vec(yd)];

    # default c matrix
    if(isempty(c))      c = zeros((Bp-Ap),Ann+Anz);          
    elseif(columns(c) != Ann+Anz | rows(c) != (Bp-Ap))
        error(sprintf("c(%dx%d); should be (%dx%d)", rows(c), columns(c), ...
          (Bp-Ap), (Ann+Anz) ));
    endif

    # append new c matrix
    Ac = [Ac;c];    # empty_list_elements_ok=1 makes this ok
  endif

  # check d matrix
  if(isempty(d)) d = zeros(Bp,Bm);
  elseif(rows(d) != Bp | columns(d) != Bm)
    error(sprintf("d(%dx%d) should be (%dx%d)",rows(d), columns(d), Bp, Bp));
  endif

  # Splice in original D matrix  
  if(Am & Ap)          d(1:Ap, 1:Am) = Ad;       endif
  Ad = d;
  
  # construct return system
  retsys = ss2sys(Aa,Ab,Ac,Ad,Ats,Ann,Anz,Ast,Ain,Aout,find(Ayd == 1));
  
  implicit_str_to_num_ok = sav_implicit_str_to_num_ok;	# restore value
  empty_list_elements_ok = sav_empty_list_elements_ok;

endfunction
