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
  # $Revision: 1.2 $
  # $Log: sysappend.m,v $
  # Revision 1.2  1998/07/21 14:53:09  hodelas
  # use isempty instead of size tests; use sys calls to reduce direct
  # access to system structure elements
  #
  # Revision 1.1.1.1  1998/05/19 20:24:09  jwe
  #
  # Revision 1.2  1997/04/09 04:36:21  scotte
  # Fixed to properly handle new names (syschnames does not let you
  # change the number of names any more.)  a.s.hodel@eng.auburn.edu
  #
  
  save_val = implicit_str_to_num_ok;	# save for later
  implicit_str_to_num_ok = 1;
  
  # check input arguments
  if ( (nargin < 2) | (nargin > 7) | (!is_struct(sys)))
    usage("retsys = sysappend(sys,b,c[,d,outname,inname,yd]) ");
  endif
  
  # update system
  sys = sysupdate(sys,"ss");
  sys.sys = [2 0 0 1];
  
  #default c
  if(nargin < 3)
    c = [];
  endif
  
  #default d
  if(nargin < 4)
    make_d = 1;
  elseif(isempty(d))
    make_d = 1;
  else
    make_d = 0;
  endif
  if(make_d)
    d = zeros(rows(c)+rows(sys.c),columns(b) + rows(sys.inname));
    #disp("sysappend: default d=")
    #disp(d)
    #disp("/sysappend: default d=")
  endif

  # add default input names for new inputs (if any)
  old_m = rows(sys.inname);
  new_m = max(columns(d),columns(b)+old_m);
  old_inname = sys.inname;
  if(new_m)
    sys.inname = sysdefioname(new_m,"u");
    if(old_m)
      sys = syschnames(sys,"in",1:old_m,old_inname);
    endif
    if(nargin >= 6)
      # input names were specified, check dimensions
      if(rows(inname) != new_m - old_m)
        inname
        new_m
        old_m
        b
        d
        error(["inname has ",num2str(rows(inname))," entries, should have ", ...
	  num2str( new_m - old_m )]);
      endif
      sys = syschnames(sys,"in",(old_m+1):new_m,inname);
    endif
  endif

  #add default output names for new outputs (if any)
  old_p = rows(sys.outname);
  new_p = max(rows(d),rows(c)+old_p);

  # default yd
  if (nargin < 7)
    # discrete if positive sampling time, no continuous states/outputs
    yd = ones(1,new_p)*( ...
         (sys.tsam > 0) & (sys.n == 0)  & isempty(find(sys.yd == 0)) ) ;
  elseif ( (rows(c) != length(yd)) & (rows(d)) != yd)
    error(["rows(c)=",num2str(rows(c)),", length(yd)=",num2str(length(yd))])
  endif
  
  old_outname = sys.outname;
  if(new_p)
    sys.outname = sysdefioname(new_p,"y");
    if(old_p)
      sys = syschnames(sys,"out",1:old_p,old_outname); 
    endif
    if(nargin >= 5)
      # output names were specified, check dimensions
      if(rows(outname) != new_p - old_p)
        outname
        new_p
        old_p
        c
        d
        error(["outname has ",num2str(rows(outname)), ...
          " entries, should have ", num2str(new_p-old_p)]);
      endif
      sys = syschnames(sys,"out",(old_p+1):new_p,outname);
    endif
  endif

  sys = syschnames(sys,"yd",(old_p+1):new_p,yd);

  # append new b matrix (if any)
  if( max(size(b)) )
    if(rows(b) != sys.n + sys.nz)
      error(["sys has ",num2str(sys.n + sys.nz)," states; b has ", ...
  	num2str(rows(b))," rows"]);
    else
      if(old_m)
        sys.b = [sys.b,b];
      else
        sys.b = b;
      endif
    endif
  endif
  
  # append new c matrix (if any)
  if(max(size(c)))
    if(columns(c) != sys.n + sys.nz)
      error(["sys has ",num2str(sys.n + sys.nz)," states; c has ", ...
  	num2str(columns(c))," columns"]);
    else
      if(old_p)
        sys.c = [sys.c;c];
      else
        sys.c = c;
      endif
    endif
  endif
  
  if(max(size(d)))
    if( (rows(d) != new_p) | (columns(d) != new_m) )
      error(["d = (",num2str(rows(d)),"x",num2str(columns(d)), ...
  	") should be (",num2str(new_p),"x",num2str(new_m),")"]);
    else
      if(old_m & old_p)
        d(1:old_p, 1:old_m) = sys.d;
      endif
      sys.d = d;
    endif
  endif
  
  # append new input names

  retsys = sys;
  
  implicit_str_to_num_ok = save_val;	# restore value

endfunction
