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
 
function retsys = syschnames(sys,opt,list,names)
# retsys = syschnames(sys,opt,list,names)
# change the names of selected inputs, outputs and states.
# inputs:
# 	sys: system data structure
#	opt: []: change default name (output)
#	     "out": change selected output names
#	     "in": change selected input names
#	     "st": change selected state names	 
#	     "yd": change selected outputs from discrete to continuous or 
#		   from continuous to discrete.
#
#     	list: vector of indices of outputs, yd, inputs, or
#             states whose respective names should be changed
#
#    	names: strings or string arrays containing
#              names corresponding to the lists above.  To
# 	       change yd, use a vector.  Set the name to 0 for continuous, 
#	       or 1 for discrete.
# outputs:
#    retsys=sys with appropriate signal names changed 
#            (or yd values, where appropriate)

# Written by John Ingram August 1996
# $Revision: 1.3 $
# $Log: syschnames.m,v $
# Revision 1.3  1998/07/17 15:31:06  hodelas
# use is_empty instead of max(size(...))
#

  save_val = implicit_str_to_num_ok;	# save for later
  implicit_str_to_num_ok = 1;

  if (nargin != 4)
    usage("retsys=syschnames(sys,opt,list[,names])");
  elseif (!is_struct(sys))
    error("sys must be a system data structure");
  elseif (isempty(opt))
    opt = "out";
  elseif( ! isstr(opt) )
    error("opt must be a string");
  elseif( ! (strcmp(opt,"out") + strcmp(opt,"yd") + ...
    strcmp(opt,"in") + strcmp(opt,"st") ) )
    error("opt must be one of [], ""out"", ""yd"", ""in"", or ""st""");
  elseif(min(size(list)) > 1)
    disp("syschnames: list=")
    disp(list);
    error("list must be a vector")
  endif

  if (strcmp(opt,"out"))
    # update output names
    sys.outname = syschnamesl(list,sys.outname,names,"outlist");
  elseif (strcmp(opt,"in"))
    sys.inname = syschnamesl(list,sys.inname,names, "inlist");
  elseif (strcmp(opt,"st"))
    sys.stname = syschnamesl(list,sys.stname,names,"stlist");
  else
    # it's yd
    ym = max(size(list));
    ys = min(size(list));
    maxn = rows(sys.outname);

    if(ym != 0)
      if( (ym  > maxn) | (ys != 1) )
        error(["system has ",num2str(maxn)," outputs, ", ...
	  "list=(",num2str(rows(list)),"x",num2str(columns(list)),")"]);
      endif

      if( ym != length(names))
        error(["list has ",num2str(ym)," entries, and names has ",...
		num2str(length(names))," entries."]);
      endif

      if (min((names == 1) | (names == 0)) == 0)
        error("yd must be either zero or one");
      endif

      if (max(list) > maxn)
        error(["The largest entry in the list is ",num2str(max(list)),...
		" exceeds number of outputs=",num2str(maxn)])
      endif      

      if (max(names) && (sys.tsam == 0) )
        warning("syschnames: discrete outputs with tsam=0; setting tsam=1");
        disp("	effected outputs are:")
        if(is_siso(sys))
          outlist(sys.outname,"	",[],list);
        else
          outlist(sys.outname(list,:),"	",[],list);
        endif
        sys.tsam = 1;
      endif

      # reshape everything as a column vector
      sys.yd = reshape(sys.yd,length(sys.yd),1);
      names  = reshape(names,length(names),1);

	#disp("syschnames: list=")
	#disp(list)
	#disp("syschnames: names=")
	#disp(names)
	#disp("syschnames: sys.yd=")
	#disp(sys.yd)

      sys.yd(list) = names;
    
      if ((min(sys.yd) == 0) && (max(sys.yd) == 0) && (sys.tsam > 0) )
        warning("discrete states but no discrete outputs selected");
      endif

    endif
    
  endif

  retsys = sys;
  implicit_str_to_num_ok = save_val;	# restore value

  #disp("syschnames: exiting with")
  #retsys
  #disp("/syschnames")

endfunction
