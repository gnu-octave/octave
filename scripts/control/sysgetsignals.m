# Copyright (C) 1998 A. Scottedward Hodel 
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
 
function [stname,inname,outname,yd] = sysgetsignals(sys,sigid,signum,strflg)
  # [stname,inname,outname,yd] = sysgetsignals(sys)
  # -or- siglist = sysgetsignals(sys,sigid)
  # -or- signame = sysgetsignals(sys,sigid,signum{,strflg})
  # Get signal names from a system
  # inputs:
  #    sys: system data structure for the state space system
  #    sigid: signal id: string, must be one of:
  #      "in": input signals
  #     "out": output signals
  #      "st": state signals
  #      "yd": value of yd
  #    signum: index of signal  (e.g., out4 = sysgetsignals(sys,"out",4)
  #            sets out4 to the name of the 4th output)
  #    strflg: flag to return a string instead of a list;
  #           strflg = 0: (default) return a list
  #           strflg = 1: return a string; exits with an error if 
  #                       length(signum) > 1
  # outputs:
  #   if sigid is not specified:
  #      stname, inname, outname: signal names (lists);  names of states,
  #          inputs, and outputs, respectively
  #      yd: binary vector; yd(ii) is nonzero if output y is discrete.
  #   if sigid is specified but signum is not specified:
  #      stname: 
  #          is the list of state names (sigid = "st")
  #          is the list input names (sigid = "in")
  #          is the list output names (sigid = "out")
  #          is the logical vector indicate discrete outputs (sigid = "yd")
  #   if all three input arguments are specified:
  #          is the list of specified state, input, or output name(s) 
  #          (sigid = "st", "in", or "out").  
  #          is a logical flag indicating if output signum is continous
  #               (sigval=0) or discrete (sigval = 1)
  # 

  # Adapted from ss2sys

  if(nargin < 1 | nargin > 4 | nargout > 4)
    usage("[stname{,inname,outname,yd}] = sysgetsignals(sys{,sigid,signum})")
  elseif(nargin > 1 & nargout > 1)
    usage("sig = sysgetsignals(sys,sigid{,signum,strflg})")
  elseif( ! is_struct(sys) )
    error("input argument must be a system data structure");
  endif
  if(nargin < 4)  strflg = 0; endif
  if(nargin == 1)
    sys = sysupdate(sys,"ss");		#make sure ss is up to date
    stname = sysgetsignals(sys,"st");
    inname = sysgetsignals(sys,"in");
    outname = sysgetsignals(sys,"out");
    yd = sysgetsignals(sys,"yd");
  elseif(!(isstr(sigid) & min(size(sigid)) == 1))
    error(sprintf("sigid(%dx%d) must be a string)",rows(sigid),columns(sigid)));
  else
    if(strcmp("st",sigid))         stname = sys.stname;
    elseif(strcmp("in",sigid))     stname = sys.inname;
    elseif(strcmp("out",sigid))    stname = sys.outname;
    elseif(strcmp("yd",sigid))     stname = vec(sys.yd)';
    else
      error(sprintf("sigid=%s must be \"st\", \"in\", \"out\", or \"yd\"", ...
	sigid));
    endif
    if(nargin >= 3)
      if(signum > length(stname))
        error(sprintf("sysgetsignals(sys,\"%s\",%d):only %d entries.\n", ...
	  sigid,signum, rows(stname)));
      else
        if(!is_scalar(strflg)) 
          error("strflg must be a scalar");
        endif
        switch(strflg)
        case(0),
          stname = stname(signum);
        case(1),
          if(length(signum) > 1)
            error("strflg=1, length(signum) = %d",length(signum));
          endif
          stname = nth(stname,signum);
        otherwise,
          error("Illegal value of strflg=%e",strflg);
        endswitch
        
      endif
    endif
  endif

endfunction

