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
 
function [stname,inname,outname,yd] = sysgetsignals(sys)
  # function [stname,inname,outname,yd] = sysgetsignals(sys)
  # Get signal names from a system
  # inputs:
  #    sys: system data structure for the state space system
  #
  # outputs:
  #    stname, inname, outname: signal names (strings);  names of states,
  #          inputs, and outputs, respectively
  #    yd: binary vector; yd(ii) is nonzero if output y is discrete.
  # 

  # Adapted from ss2sys

  if(nargin != 1 | nargout > 4)
    usage("[stname,inname,outname,yd] = sysgetsignals(sys)")
  elseif( ! is_struct(sys) )
    error("input argument must be a system data structure");
  endif
  sys = sysupdate(sys,"ss");		#make sure ss is up to date
  yd = sys.yd;
  stname = sys.stname;
  inname = sys.inname;
  outname = sys.outname;

endfunction

