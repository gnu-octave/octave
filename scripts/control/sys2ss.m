# Copyright (C) 1996, 1998 Auburn University.  All Rights Reserved.
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
 
function [a,b,c,d,tsam,n,nz,stname,inname,outname,yd] = sys2ss(sys)
  # function [a,b,c,d(,tsam,n,nz,stname,inname,outname,yd)] = sys2ss(sys)
  # Convert from system data structure to state space form
  # inputs:
  #    sys: system data structure for the state space system
  #
  #      x' = Ax + Bu
  #      y  = Cx + Du
  #
  #  or a similar discrete-time system.  
  #
  # outputs:
  #    a,b,c,d: state space matrices for sys
  #    tsam: sampling time of sys (0 if continuous)
  #    n, nz: number of continuous, discrete states (discrete states come
  #          last in state vector x)
  #    stname, inname, outname: signal names (strings);  names of states,
  #          inputs, and outputs, respectively
  #    yd: binary vector; yd(ii) is nonzero if output y is discrete.
  # 
  # A warning message is printed if the system is a mixed 
  # continuous/discrete system.

  # Written by David Clem August 19, 1994
  # Updates by John Ingram July 14, 1996

  if(nargin != 1)
    usage("[a,b,c,d,tsam,n,nz,stname,inname,outname,yd] = sys2ss(sys)")
  endif

  if (nargout > 11)
    warning(["sys2ss: ",num2str(nargout)," out arguments exceeds max=11"])
    usage("[a,b,c,d,tsam,n,nz,stname,inname,outname,yd] = sys2ss(sys)")
  endif

  if( ! is_struct(sys) )
    error("input argument must be a system data structure");
  endif

  sys = sysupdate(sys,"ss");        # make sure state space data is there
  [n,nz,m,p] = sysdimensions(sys);
  [stname,inname,outname,yd] = sysgetsignals(sys);
  tsam = sysgettsam(sys);

  cont = sum(yd == 0) + n;
  dig = sum(yd != 0) + nz + tsam;
  if(cont*dig)
    warning("sys2ss: input system is mixed continuous/discrete");
  endif

  a = sys.a;
  b = sys.b;
  c = sys.c;
  d = sys.d;

endfunction

