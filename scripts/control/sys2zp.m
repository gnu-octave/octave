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
 
function [zer,pol,k,tsam,inname,outname] = sys2zp(sys)
# [zer,pol,k,tsam,inname,outname] = sys2zp(sys)
# extract zero/pole/leading coefficient information from a system data
# structure
# inputs: sys: system data structure
# outputs:
#   zer: vector of system zeros
#   pol: vector of system poles
#   k: scalar leading coefficient
#   tsam: sampling period. default: 0 (continuous system)
#   inname, outname: input/output signal names (strings)

# Created by John Ingram July 15 1996

  if(nargin != 1)
    usage("[zer,pol,k,tsam,inname,outname] = sys2zp(sys)");
  elseif( !is_struct(sys))
    error("sysconnect: sys must be in system data structure form")
  elseif (! is_siso(sys) )
    [n, nz, m, p] = sysdimensions(sys);
    error(["system is not SISO (",num2str(m)," inputs, ...
	", num2str(p)," outputs"]);
  endif

  # update zero-pole form
  sys = sysupdate(sys,"zp");

  zer = sys.zer;
  pol = sys.pol;
  k = sys.k;
  tsam    = sysgettsam(sys);
  inname  = sysgetsignals(sys,"in");
  outname = sysgetsignals(sys,"out");

endfunction


