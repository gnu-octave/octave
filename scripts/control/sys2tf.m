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
 
function [num,den,tsam,inname,outname] = sys2tf(Asys)
# function [num,den,tsam,inname,outname] = sys2tf(Asys)
# Conversion from a system data structure format to a transfer function.  The 
# transfer function part of ASYS is returned to the user in the form:
#
#                num(s)
#          G(s)=-------
#                den(s)
#
# The user can also have the sampling time (TSAM), the name of the input 
# (INNAME), and the output name (OUTNAME)

# Written by R. Bruce Tenison (June 24, 1994) btenison@eng.auburn.edu
# modified to make sys2tf by A. S. Hodel Aug 1995
# modified again for updated system format by John Ingram July 1996

  if(nargin != 1)
    usage("[num,den,tsam,inname,outname] = sys2tf(Asys)");
  endif

  if( !is_struct(Asys))
    error("Asys must be a system data structure (see ss2sys, tf2sys, zp2sys)");
  elseif (! is_siso(Asys) )
    [n, nz, m, p] = sysdimensions(Asys);
    error(["system is not SISO (",num2str(m)," inputs, ...
        ", num2str(p)," outputs"]);
  endif

  Asys = sysupdate(Asys,"tf");		# just in case

  num = Asys.num;
  den = Asys.den;
  
  tsam = sysgettsam(Asys);
  inname = sysgetsignals(Asys,"in");
  outname = sysgetsignals(Asys,"out");

endfunction

