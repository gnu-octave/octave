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
 
function sysp = parallel(Asys,Bsys)
# function sysp = parallel(Asys,Bsys)
# Forms the parallel connection of two systems.
#
#              ____________________
#              |      ________    |
#     u  ----->|----> | Asys |--->|----> y1
#         |    |      --------    |
#         |    |      ________    |
#         |--->|----> | Bsys |--->|----> y2
#              |      --------    |
#              --------------------
#                   Ksys

# Written by David Clem August 15, 1994
# completely rewritten Oct 1996 a s hodel
# SYS_INTERNAL accesses members of system structure
# $Revision: 1.1.1.1 $

  if(nargin != 2)
    usage("sysp = parallel(Asys,Bsys)");
  endif
  if(! is_struct(Asys) )
    error("1st input argument is not a system data structure")
  elseif (! is_struct(Bsys) )
    error("2nd input argument is not a system data structure")
  endif
  mA = rows(Asys.inname);
  mB = rows(Bsys.inname);
  if(mA != mB)
    error(["Asys has ",num2str(mA)," inputs, Bsys has ",num2str(mB)," inputs"]);
  endif
  sysp = sysgroup(Asys,Bsys);
  sysD = ss2sys([],[],[],[eye(mA);eye(mA)]);
  
  #disp("sysp=")
  #sysout(sysp)
  #disp("sysD")
  #sysout(sysD)

  sysp = sysmult(sysp,sysD);
  sysp = syschnames(sysp,"in",1:mA,Asys.inname);
  
endfunction
