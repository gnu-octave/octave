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
 
function DIGITAL = is_digital(sys)
# function DIGITAL = is_digital(sys)
# retrurn nonzero if system is digital
# exits with an error of sys is a mixed (continuous and discrete) system

# a s hodel July 1996
# $Revision: 2.0.0.0 $
# SYS_INTERNAL accesses members of system structure

  # checked for sampled data system (mixed)
  # discrete system
  sysyd = sysgetsignals(sys,"yd");
  [nn,nz] = sysdimensions(sys);
  cont = sum(sysyd == 0) + nn;
  tsam = sysgettsam(sys);
  dig = sum(sysyd != 0) + nz + tsam;
  if( cont*dig != 0)
   sysout(sys);
   error("continuous/discrete system; use syscont, sysdisc, or c2d first");
  else
    DIGITAL = (tsam > 0);
  endif
 
endfunction
