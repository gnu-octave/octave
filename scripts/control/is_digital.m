# Copyright (C) 1996, 1999 A. Scottedward Hodel 
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
 
function DIGITAL = is_digital(sys,eflg)
# function DIGITAL = is_digital(sys{,eflg})
# return nonzero if system is digital
# inputs:
#   sys: system data structure
#   eflg: 0 [default] exit with an error if system is mixed (continuous and
#           discrete components)
#       : 1 print a warning if system is mixed (continuous and discrete)
#       : 2 silent operation
# outputs:
#   DIGITAL:  0: system is purely continuous
#          :  1: system is purely discrete
#          : -1: system is mixed continuous and discrete
# exits with an error of sys is a mixed (continuous and discrete) system

# a s hodel July 1996

  switch(nargin)
  case(1),  eflg = 0;
  case(2),  
    if( isempty(find(eflg == [0 1 2])) )
      error("Illegal value of eflg=%d (%e)",eflg,eflg);
    endif
  otherwise,
    usage("DIGITAL = is_digital(sys{,eflg})");
  endswitch

  # checked for sampled data system (mixed)
  # discrete system
  sysyd = sysgetsignals(sys,"yd");
  [nn,nz] = sysdimensions(sys);
  cont = sum(sysyd == 0) + nn;
  tsam = sysgettsam(sys);
  dig = sum(sysyd != 0) + nz + tsam;

  # check for mixed system
  if( cont*dig != 0)
   switch(eflg)
   case(0),
     error("continuous/discrete system; use syscont, sysdisc, or c2d first");
   case(1),
     warning("is_digital: mixed continuous/discrete system");
   endswitch
   dig_sign = -1;
  else
   dig_sign = 1;
  endif

  DIGITAL = dig_sign*(tsam > 0);
 
endfunction
