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
 
function [c,tsam,input,output] = sys2fir(sys)
# function [c,tsam,input,output] = sys2fir(sys)
# extract fir system from system data structure

# $Revision: 1.1.1.1 $
# a s hodel July 1996

  sys=sysupdate(sys,"tf");		# make sure it's SISO
  alph = sys.den(1);			# scale to get monic denominator
  sys.den = sys.den/alph;
  sys.num = sys.num/alph;
  l = length(sys.den);
  m = length(sys.num);
  if( norm(sys.den(2:l)) )
    sysout(sys,"tf");
    error("denominator has poles away from origin");
  elseif( !is_digital(sys) )
    error("system must be discrete-time to be FIR");
  elseif(m != l)
    warning(["sys2fir: deg(num) - deg(den) = ",num2str(m-l), ...
	"; coefficients must be shifted"]);
  endif
  c = sys.num;
  tsam = sys.tsam;
  input = sys.inname;
  output = sys.outname;
endfunction

