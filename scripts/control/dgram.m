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
 
function m = dgram(a,b)
  # m = dgram(a,b)
  # Return controllability grammian of discrete time system
  #
  #  x(k+1) = a x(k) + b u(k)
  #
  # a m a' - m + b*b' = 0 

  # Written by A. S. Hodel July 1995

  # let dlyap do the error checking...
  m = dlyap(a,b*b');
endfunction
