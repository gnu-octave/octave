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

function m = gram(a,b)
  # m = gram(a,b)
  # Return controllability grammian of continuous time system
  #
  #  dx/dt = a x + b u
  #
  # a m + a' + b*b' = 0 

  # Written by A. S. Hodel 
  # $Revision: 2.0.0.0 $

  # let lyap do the error checking...
  m = lyap(a,b*b');
endfunction
