# Copyright (C) 1996,1998 A. Scottedward Hodel 
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
 
function [a,b] = zgsgiv(c,s,a,b)
  # [a,b] = zgsgiv(c,s,a,b)
  # apply givens rotation c,s to row vectors a,b
  # No longer used in zero-balancing (zgpbal); kept for backward compatibility
  
  # A. S. Hodel July 29, 1992
  # Convertion to Octave by R. Bruce Tenison July 3, 1994
  # $Revision: 2.0.0.0 $

  t1 = c*a + s*b;
  t2 = -s*a + c*b;
  a = t1;
  b = t2;
endfunction
