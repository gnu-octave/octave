# Copyright (C) 1996,1998 Auburn University.  All Rights Reserved
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
# Software Foundation, 59 Temple Place, Suite 330, Boston, MA 02111 USA. 
 
function [num,den] = zp2tf(zer,pol,k)
# [num,den] = zp2tf(zer,pol,k)
# Converts zeros / poles to a transfer function.
#
# Inputs:
#   zer, pol: vectors of (possibly complex) poles and zeros of a transfer
#             function.  Complex values should appear in conjugate pairs
#   k: real scalar (leading coefficient)
# Forms the transfer function num/den from
# the vectors of poles and zeros.  K is a scalar gain associated with the
# zeros.

# Find out whether data was entered as a row or a column vector and
# convert to a column vector if necessary
# Written by A. S. Hodel with help from students Ingram, McGowan.
# a.s.hodel@eng.auburn.edu
#

  [rp,cp] = size(pol);
  [rz,cz] = size(zer);

  if(!(is_vector(zer) | isempty(zer)) )
    error(sprintf("zer(%dx%d) must be a vector",rz,cz));
  elseif(!(is_vector(pol) | isempty(pol)) )
    error(sprintf("pol(%dx%d) must be a vector",rp,cp));
  elseif(length(zer) > length(pol))
    error(sprintf("zer(%dx%d) longer than pol(%dx%d)",rz,cz,rp,cp));
  endif

  num = k;  den = 1;		# initialize converted polynomials

  # call zp2ssg2 if there are complex conjugate pairs left, otherwise
  # construct real zeros one by one.  Repeat for poles.
  while(!isempty(zer))
    if( max(abs(imag(zer))) )     [poly,zer] = zp2ssg2(zer);
    else                          poly = [1 -zer(1)];  
                                  zer = zer(2:length(zer));      endif
    num = conv(num,poly);
  endwhile

  while(!isempty(pol))
    if( max(abs(imag(pol))) )     [poly,pol] = zp2ssg2(pol);
    else                          poly = [1 -pol(1)];  
                                  pol = pol(2:length(pol));      endif
    den = conv(den,poly);
  endwhile

endfunction
