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
 
function out = ltifr(a,b,w)
  #ltifr:  Linear time invarient frequency response of SISO systems
  # out = ltifr(A,B,w)
  # user enters the A and B matrices
  #
  # out = ltifr(sys,w)
  # user enters the system, SYS
  #
  # this function takes the system matrices, A and B and
  # returns:               -1
  #          G(s) = (jw I-A) B
  #
  # for complex frequencies s = jw. 

  # R. B. Tenison, D. Clem, A. S. Hodel, July 1995
  # updated by John Ingram August 1996 for system format
  # $Revision: 1.1.1.1 $
  
  if ((nargin < 2) || (nargin > 3))
    error("incorrect number of input arguments");
  endif

  if (nargin == 2)
    sys = a;
    w = b;

    if (!is_vector(w))
      error("w must be a vector");
    endif
    
    sys = sysupdate(sys,"ss");

    if(columns(sys.b) != 1)
      error("sys is not an SISO system");
    endif

    a = sys.a;
    b = sys.b;        

  else  

    if (columns(a) != rows(b)),
      error("ltifr:  A, B not compatibly dimensioned");
    endif

    if(columns(b) != 1)
      error("ltifr: 2nd argument must be a single column vector");
    endif
  
    if (!is_square(a))
      error("ltifr:  A must be square.")
    endif

  endif

  if (!is_vector(w))
    error("w must be a vector");
  endif

  ey = eye(size(a));
  lw = length(w);
  out = ones(columns(a),lw);

  for ii=1:lw,
    out(:,ii) = (w(ii)*ey-a)\b;
  endfor
endfunction
