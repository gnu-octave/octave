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
  
  if ((nargin < 2) || (nargin > 3))
    error("incorrect number of input arguments");
  endif

  if (nargin == 2)
    sys = a;
    w = b;
    if(!is_struct(sys))
      error("two arguments: 1st must be a system data structure");
    endif

    if (!is_vector(w))
      error("w must be a vector");
    endif
    
    [nn,nz,mm,pp] = sysdimensions(sys);
    if(mm != 1)       error("sys has %d > 1 inputs",mm); endif

    [a,b] = sys2ss(sys);

  else  

    if (columns(a) != rows(b)),
      error("ltifr:  A(%dx%d), B(%dx%d) not compatibly dimensioned", ...
	rows(a), columns(a), rows(b), columns(b));
    endif

    if(columns(b) != 1)
      error("ltifr: b(%dx%d) must be a single column vector", ...
	rows(b),columns(b));
    endif
  
    if (!is_square(a))
      error("ltifr:  A(%dx$d) must be square.",rows(a),columns(a))
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
