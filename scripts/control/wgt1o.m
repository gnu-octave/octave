# Copyright (C) 1998 Kai P. Mueller
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
 
function wsys = wgt1o(vl, vh, fc)
# wgt10  State space description of a first order weighting function.
#
#     wsys = wgt1o(vl, vh, fc)
#
# Weighting function are needed by the H2/H_infinity design procedure.
# These function are part of thye augmented plant P (see hinfdemo
# for an applicattion example).
#
# vl = Gain @ low frequencies
# vh = Gain @ high frequencies
# fc = Corner frequency (in Hz, *not* in rad/sec)

# Written by Kai P. Mueller September 30, 1997

  if (nargin != 3)
    usage("wsys = wgt1o(vl, vh, fc)");
  endif

  if(nargout > 1)
    usage("wsys = wgt1o(vl, vh, fc)");
  endif

  if (vl == vh)
      a = [];
      b = [];
      c = [];
  else
      a = [-2*pi*fc];
      b = [-2*pi*fc];
      c = [vh-vl];
  endif
  d=[vh];

  wsys = ss2sys(a,b,c,d);
endfunction
