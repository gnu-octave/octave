# Copyright (C) 1996,1998 Auburn University.  All Rights Reserved.
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
 
function ioname = sysdefioname(n,str,m)
# function ioname = sysdefioname(n,str[,m])
# return list of default input or output names given n, str, m
# n is the final value, str is the string prefix, and m is start value
# ex: ioname = sysdefioname(5,"u",3)
#
# returns: 	ioname =
#               (
#                 [1] = u_3
#                 [2] = u_4
#                 [3] = u_5
#               )
# used internally, minimal argument checking

  if (nargin < 2 | nargin > 3)
    usage("ioname = sysdefioname(n,str[,m])");
  endif

  if (nargin == 2)           m = min(1,n);            endif

  ioname = list();
  jj = 1;
  if(n > 0 & m > 0 & m <= n)
    for ii = m:n
      ioname(ii+1-m) = sprintf("%s_%d",str,ii);
    endfor
  elseif(m > n)
    error("str=%s; start value m=%d > final value n=%d",str,m,n);
  endif

endfunction
