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
 
function ioname = sysdefioname(n,str,m)
# function ioname = sysdefioname(n,str[,m])
# return default input or output names given n, str, m
# n is the final value, str is the string prefix, and m is start value
# ex: ioname = sysdefioname(5,"u",3)
#
# returns: 	ioname = 	u_3
#				u_4
#				u_5
# used internally, minimal argument checking

# $Log: sysdefioname.m,v $

  save_val = implicit_str_to_num_ok;	# save for later
  implicit_str_to_num_ok = 1;

  if (nargin < 2 | nargin > 3)
    usage("ioname = sysdefioname(n,str[,m])");
  endif

  if (nargin == 2)
    m = 1;
  endif

  jj = 1;

  if(n > 0 & m > 0 & m <= n)
    for ii = m:n
      strval = [str,"_",num2str(ii)];
      ioname(jj,1:length(strval)) = strval;
      jj = jj+1;
    endfor
  elseif(n == 0)
    ioname = "";
  elseif(m > n)
    error(["start value m=",num2str(m)," > final value n=",num2str(n),"; bad!"])
  endif

  if( !isstr(ioname) )
    ioname = setstr(ioname);
  endif

  implicit_str_to_num_ok = save_val;	# restore value
 
endfunction
