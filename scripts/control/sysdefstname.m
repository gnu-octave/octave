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
 
function stname = sysdefstname(n,nz)
# function stname = sysdefstname(n,nz)
# return default state names given n, nz
# used internally, minimal argument checking

# $Revision: 1.2 $

  sav_val = implicit_str_to_num_ok;
  implicit_str_to_num_ok = 1;

  stname = [];
  if(n > 0)
    for ii = 1:n
      strval = ["x_",num2str(ii)];
      stname(ii,1:length(strval)) = strval;
    endfor
  endif
 
  # Set default names for discrete states
  if(nz > 0)
    for ii = (n+1):(n+nz)
      strval = ["xd_",num2str(ii)];
      stname(ii,1:length(strval)) = strval;
    endfor
  endif

  if( !(isstr(stname) | (rows(stname) == 0) ) )
    stname = setstr(stname);
  endif

  implicit_str_to_num_ok = sav_val;
endfunction
