# Copyright (C) 1998 Auburn University.  All Rights Reserved.
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
 
function systype = sysgettype(sys)
# systype = sysgetype(sys)
# return the initial system type of the system
# inputs:
#   sys: system data structure
# outputs:
#   systype: string indicating how the structure was initially 
#            constructed:
#      values: "ss", "zp", or "tf"

  if(!is_struct(sys))
    error("sysgettype: input sys is not a structure");
  endif

  typestr = list("tf","zp","ss");
  systype = nth(typestr,sys.sys(1) + 1);
endfunction
