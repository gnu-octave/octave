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
 
function tfout(num,denom,x)
#
# usage: tfout(num,denom[,x])
#
# print formatted transfer function num(s)/d(s) 
# to the screen
# x defaults to the string "s"
#
#  SEE ALSO: polyval, polyvalm, poly, roots, conv, deconv, residue, 
#	filter, polyderiv, polyinteg, polyout

# Written by A. Scottedward Hodel (scotte@eng.auburn.edu) June 1995)
# $Revision: 2.0.0.0 $
  
  save_val = implicit_str_to_num_ok;
  save_empty = empty_list_elements_ok;
  empty_list_elements_ok = implicit_str_to_num_ok = 1;
  
  if (nargin < 2 ) | (nargin > 3) | (nargout != 0 ) 
    usage("tfout(num,denom[,x])");
  endif

  if ( (!is_vector(num)) | (!is_vector(denom)) )
    error("tfout: first two argument must be vectors");
  endif
  
  if (nargin == 2)
    x = 's';
  elseif( ! isstr(x) )
    error("tfout: third argument must be a string");
  endif

  numstring = polyout(num,x);
  denomstring = polyout(denom,x);
  len = max(length(numstring),length(denomstring));
  if(len > 0)
    y = strrep(blanks(len)," ","-");
    disp(numstring)
    disp(y)
    disp(denomstring)
  else
    error('tfout: empty transfer function')
  end

  implicit_str_to_num_ok = save_val;
  empty_list_elements_ok = save_empty;
endfunction
