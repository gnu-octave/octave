# Copyright (C) 1998 A. Scottedward Hodel 
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
 
function retval = strappend(str,suffix);
  # retval = strappend(str,suffix);
  # append string suffix to each string in the string matrix str
  
  if(nargin != 2 | nargout > 1)
    usage(" retval = strappend(str,suffix)");
  elseif(!isstr(str) | !isstr(suffix))
    error("Both arguments must be strings")
  endif

  for ii=1:rows(str)
    newst = [dezero(str(ii,:)),suffix];
    retval(ii,1:length(newst)) = (newst);
  endfor

endfunction
