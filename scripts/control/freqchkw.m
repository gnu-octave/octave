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
 
function USEW = freqchkw(w)
  # function USEW = freqchkw(w)
  # used by freqresp to check that input frequency vector is legal

  # A S Hodel July 1996

  if(isempty(w))
    USEW = 0;
  elseif(!is_vector(w))
    error(["w (",num2str(rows(w)),"x",num2str(columns(w)), ...
      "): must be [], a vector or a scalar"]);
  elseif( (max(abs(imag(w))) != 0) && (min(real(w)) <= 0) )
    error("w must have real positive entries");
  else
    w = sort(w);
    USEW = 1;   # vector provided (check values later)
  endif
endfunction
