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
 
function [sig, tau] = zgrownorm(mat,meps)
# function [nonz, zer] = zgrownorm(mat,meps)
# used internally in tzero
# returns nonz = number of rows of mat whose two norm exceeds meps
#         zer = number of rows of mat whose two norm is less than meps


  rownorm = [];
  for ii=1:rows(mat)
    rownorm(ii) = norm(mat(ii,:));
  endfor
  sig = sum(rownorm > meps);
  tau = sum(rownorm <= meps);

endfunction

