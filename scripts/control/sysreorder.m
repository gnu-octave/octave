# Copyright (C) 1996 Auburn University.  All Rights Reserved
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
 
function pv = sysreorder(vlen,list)
# function pv = sysreorder(vlen,list)
#
# inputs: vlen: vector length
#         list: a subset of {1:vlen}
# pv: a permutation vector to order elements of [1:vlen] in -list-
#         to the end of a vector
# used internally by sysconnect to permute vector elements to their
# desired locations.  No user-serviceable parts inside; do not attempt
# to use this at home!

# A. S. Hodel, Aug 1995
  
  #disp('sysreorder: entry')
  
  pv = 1:vlen;
  # make it a row vector
  list = reshape(list,1,length(list));
  A = pv'*ones(size(list));
  B = ones(size(pv'))*list;
  X = (A != B);
  if(!is_vector(X))
    y = min(X');
  else
   y = X';
  endif
  z = find(y == 1);
  if(!isempty(z))
    pv = [z, list];
  else
    pv = list;
  endif
  
endfunction
