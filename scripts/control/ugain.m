# Copyright (C) 1997 Kai P. Mueller
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
 
function outsys = ugain(n)
  # function outsys = ugain(n)
  # Creates a system with unity gain, no states.
  # This trivial system is sometimes needed to create arbitrary
  # complex systems from simple systems with buildssic.
  # Watch out if you are forming sampled systems since "ugain"
  # does not contain a sampling period.  
  #
  # See also: hinfdemo (MIMO H_infinty example, Boeing 707-321 aircraft model)

  # Written by Kai P. Mueller April, 1998
  # Updates

  if((nargin != 1) || (nargout > 1))
    usage("outsys = ugain(n)")
  endif
  outsys = ss2sys([],[],[],eye(n));
endfunction
