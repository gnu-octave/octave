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
 
function out = h2norm(sys)
  # Usage: out = h2norm(sys)
  #
  # Computes the H2 norm system data structure (continuous time only)
  # sys = system data structure [see ss2sys()]
  # returns out = Inf if system is unstable
  #
  # Reference:
  # Doyle, Glover, Khargonekar, Francis, "State Space Solutions to Standard
  # H2 and Hinf Control Problems", IEEE TAC August 1989
  #

  # A. S. Hodel Aug 1995
  # updated for system data structure by John Ingram November 1996
  # $Revision: 1.2 $

  if((nargin != 1))
    usage("out = h2norm(sys)");
  elseif(!is_struct(sys))
    error("Sys must be in system data structure");
  end
  dflg = is_digital(sys);

  if(!is_stable(sys))
    warning("h2norm: unstable input system; returning Inf");
    out = Inf;
  else
    # compute gain
    [a,b,c,d] = sys2ss(sys);
    if(dflg)
      M = dlyap(a,b*b');
    else
      M = lyap (a,b*b');
    endif
    if( min(real(eig(M))) < 0)
      error("h2norm: grammian not >= 0 (lightly damped modes?)")
    endif
    out = sqrt(d'*d + trace(c*M*c'));
  endif
endfunction
