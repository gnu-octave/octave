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
 
function outsys = ord2(nfreq, damp, gain)
  # function outsys = ord2(nfreq, damp[, gain])
  # Creates a continuous 2nd order system with parameters:
  #
  #      nfreq:   natural frequency [Hz]. (not in rad/s)
  #      damp:    damping coefficient
  #      gain:    dc-gain
  #               This is steady state value only for damp > 0.
  #               gain is assumed to be 1.0 if ommitted.
  #
  #      The system has representation with w = 2 * pi * nfreq:
  #
  #          /                                        \
  #          | / -2w*damp -w \  / w \                 |
  #      G = | |             |, |   |, [ 0  gain ], 0 |
  #          | \   w       0 /  \ 0 /                 |
  #          \                                        /
  #
  # See also: jet707 (MIMO example, Boeing 707-321 aircraft model)

  # Written by Kai P. Mueller September 28, 1997
  # Updates
  # $Revision: 1.1.1.1 $
  # $Log: ord2.m,v $
  # Revision 1.1.1.1  1998/05/19 20:24:07  jwe
  #
  # Revision 1.3  1997/12/01 16:51:50  scotte
  # updated by Mueller 27 Nov 97
  #
# Revision 1.1  1997/11/11  17:34:06  mueller
# Initial revision
#

  if(nargin != 2 & nargin != 3)
    usage("outsys = ord2(nfreq, damp[, gain])")
  endif
  if (nargout > 1)
    usage("outsys = ord2(nfreq, damp[, gain])")
  endif
  if (nargin == 2)
    gain = 1.0;
  endif

  w = 2.0 * pi * nfreq;
  outsys = ss2sys([-2.0*w*damp, -w; w, 0], [w; 0], [0, gain]);
endfunction
