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
 
function [y, t] = step(sys, inp, tstop, n)
# step: Step response for a linear system.
#       The system can be discrete or multivariable (or both).
#
# [y, t] = step(sys[, inp, tstop, n])
# Produces a plot or the step response data for system sys.
#
# The argument tstop (scalar value) denotes the time when the
# simulation should end. The Parameter n is the number of data values.
# Both parameters tstop and n can be ommitted and will be
# computed from the eigenvalues of the A-Matrix.
#
# When the step function is invoked with the output parameter y
# a plot is not displayed.
#
# See also: impulse, stepimp

# Written by Kai P. Mueller September 30, 1997
# based on lsim.m of Scottedward Hodel
# modified by

  if((nargin < 1) || (nargin > 4))
    usage("[y, u] = step(sys[, inp, tstop, n])");
  endif

  if(nargout > 2)
    usage("[y, u] = step(sys[, inp, tstop, n])");
  endif

  if(!is_struct(sys))
    error("step: sys must be a system data structure.");
  endif

  if (nargout == 0)
    switch (nargin)
      case (1)
        stepimp(1, sys);
      case (2)
        stepimp(1, sys, inp);
      case (3)
        stepimp(1, sys, inp, tstop);
      case (4)
        stepimp(1, sys, inp, tstop, n);
    endswitch
  else
    switch (nargin)
      case (1)
        [y, t] = stepimp(1, sys);
      case (2)
        [y, t] = stepimp(1, sys, inp);
      case (3)
        [y, t] = stepimp(1, sys, inp, tstop);
      case (4)
        [y, t] = stepimp(1, sys, inp, tstop, n);
    endswitch
  endif

endfunction
