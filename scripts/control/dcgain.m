# Copyright (C) 1993, 1994, 1995 John W. Eaton
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

function gm = dcgain(sys, tol)
# Usage: gm = dcgain(sys[, tol])
#      Returns dc-gain matrix. If dc-gain is infinity
#      an empty matrix is returned.
#      The argument tol is an optional tolerance for the condition
#      number of A-Matrix in sys (default tol = 1.0e-10)
#
# See also: (nothing)

# Written by Kai P Mueller (mueller@ifr.ing.tu-bs.de) October 1, 1997
# Updated
# $Revision: 1.1.1.1 $
# $Log: dcgain.m,v $
# Revision 1.1.1.1  1998/05/19 20:24:06  jwe
#
# Revision 1.3  1997/12/01 16:51:50  scotte
# updated by Mueller 27 Nov 97
#
# Revision 1.1  1997/11/11  17:32:46  mueller
# Initial revision
#

  if((nargin < 1) || (nargin > 2) || (nargout > 1))
    usage("[gm, ok] = dcgain(sys[, tol])");
  endif
  if(!is_struct(sys))
    error("dcgain: first argument is not a system data structure.")
  endif
  sys = sysupdate(sys, "ss");
  aa = sys.a;
  if (is_digital(sys))  aa = aa - eye(size(aa));  endif
  if (nargin == 1)  tol = 1.0e-10;  endif
  r = rank(aa, tol);
  if (r < rows(aa))
    gm = [];
  else
    gm = -sys.c / aa * sys.b + sys.d;
  endif
endfunction
