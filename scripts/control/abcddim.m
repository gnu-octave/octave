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

function [n, m, p] = abcddim (a, b, c, d)

# Usage: [n, m, p] = abcddim (a, b, c, d)
#
# Check for compatibility of the dimensions of the matrices defining
# the linear system (a, b, c, d).
#
# Returns n = number of system states,
#         m = number of system inputs,
#         p = number of system outputs.
#
# Returns n = m = p = -1 if the system is not compatible.

# Written by A. S. Hodel (scotte@eng.auburn.edu) August 1993.

  if (nargin != 4)
    error ("abcddim: invalid number of arguments.  need four.")
  endif

  n = m = p = -1;

  [an, am] = size(a);
  if (an != am)
    error ("abcddim: a is not square");
  endif

  [bn, bm] = size(b);
  if (bn != am)
    error ("abcddim: a and b are not compatible");
  endif

  [cn, cm] = size(c);
  if (cm != an)
    error ("abcddim: a and c are not compatible");
  endif

  [dn, dm] = size(d);
  if (cn != dn)
    error ("abcddim: c and d are not compatible");
  endif

  if (bm != dm)
    error ("abcddim: b and d are not compatible");
  endif

  n = an;
  m = bm;
  p = cn;

endfunction
