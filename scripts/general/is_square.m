# Copyright (C) 1993, 1994 John W. Eaton
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

function retval = is_square (x)

# usage: is_square (x)
#
# If x is square, then return value is the dimension of x.
# otherwise, returns a value of 0
#
# See also: size, rows, columns, length, is_matrix, is_scalar, is_vector

# Written by A. S. Hodel (scotte@eng.auburn.edu) August 1993.

  if (nargin == 1)
    [nr, nc] = size (x);
    if (nr == nc) 
      retval = nr;
    else
      retval = 0;
    endif
  else
    usage ("is_square (x)");
  endif

endfunction
