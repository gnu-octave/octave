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

function retval = int2str (x)

# usage: int2str (x)
#
# Round x to the nearest integer and format as a string.
#
# See also: sprintf, num2str 

# XXX FIXME XXX -- this will fail for very large values.

  if (nargin == 1)
    if (rows (x) == 1 && columns (x) == 1)
      retval = sprintf ("%d", round (x));
    else
      error ("int2str: expecting scalar argument");
    endif
  else
    error ("usage: int2str (x)");
  endif

endfunction
