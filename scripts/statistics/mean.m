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

function retval = mean (a)

# usage: mean (a)
#
# For vector arguments, return the mean the values.
#
# For matrix arguments, return a row vector containing the mean for
# each column.
#
# See also: median, std

  if (nargin != 1)
    usage ("mean (a)");
  endif

  [nr, nc] = size (a);
  if (nr == 1 || nc == 1)
    retval = sum (a) / length (a);
  elseif (nr > 0 && nc > 0)
    retval = sum (a) / nr;
  else
    error ("mean: invalid matrix argument");
  endif

endfunction
