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

function retval = median (a)

# usage: median (a)
#
# For vector arguments, return the median of the values.
#
# For matrix arguments, return a row vector containing the median for
# each column.
#
# See also: std, mean

  if (nargin != 1)
    usage ("medain (a)");
  endif

  [nr, nc] = size (a);
  s = sort (a);
  if (nr == 1 && nc > 0)
    if (rem (nc, 2) == 0)
      i = nc/2;
      retval = (s (i) + s (i+1)) / 2;
    else
      i = ceil (nc/2);
      retval = s (i);
    endif
  elseif (nr > 0 && nc > 0)
    if (rem (nr, 2) == 0)
      i = nr/2;
      retval = (s (i,:) + s (i+1,:)) / 2;
    else
      i = ceil (nr/2);
      retval = s (i,:);
    endif
  else
    error ("median: invalid matrix argument");
  endif

endfunction
