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

function retval = triu (x, k)

# usage: triu (x, k)
#
# Return the upper triangular part of x above the k-th diagonal.  If
# the second argument is omitted, k = 0 is assumed.
#
# See also: tril, diag

  if (nargin > 0)
    [nr, nc] = size (x);
    retval = x;
  endif

  if (nargin == 1)
    k = 0;
  elseif (nargin == 2)
    max_nr_nc = max (nr, nc);
    if ((k > 0 && k > nc - 1) || (k < 0 && k < 1 - nr))
      error ("triu: requested diagonal out of range")
    endif
  else
    usage ("triu (x [, k])");
  endif

  for j = 1:nc
    for i = j+1-k:nr
      retval (i, j) = 0.0;
    endfor
  endfor

endfunction
