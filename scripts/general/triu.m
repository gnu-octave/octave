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
# Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

function retval = triu (x, k)

# usage: triu (x, k)
#
# Return the upper triangular part of x above the k-th diagonal.  If
# the second argument is omitted, k = 0 is assumed.
#
# See also: tril, diag

  if (nargin > 0)
    [nr, nc] = size (x);
    retval = zeros (nr, nc);
  endif

  if (nargin == 1)
    k = 0;
  elseif (nargin == 2)
    if ((k > 0 && k > nc) || (k < 0 && k < -nr))
      error ("triu: requested diagonal out of range");
    endif
  else
    usage ("triu (x [, k])");
  endif

  for j = max (1, k) : nc
    nr_limit = min (nr, j-k);
    retval (1:nr_limit, j) = x (1:nr_limit, j);
  endfor

endfunction
