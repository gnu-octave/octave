# Copyright (C) 1996 John W. Eaton
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

function v = findstr (s, t)

# usage: findstr (s, t)
#
# Returns the vector of all positions in the longer of the two strings
# S and T where an occurence of the shorter of the two starts.
  
# Original version by Kurt Hornik <Kurt.Hornik@ci.tuwien.ac.at>.

  if (nargin != 2)
    usage ("findstr (s, t)");
  endif

  if (isstr (s) && isstr (t))

    # Make S be the longer string.

    if (length (s) < length (t))
      tmp = s;
      s = t;
      t = tmp;
    endif

    s = toascii (s);
    t = toascii (t);

    ind = 1 : length (t);
    limit = length (s) - length (t) + 1;
    v  = zeros (1, limit);
    i = 0;

    for k = 1 : limit
      if (s (ind + k - 1) == t)
	v (++i) = k;
      endif
    endfor

    if (i > 0)
      v = v (1:i);
    else
      v = [];
    endif

  else
    error ("findstr: both arguments must be strings");
  endif

endfunction
