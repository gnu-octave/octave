# Copyright (C) 1995 John W. Eaton
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

function r = roots (v)

# usage: roots (v)
#
# For a vector v with n components, return the roots of the
# polynomial v(1) * z^(n-1) + ... + v(n-1) * z + v(n).
  
# Written by KH (Kurt.Hornik@neuro.tuwien.ac.at) on Dec 24, 1993.

  [nr, nc] = size (v);
  if (nr <= 1 && nc <= 1)
    r = [];
    return;
  elseif (! ((nr == 1 && nc > 1) || (nc == 1 && nr > 1)))
    usage ("roots (v), where v is a nonzero vector");
  endif

  n = nr + nc - 1;
  v = reshape (v, 1, n);

# If v = [ 0 ... 0 v(k+1) ... v(k+l) 0 ... 0 ], we can remove the
# leading k zeros and n - k - l roots of the polynomial are zero.  

  f = find (v);
  m = max (size (f));
  if (m > 0)
    v = v (f(1):f(m));
    l = max (size (v));
    if (l > 1)
      A = diag (ones (1, l-2), -1);
      A (1, :) = -v (2:l) ./ v (1);
      r = eig (A);    
      if (f (m) < n)
	r = [r; zeros(n - f(m), 1)];
      endif
    else
      r = zeros (n - f(m), 1);
    endif
  else
    usage ("roots (v), where v is a nonzero vector");
  endif
  
endfunction
