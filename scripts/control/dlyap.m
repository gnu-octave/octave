# Copyright (C) 1993, 1994, 1995 Auburn University.  All Rights Reserved.
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
# Software Foundation, 59 Temple Place, Suite 330, Boston, MA 02111 USA.

function x = dlyap (a, b)

# Usage: x = dlyap (a, b)
#
# Solve a x a' - x + b = 0 (discrete Lyapunov equation) for square
# matrices a and b.  If b is not square, then the function attempts 
# to solve either
#
#  a x a' - x + b b' = 0
#
# or
#
#  a' x a - x + b' b = 0
#
# whichever is appropriate.  Uses Schur decomposition as in Kitagawa
# (1977).

# Written by A. S. Hodel (scotte@eng.auburn.edu) August 1993.

  if ((n = is_square (a)) == 0)
    warning ("dlyap: a must be square");
  endif

  if ((m = is_square (b)) == 0)
    [n1, m] = size (b);
    if (n1 == n)
      b = b*b';
      m = n1;
    else
      b = b'*b;
      a = a';
    endif
  endif

  if (n != m)
    warning ("dlyap: a,b not conformably dimensioned");
  endif

  # Solve the equation column by column.

  [u, s] = schur (a);
  b = u'*b*u;

  j = n;
  while (j > 0)
    j1 = j;

# Check for Schur block.

    if (j == 1)
      blksiz = 1;
    elseif (s (j, j-1) != 0)
      blksiz = 2;
      j = j - 1;
    else
      blksiz = 1;
    endif

    Ajj = kron (s (j:j1, j:j1), s) - eye (blksiz*n);

    rhs = reshape (b (:, j:j1), blksiz*n, 1);

    if (j1 < n)
      rhs2 = s*(x (:, (j1+1):n) * s (j:j1, (j1+1):n)');
      rhs = rhs + reshape (rhs2, blksiz*n, 1);
    endif

    v = - Ajj\rhs;
    x (:, j) = v (1:n);

    if(blksiz == 2)
      x (:, j1) = v ((n+1):blksiz*n);
    endif

    j = j - 1;

  endwhile

# Back-transform to original coordinates.

  x = u*x*u';

endfunction
