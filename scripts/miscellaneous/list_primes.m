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

function retval = list_primes (n)

# usage: list_primes (n)
#
# List the first n primes.  If n is unspecified, the first 30 primes
# are listed.
#
# The algorithm used is from page 218 of the TeXbook.

  if (nargin > 0)
    if (! is_scalar (n))
      error ("list_primes: argument must be a scalar");
    endif
  endif

  if (nargin == 0)
    n = 30;
  endif

  if (n == 1)
    retval = 2;
    return;
  endif

  if (n == 2)
    retval = [2; 3];
    return;
  endif

  retval = zeros (1, n);
  retval (1) = 2;
  retval (2) = 3;

  n = n - 2;
  i = 3;
  p = 5;
  while (n > 0)

    is_prime = 1;
    is_unknown = 1;
    d = 3;
    while (is_unknown)
      a = fix (p / d);
      if (a <= d)
        is_unknown = 0;
      endif
      if (a * d == p)
        is_prime = 0;
        is_unknown = 0;
      endif
      d = d + 2;
    endwhile

    if (is_prime)
      retval (i++) = p;
      n--;
    endif
    p = p + 2;

  endwhile

endfunction