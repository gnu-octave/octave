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
# Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

function l = lcm (a, ...)

# usage: lcm (a, ...)
#
# lcm (a) returns the least common multiple of the entries of the
# integer vector a.
# lcm (a1, ..., ak) is the same as lcm([a1, ..., ak]).
  
# Written by KH (Kurt.Hornik@ci.tuwien.ac.at) on Sep 16, 1994.

  if (nargin > 1)
    va_start;
    for k = 2:nargin;
      a = [a, va_arg ()];
    endfor
  endif
  
  if (round (a) != a)
    error ("lcm:  all arguments must be integer");
  endif
  
  if (any (a) == 0)
    l = 0;
  else
    a = abs (a);
    l = a (1);
    for k = 1:(length (a) - 1)
      l = l * a(k+1) / gcd (l, a(k+1));
    endfor
  endif
    
endfunction
    