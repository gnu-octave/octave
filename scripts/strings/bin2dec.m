# Copyright (C) 1996 Kurt Hornik
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

function y = bin2dec (x)

# usage:  bin2dec (x)
#
# Returns the decimal number corresponding to the binary number in
# quotes.  For example, bin2dec ("1110") returns 14.
  
# Original version by Kurt Hornik <Kurt.Hornik@ci.tuwien.ac.at>.
  
  if (nargin != 1)
    usage ("bin2dec (x)");
  endif

  x = toascii (x) - toascii ("0");

  if (all (x == 0 | x == 1))
    y = sum ((x .* (ones (rows (x), 1) * 2.^((length (x) - 1) : -1 : 0)))')';
  else
    error ("bin2dec: argument must be a string of zeros and ones");
  endif
  
endfunction
