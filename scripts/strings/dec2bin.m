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

function y = dec2bin (x)

# usage:  dec2bin (x)
#
# Returns the binary number corresponding to the nonnegative integer
# x.  For example, dec2bin (14) returns "1110".

# Original version by Kurt Hornik <Kurt.Hornik@ci.tuwien.ac.at>.
  
  if (nargin != 1)
    usage ("dec2bin (x)");
  endif

  [nr, nc] = size (x);

  len = nr * nc;

  x = reshape (x, 1, len);

  eleo = empty_list_elements_ok;
  unwind_protect
    empty_list_elements_ok = 1;
    y = [];
    for i = 1:len
      tmp = x (i);
      if (tmp == round (tmp) && tmp >= 0)
	while (tmp >= 2)
	  z = fix (tmp ./ 2);
	  y = [y, tmp - 2 * z];
	  tmp = z;
	endwhile
	y = [y, tmp];
      else
	error ("dec2hex: invalid conversion");
      endif
    endfor
    y = fliplr (y);
    y = setstr (y + toascii ("0"));
  unwind_protect_cleanup
    empty_list_elements_ok = eleo;
  end_unwind_protect

endfunction
    
