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

function retval = rem (x, y)

# usage: rem (x, y)
#
# Return remainder (x, y).

  if (nargin != 2)
    usage ("rem (x, y)");
  endif

  if (any (size (x) != size (y)) && ! (is_scalar (x) || is_scalar (y)))
    error ("rem: argument sizes must agree");
  endif

# Matlab allows complex arguments, but as far as I can tell, that's a
# bunch of hooey.

  if (any (any (imag (x))) || any (any (imag (y))))
    error ("rem: complex arguments are not allowed");
  endif

  if (nargin == 2)
    retval = x - y .* fix (x ./ y);
  else
    error ("usage: rem (x, y)");
  endif

endfunction
