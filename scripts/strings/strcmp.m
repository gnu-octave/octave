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

function status = strcmp (s1, s2)

# usage: strcmp (s1, s2)
#
# Compare two strings.
#
# WARNING:  Unlike the C function of the same name, this function
# returns 1 for equal and zero for not equal.  Why?  To be compatible
# with Matlab, of course. 

  if (nargin != 2)
    error ("usage: strcmp (s, t)");
  endif

  status = 0;
  if (isstr (s1) && isstr(s2))
    len_s1 = columns (s1);
    len_s2 = columns (s2);
    if (len_s1 == len_s2)
      if (len_s1 == 0)
        status = 1;
      else
        tmp = implicit_str_to_num_ok;
        implicit_str_to_num_ok = "true";
        status = all (s1 == s2);
        implicit_str_to_num_ok = tmp;
      endif
    endif
  endif

endfunction
