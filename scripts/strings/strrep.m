# Copyright (C) 1995, 1996  Kurt Hornik
# 
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
# 
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details. 
# 
# You should have received a copy of the GNU General Public License
# along with this file.  If not, write to the Free Software Foundation,
# 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

function t = strrep (s, x, y)
  
# usage:  strrep (s, x, y)
#
# Replace all occurences of the substring x of the string s with the
# string y.
  
# Written by Kurt Hornik <Kurt.Hornik@ci.tuwien.ac.at> on 1994/10/11
# Updated by Kurt Hornik <Kurt.Hornik@ci.tuwien.ac.at> on 1996/05/25  
  
  if (nargin <> 3)
    usage ("strrep (s, x, y)");
  endif
  
  if (! (isstr (s) && isstr (x) && isstr (y)))
    error ("strrep: all arguments must be strings");
  endif
  
  if (length (x) > length (s) || isempty (x))
    t = s;
    return;
  endif
  
  ind = findstr (s, x, 0);
  len = length (ind);
  if (len == 0)
    t = s;
  else
    s = toascii (s);
    x = toascii (x);
    y = toascii (y);

    l_x = length (x);
    tmp = s (1 : ind (1) - 1);
    t = [tmp, y];
    for k = 1 : len - 1
      tmp = s (ind (k) + l_x : ind (k+1) - 1);
      t = [t, tmp, y];
    endfor
    tmp = s (ind(len) + l_x : length (s));
    t = [t, tmp];
    t = setstr (t);
  endif
  
endfunction
