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
# Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

function [y,my,ny] = abcddims (x)

# Usage: [y,my,ny] = abcddims (x)
#
# Used internally in abcddim.  If x is a zero-size matrix, both dimensions
# get set to 0.  my and ny are the row and column dimensions of the result.

# Written by A. S. Hodel (scotte@eng.auburn.edu) Feb 1997
# $Revision: 2.0.0.0 $

  y = x;
  if(isempty(y))
    y = [];
  endif
  [my,ny] = size(y);
endfunction
