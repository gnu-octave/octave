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

function [X, map] = gray2ind (I, n)

# Image format conversion

# Written by Tony Richardson (amr@mpl.ucsd.edu) July 1994.

  if (nargin < 1 || nargin > 2)
    usage ("gray2ind (I, n)");
  elseif (nargin == 1)
    n = 64;
  endif

  map = gray (n);

  X = round (I*(n-1)) + 1;

endfunction
