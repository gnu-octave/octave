# Copyright (C) 1993 John W. Eaton
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

function retval = std (a)

# usage: std (a)
#
# For vector arguments, std returns the standard deviation of the
# values.  For matrix arguments, std returns a row vector containing
# the standard deviation for each column.
#
# See also: mean, median

  if (nargin != 1)
    error ("usage: std (a)");
  endif

  nr = rows (a);
  nc = columns (a);
  if (nc == 1 && nr == 1)
    retval = 0;
  elseif (nc == 1 || nr == 1)
    tmp = sum (a);
    n = length (a);
    retval = sqrt ((n * sumsq (a) - tmp .* tmp) / (n * (n - 1)));
  elseif (nr > 1 && nc > 0)
    tmp = sum (a);
    retval = sqrt ((nr * sumsq (a) - tmp .* tmp) / (nr * (nr - 1)));
  else
    error ("mean: invalid matrix argument");
  endif

endfunction
