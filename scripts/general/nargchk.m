# Copyright (C) 1994 John W. Eaton
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

function retval = nargchk (nargin_min, nargin_max, n)

# usage: nargchk (nargin_min, nargin_max, n)
#
# If N is in the range NARGIN_MIN to NARGIN_MAX, return the empty
# matrix.  Otherwise, return a message indicating whether N is too
# large or too small.

  if (nargin != 3)
    usage ("nargchk (nargin_min, nargin_max, n)");
  endif

  if (nargin_min > nargin_max)
    error  ("nargchk: nargin_min > nargin_max");
  endif

  if (n < nargin_min)
    retval = "nargchk: N is less than nargin_min";
  elseif (n > nargin_max)
    retval = "nargchk: N is greater than nargin_max";
  else
    retval = [];
  endif

endfunction
