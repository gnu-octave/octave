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

function retval = reshape (a, m, n)

# usage: reshape (a, m, n)
#
# Form an m x n matrix from the elements of a (taken in Fortran's
# column major ordering).
#
# See also: `:', do_fortran_indexing

  if (nargin != 3)
    error ("usage: reshape (a, m, n)");
  else
    [nr, nc] = size (a);
    if (nr * nc == m * n)
      tmp = do_fortran_indexing;
      do_fortran_indexing = "true";
      retval = zeros (m, n);
      retval (:) = a;
      do_fortran_indexing = tmp;
    else
      error ("reshape: sizes must match");
    endif
  endif

endfunction
