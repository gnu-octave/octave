### Copyright (C) 1996 John W. Eaton
###
### This file is part of Octave.
###
### Octave is free software; you can redistribute it and/or modify it
### under the terms of the GNU General Public License as published by
### the Free Software Foundation; either version 2, or (at your option)
### any later version.
###
### Octave is distributed in the hope that it will be useful, but
### WITHOUT ANY WARRANTY; without even the implied warranty of
### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
### General Public License for more details.
###
### You should have received a copy of the GNU General Public License
### along with Octave; see the file COPYING.  If not, write to the Free
### Software Foundation, 59 Temple Place - Suite 330, Boston, MA
### 02111-1307, USA.

function retval = reshape (a, m, n)

  ## usage: reshape (a, m, n)
  ##
  ## Form an m x n matrix from the elements of a (taken in Fortran's
  ## column major ordering).
  ##
  ## See also: `:', do_fortran_indexing

  if (nargin != 3)
    usage ("reshape (a, m, n)");
  else
    [nr, nc] = size (a);
    if (nr * nc == m * n)
      dfi = do_fortran_indexing;
      istno = implicit_str_to_num_ok;
      unwind_protect
        do_fortran_indexing = 1;
	implicit_str_to_num_ok = 1;
        retval = zeros (m, n);
        retval (:) = a;
	if (isstr (a))
	  retval = setstr (retval);
	endif
      unwind_protect_cleanup
        do_fortran_indexing = dfi;
	implicit_str_to_num_ok = istno;
      end_unwind_protect
    else
      error ("reshape: sizes must match");
    endif
  endif

endfunction
