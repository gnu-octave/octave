## Copyright (C) 1996, 1997 John W. Eaton
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, 59 Temple Place - Suite 330, Boston, MA
## 02111-1307, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} reshape (@var{a}, @var{m}, @var{n})
## Return a matrix with @var{m} rows and @var{n} columns whose elements are
## taken from the matrix @var{a}.  To decide how to order the elements,
## Octave pretends that the elements of a matrix are stored in column-major
## order (like Fortran arrays are stored).
##
## For example,
##
## @example
## @group
## reshape ([1, 2, 3, 4], 2, 2)
##      @result{}  1  3
##          2  4
## @end group
## @end example
##
## If the variable @code{do_fortran_indexing} is nonzero, the
## @code{reshape} function is equivalent to
##
## @example
## @group
## retval = zeros (m, n);
## retval (:) = a;
## @end group
## @end example
##
## @noindent
## but it is somewhat less cryptic to use @code{reshape} instead of the
## colon operator.  Note that the total number of elements in the original
## matrix must match the total number of elements in the new matrix.
## @end deftypefn
## @seealso{`:' and do_fortran_indexing}

## Author: jwe

function retval = reshape (a, m, n)

  if (nargin == 2 && prod (size (m)) == 2)
    n = m(2);
    m = m(1);
    nargin = 3;
  endif

  if (nargin == 3)
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
  else
    usage ("reshape (a, m, n) or reshape (a, size (b))");
  endif

endfunction
