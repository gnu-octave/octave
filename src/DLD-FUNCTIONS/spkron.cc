/*

Copyright (C) 2002 John W. Eaton
Copyright (C) 2005 David Bateman

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

*/

// Author: Paul Kienzle <pkienzle@users.sf.net>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "dSparse.h"
#include "CSparse.h"
#include "quit.h"

#include "defun-dld.h"
#include "error.h"
#include "oct-obj.h"

#if !defined (CXX_NEW_FRIEND_TEMPLATE_DECL)
extern void
kron (const Sparse<double>&, const Sparse<double>&, Sparse<double>&);

extern void
kron (const Sparse<Complex>&, const Sparse<Complex>&, Sparse<Complex>&);
#endif

template <class T>
void
kron (const Sparse<T>& A, const Sparse<T>& B, Sparse<T>& C)
{
  octave_idx_type idx = 0;
  C = Sparse<T> (A.rows () * B.rows (), A.columns () * B.columns (), 
		 A.nzmax () * B.nzmax ());

  C.cidx (0) = 0;

  for (octave_idx_type Aj = 0; Aj < A.columns (); Aj++)
    for (octave_idx_type Bj = 0; Bj < B.columns (); Bj++)
      {
	for (octave_idx_type Ai = A.cidx (Aj); Ai < A.cidx (Aj+1); Ai++)
	  {
	    octave_idx_type Ci = A.ridx(Ai) * B.rows ();
	    const T v = A.data (Ai);

	    for (octave_idx_type Bi = B.cidx (Bj); Bi < B.cidx (Bj+1); Bi++)
	      {
		OCTAVE_QUIT;
		C.data (idx) = v * B.data (Bi);
		C.ridx (idx++) = Ci + B.ridx (Bi);
	      }
	  }
	C.cidx (Aj * B.columns () + Bj + 1) = idx;
      }
}

template void
kron (const Sparse<double>&, const Sparse<double>&, Sparse<double>&);

template void
kron (const Sparse<Complex>&, const Sparse<Complex>&, Sparse<Complex>&);

// PKG_ADD: dispatch ("kron", "spkron", "sparse matrix");
// PKG_ADD: dispatch ("kron", "spkron", "sparse complex matrix");
// PKG_ADD: dispatch ("kron", "spkron", "sparse bool matrix");
DEFUN_DLD (spkron, args,  nargout, "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} spkron (@var{a}, @var{b})\n\
Form the kronecker product of two sparse matrices. This is defined\n\
block by block as\n\
\n\
@example\n\
x = [a(i, j) b]\n\
@end example\n\
\n\
For example,\n\
\n\
@example\n\
@group\n\
kron(speye(3),spdiag([1,2,3]))\n\
@result{}\n\
Compressed Column Sparse (rows = 9, cols = 9, nnz = 9)\n\
\n\
  (1, 1) ->  1\n\
  (2, 2) ->  2\n\
  (3, 3) ->  3\n\
  (4, 4) ->  1\n\
  (5, 5) ->  2\n\
  (6, 6) ->  3\n\
  (7, 7) ->  1\n\
  (8, 8) ->  2\n\
  (9, 9) ->  3\n\
@end group\n\
@end example\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin != 2 || nargout > 1)
    {
      print_usage ();
    }
  else if (args(0).is_complex_type () || args(1).is_complex_type ())
    {
      SparseComplexMatrix a (args(0).sparse_complex_matrix_value());
      SparseComplexMatrix b (args(1).sparse_complex_matrix_value());

      if (! error_state)
	{
	  SparseComplexMatrix c;
	  kron (a, b, c);
	  retval(0) = c;
	}
    }
  else
    {
      SparseMatrix a (args(0).sparse_matrix_value ());
      SparseMatrix b (args(1).sparse_matrix_value ());

      if (! error_state)
	{
	  SparseMatrix c;
	  kron (a, b, c);
	  retval (0) = c;
	}
    }

  return retval;
}

/*

%!assert(spkron(spdiag([1,2,3]),spdiag([1,2,3])),sparse(kron(diag([1,2,3]),diag([1,2,3]))))
%!assert(spkron(spdiag([1i,2,3]),spdiag([1i,2,3])),sparse(kron(diag([1i,2,3]),diag([1i,2,3]))))

*/

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
