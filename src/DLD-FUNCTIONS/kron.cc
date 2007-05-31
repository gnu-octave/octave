/*

Copyright (C) 2002 John W. Eaton

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

#include "dMatrix.h"
#include "CMatrix.h"
#include "quit.h"

#include "defun-dld.h"
#include "error.h"
#include "oct-obj.h"

#if !defined (CXX_NEW_FRIEND_TEMPLATE_DECL)
extern void
kron (const Array2<double>&, const Array2<double>&, Array2<double>&);

extern void
kron (const Array2<Complex>&, const Array2<Complex>&, Array2<Complex>&);
#endif

template <class T>
void
kron (const Array2<T>& A, const Array2<T>& B, Array2<T>& C)
{
  C.resize (A.rows () * B.rows (), A.columns () * B.columns ());

  octave_idx_type Ac, Ar, Cc, Cr;

  for (Ac = Cc = 0; Ac < A.columns (); Ac++, Cc += B.columns ())
    for (Ar = Cr = 0; Ar < A.rows (); Ar++, Cr += B.rows ())
      {
	const T v = A (Ar, Ac);
	for (octave_idx_type Bc = 0; Bc < B.columns (); Bc++)
	  for (octave_idx_type Br = 0; Br < B.rows (); Br++)
	    {
	      OCTAVE_QUIT;
	      C.xelem (Cr+Br, Cc+Bc) = v * B.elem (Br, Bc);
	    }
      }
}

template void
kron (const Array2<double>&, const Array2<double>&, Array2<double>&);

template void
kron (const Array2<Complex>&, const Array2<Complex>&, Array2<Complex>&);

DEFUN_DLD (kron, args,  nargout, "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} kron (@var{a}, @var{b})\n\
Form the kronecker product of two matrices, defined block by block as\n\
\n\
@example\n\
x = [a(i, j) b]\n\
@end example\n\
\n\
For example,\n\
\n\
@example\n\
@group\n\
kron (1:4, ones (3, 1))\n\
      @result{}  1  2  3  4\n\
          1  2  3  4\n\
          1  2  3  4\n\
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
      ComplexMatrix a (args(0).complex_matrix_value());
      ComplexMatrix b (args(1).complex_matrix_value());

      if (! error_state)
	{
	  ComplexMatrix c;
	  kron (a, b, c);
	  retval(0) = c;
	}
    }
  else
    {
      Matrix a (args(0).matrix_value ());
      Matrix b (args(1).matrix_value ());

      if (! error_state)
	{
	  Matrix c;
	  kron (a, b, c);
	  retval (0) = c;
	}
    }

  return retval;
}
